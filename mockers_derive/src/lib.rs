//#![feature(proc_macro)]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
extern crate itertools;
// #[macro_use] extern crate lazy_static;
#[macro_use]
extern crate synom;

use std::result::Result;
// use std::sync::Mutex;
use std::collections::{HashSet, HashMap};
use proc_macro2::TokenStream;
use syn::{Item, ItemTrait, ItemForeignMod, ForeignItem, ForeignItemFn, Ident, Path, TraitItem, TypeParamBound, TraitBoundModifier,
          PathArguments, PathSegment, TraitItemType, Type, Generics, TypeParam, GenericParam,
          AngleBracketedGenericArguments, FnDecl, ImplItem, Visibility, GenericArgument, TypeReference,
          Expr, FnArg, ReturnType, Pat, TraitItemMethod, ArgCaptured, PatIdent, TypePtr, TypePath, TypeTuple,
          QSelf, TypeBareFn, ParenthesizedGenericArguments, TraitBound, BareFnArg, ImplItemType, ExprPath, Attribute};
use syn::punctuated::Punctuated;
use syn::buffer::TokenBuffer;
use std::str::FromStr;
use quote::ToTokens;
use itertools::Itertools;
use proc_macro2::Span;

/// Each mock struct generated with `#[derive(Mock)]` or `mock!` gets
/// unique type ID. It is added to both call matchers produced by
/// `*_call` methods and to `Call` structure created by mocked method.
/// It is same to use call matcher for inspecting call object only when
/// both mock type ID and method name match.
static mut NEXT_MOCK_TYPE_ID: usize = 0;

/* TODO: enable again
lazy_static! {
    static ref KNOWN_TRAITS: Mutex<HashMap<Path, ItemTrait>> = Mutex::new(HashMap::new());
}*/

struct MockAttrOptions {
    mock_name: Option<Ident>,
    module_path: Option<Path>,
    refs: HashMap<Path, Path>,
}

fn parse_options(attr_tokens: proc_macro::TokenStream) -> Result<MockAttrOptions, String> {
    use syn::{Meta, NestedMeta, MetaList, MetaNameValue, LitStr};

    let buffer = TokenBuffer::new(proc_macro::TokenStream::from_str(&format!("#[mocked({})]", attr_tokens)).unwrap());
    let (attr, _) = Attribute::parse_outer(buffer.begin()).expect("parsed");

    let mut mock_name: Option<Ident> = None;
    let mut module_path: Option<Path> = None;
    let mut refs: HashMap<Path, Path> = HashMap::new();

    match attr.interpret_meta().expect("unable to interpret attribute meta") {
        // Just plain `#[mocked]` without parameters.
        Meta::Word(..) => (),

        // `#[mocked(module="...", inherits(...))]`
        Meta::List(MetaList { ref nested, .. }) => {
            for item in nested {
                match *item {
                    NestedMeta::Meta(Meta::NameValue(MetaNameValue {ref ident, lit: syn::Lit::Str(ref refs_str), ..}))
                    if &ident.to_string() == "refs" => {
                        use syn::synom::{Synom, PResult};
                        use syn::buffer::Cursor;

                        fn parse_path(i: &str) -> synom::IResult<&'static str, Path> {
                            let buf = TokenBuffer::new2(TokenStream::from_str(i).unwrap());
                            
                            match Path::parse(buf.begin()) {
                                Ok((o, _)) => synom::IResult::Done("", o),
                                Err(_) => synom::IResult::Error,
                            }
                        }

                        named!(refs_parser -> Vec<(Path, Path)>,
                            terminated_list!(punct!(","), do_parse!(
                                source: parse_path >>
                                punct!("=>") >>
                                target: parse_path >>
                                ((source, target))
                            ))
                        );
                        let refs_list = unwrap("`refs` attr parameter", refs_parser, &refs_str.value())?;

                        for (source, target) in refs_list {
                            if source.global() {
                                return Err("global source path".to_string());
                            }
                            if !target.global() {
                                return Err("local target path".to_string());
                            }
                            refs.insert(source, target);
                        }
                    }

                    NestedMeta::Meta(Meta::NameValue(MetaNameValue {ref ident, lit: syn::Lit::Str(ref path_str), ..}))
                        if &ident.to_string() == "module" => {
                        use syn::synom::Synom;
                        if module_path.is_some() {
                            return Err("module attribute parameters is used more than once".to_string());
                        }
                        let path: Path = path_str.parse().map_err(|err| format!("{}", err))?;
                        if !path.global() {
                            return Err("module path must be global".to_string());
                        }
                        module_path = Some(path);
                    },

                    NestedMeta::Meta(Meta::Word(ref ident)) => {
                        mock_name = Some(ident.clone());
                    },

                    _ => return Err("unexpected attribute parameter".to_string()),
                }
            }
        },

        // #[mocked="..."], such form isn't used right now, but may be used for specifying
        // mock struct name.
        Meta::NameValue(_) => return Err(format!("unexpected name-value attribute param")),
    }

    Ok(MockAttrOptions { mock_name, module_path, refs })
}

#[proc_macro_attribute]
pub fn mocked(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_mock(attr, input)
}

// To be deprecated
#[proc_macro_attribute]
pub fn derive_mock(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let opts = match parse_options(attr) {
        Ok(opts) => opts,
        Err(err) => panic!("{}", err),
    };
    match mocked_impl(input.into(), &opts) {
        Ok(tokens) => tokens.into(),
        Err(err) => panic!("{}", err),
    }
}

fn mocked_impl(input: TokenStream, opts: &MockAttrOptions) -> Result<TokenStream, String> {
    use syn::synom::Synom;
    use syn::buffer::TokenBuffer;

    let mut source = input.to_string();
    let buffer = TokenBuffer::new(input.into());
    let (source_item, _) = Item::parse(buffer.begin()).map_err(|e| format!("{:?}", e))?;
    let (tokens, include_source) = generate_mock(&source_item, opts)?;

    if cfg!(feature="debug") {
        println!("{}", tokens.to_string());
    }

    if !include_source {
        source.clear();
    }
    source.push_str(&tokens.to_string());
    TokenStream::from_str(&source).map_err(|e| format!("{:?}", e))
}

struct TraitDesc {
    mod_path: Path,
    trait_item: Item,
}

fn generate_mock(item: &Item, opts: &MockAttrOptions) -> Result<(TokenStream, bool), String> {
    let (supertraits, ident) = match item {
        Item::Trait(ItemTrait { ref supertraits, ref ident, .. }) => (supertraits, ident),
        Item::ForeignMod(ref foreign_mod) => {
            let mock_name = opts.mock_name.as_ref().ok_or_else(||
                "mock type name must be set explicitly for extern block".to_string())?;
            return Ok((generate_extern_mock(foreign_mod, mock_name)?, false))
        },
        _ => return Err("Attribute may be used on traits and extern blocks only".to_string()),
    };
    let mock_ident = opts.mock_name.clone().unwrap_or_else(|| Ident::new(&format!("{}Mock", ident), Span::call_site()));

    // Find definitions for referenced traits.
    let referenced_items = supertraits.iter().map(|b| {
        let path = match *b {
            TypeParamBound::Lifetime(..) =>
                return Err("lifetime parameters not supported yet".to_string()),
            TypeParamBound::Trait(TraitBound { ref path, .. }) => path,
        };
        let full_path = if path.global() {
            path
        } else {
            match opts.refs.get(path) {
                Some(p) => p,
                None => return Err("parent trait path must be given using 'refs' param".to_string()),
            }
        };
        /*
        TODO: enable again 
        if let Some(referenced_trait) = KNOWN_TRAITS.lock().unwrap().get(full_path) {
            let mod_path = Path {
                global: path.global,
                segments: path.segments[..path.segments.len()-1].into(),
            };
            Ok(TraitDesc {
                mod_path: mod_path,
                trait_item: Item::Trait(referenced_trait.clone()),
            })
        } else {*/
            Err(format!("Can't resolve trait reference: {:?}", path))
        /*}*/
    }).collect::<Result<Vec<TraitDesc>, String>>()?;

    // Remember full trait definition, so we can recall it when it is references by
    // another trait.
    if let Some(ref module_path) = opts.module_path {
        let mut full_path = module_path.clone();
        full_path.segments.push(PathSegment::from(ident.clone()));
        /* TODO: enable again KNOWN_TRAITS.lock().unwrap().insert(full_path, item.clone()); */
    }

    let trait_desc = TraitDesc {
        mod_path: Path {
            leading_colon: None,
            segments: Punctuated::new(),
        },
        trait_item: item.clone(),
    };
    let mut all_traits = referenced_items;
    all_traits.push(trait_desc);

    Ok((generate_mock_for_traits(mock_ident, &all_traits, true)?, true))
}

/// Generate mock struct and all implementations for given `trait_items`.
/// `mock_ident` is identifier for mock struct.
/// If `local` is `true`, `Mocked` instance generated for mock, which
/// allows to use `scenario.create_mock_for::<Trait>`.
fn generate_mock_for_traits(mock_ident: Ident,
                            trait_items: &[TraitDesc],
                            local: bool)
                            -> Result<TokenStream, String> {
    let mock_ident_ref = &mock_ident;
    // Validate items, reject unsupported ones.
    let mut trait_paths = HashSet::<String>::new();
    let traits: Vec<(Path, &Vec<TraitItem>)> = trait_items.iter()
        .map(|desc| {
            match desc.trait_item {
                Item::Trait(ItemTrait { ref unsafety, ref generics, ref supertraits, ref items, ref ident, .. }) => {
                    if unsafety.is_some() {
                        return Err("Unsafe traits are not supported yet".to_string());
                    }

                    if !generics.params.is_empty() || generics.where_clause.is_some() {
                        return Err("Parametrized traits are not supported yet".to_string());
                    }

                    for bound in supertraits {
                        match *bound {
                            TypeParamBound::Trait(TraitBound { ref lifetimes, ref modifier, ref path, .. }) => {
                                match modifier {
                                    TraitBoundModifier::None => {
                                        assert!(lifetimes.is_none());

                                        // Ok, this is plain base trait reference with no lifetimes
                                        // and type bounds. Check whether base trait definition was
                                        // provided by user.
                                        if !trait_paths.contains(&format!("{:?}", path)) {
                                            return Err("All base trait definitions must be \
                                                        provided"
                                                .to_string());
                                        }
                                    }
                                    _ => {
                                        return Err("Type bound modifiers are not supported yet"
                                            .to_string())
                                    }
                                }
                            }
                            TypeParamBound::Lifetime(..) => {
                                return Err("Lifetime parameter bounds are not supported yet"
                                    .to_string())
                            }
                        }
                    }

                    let mut trait_path = desc.mod_path.clone();
                    trait_path.segments.push(PathSegment {
                        ident: ident.clone(),
                        arguments: PathArguments::None,
                    });

                    trait_paths.insert(format!("{:?}", trait_path));
                    Ok((trait_path, items))
                }
                _ => {
                    return Err("Only traits are accepted here".to_string());
                }
            }
        })
        .collect::<Result<Vec<(Path, &Vec<TraitItem>)>, String>>()?;

    // Gather associated types from all traits, because they are used in mock
    // struct definition.
    let mut assoc_types = Vec::new();
    for &(_, ref members) in &traits {
        for member in members.iter() {
            if let TraitItem::Type(TraitItemType { ref bounds, ref ident, .. }) = member {
                if !bounds.is_empty() {
                    return Err("associated type bounds are not supported yet".to_string());
                }
                assoc_types.push(ident.clone());
            }
        }
    }

    let struct_item = generate_mock_struct(&mock_ident, &assoc_types);

    // Generic parameters used for impls. It is part inside angles in
    // `impl<A: ::std::fmt::Debug, B: ::std::fmt::Debug, ...> ...`.
    let generics = {
        let mut gen = Generics::default();
        gen.params = assoc_types.iter()
            .cloned()
            .map(|param| {
                GenericParam::Type(TypeParam {
                    colon_token: Some(Token![:]([Span::call_site()])),
                    eq_token: None,
                    ident: param,
                    attrs: vec![],
                    bounds: parse_quote! { ::std::fmt::Debug },
                    default: None,
                })
            })
            .collect();
        gen
    };

    let mut struct_type_path_segments = Punctuated::new();

    struct_type_path_segments.push_value(PathSegment {
                                  ident: mock_ident.clone(),
                                  arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                      colon2_token: Some(Token![::]([Span::call_site(), Span::call_site()])),
                                      gt_token: Token![>]([Span::call_site()]),
                                      lt_token: Token![<]([Span::call_site()]),
                                      args: assoc_types.iter()
                                          .cloned()
                                          .map(|ident| GenericArgument::Type(Type::Path(TypePath { qself: None, path: Path::from(ident) })))
                                          .collect(),
                                  }),
                                });

    // Type of mock struct with all type parameters specified.
    let struct_type = Type::Path(TypePath {
        qself: None,
                               path: Path {
                                   leading_colon: None,
                                   segments: struct_type_path_segments,
                               }});

    let mut generated_items = vec![struct_item];

    for &(ref trait_path, ref members) in &traits {
        let mut impl_methods = Vec::new();
        let mut trait_impl_methods = Vec::new();

        for member in members.iter() {
            match member {
                TraitItem::Method(TraitItemMethod { ref sig, .. }) => {
                    if sig.unsafety.is_some() {
                        return Err("unsafe trait methods are not supported".to_string());
                    }
                    if sig.constness.is_some() {
                        return Err("const trait methods are not supported".to_string());
                    }
                    if sig.abi.is_some() {
                        return Err("non-Rust ABIs for trait methods are not supported".to_string());
                    }

                    let methods = generate_trait_methods(sig.ident.clone(),
                                                         &sig.decl,
                                                         &sig.decl.generics,
                                                         &trait_path)?;
                    impl_methods.push(methods.impl_method);
                    trait_impl_methods.push(methods.trait_impl_method);
                }
                TraitItem::Type(TraitItemType { ref bounds, .. }) => {
                    if !bounds.is_empty() {
                        return Err("associated type bounds are not supported yet".to_string());
                    }
                }
                TraitItem::Const(..) => {
                    return Err("trait constants are not supported yet".to_string());
                }
                TraitItem::Macro(..) => {
                    return Err("trait macros are not supported yet".to_string());
                },
                TraitItem::Verbatim(..) => {
                    return Err("syn does not parse this".to_string());
                }
            }
        }

        // `impl<...> AMock<...> { pub fn foo_call(...) { ... } }`
        let impl_item = quote!{
            impl #generics #struct_type {
                #(#impl_methods)*
            }
        };

        // `impl<...> A for AMock<...> { ... }`
        let mut trait_impl_items = trait_impl_methods;
        let trait_type_items =
            assoc_types.iter().cloned().zip(assoc_types.iter().cloned()).map(|(assoc, param)| {
                let path: Path = param.into();

                ImplItem::Type(ImplItemType {
                    attrs: vec![],
                    vis: Visibility::Inherited,
                    defaultness: None,
                    type_token: Token![type](Span::call_site()),
                    ident: assoc,
                    generics: Generics::default(),
                    eq_token: Token![=]([Span::call_site()]),
                    ty: Type::Path(TypePath { path, qself: None }),
                    semi_token: Token![;]([Span::call_site()]),
                })
            });
        let trait_impl_item = quote!{
            impl #generics #trait_path for #struct_type {
                #(#trait_type_items)*
                #(#trait_impl_items)*
            }
        };

        generated_items.push(impl_item);
        generated_items.push(trait_impl_item);
    }

    let mocked_class_name = traits.iter()
        .map(|&(ref path, _)| {
            let mut token_stream = TokenStream::new();
            path.to_tokens(&mut token_stream);
            token_stream.to_string()
        })
        .join("+");

    let mock_impl_item = generate_mock_impl(&mock_ident, &mocked_class_name, &assoc_types, &quote!{});
    generated_items.push(mock_impl_item);

    let assoc_types_ref = &assoc_types;
    let debug_impl_item = quote!{
        impl<#(#assoc_types_ref),*> ::std::fmt::Debug for #mock_ident_ref<#(#assoc_types_ref),*> {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                f.write_str(self.scenario.borrow().get_mock_name(self.mock_id))
            }
        }
    };
    generated_items.push(debug_impl_item);

    let has_generic_method =
        Itertools::flatten(traits.iter().map(|&(_, members)| members.iter()))
        .any(|member| match member {
            TraitItem::Method(TraitItemMethod { ref sig, .. }) => !sig.decl.generics.params.is_empty(),
            _ => false
        });
    if local && !has_generic_method {
        let (ref trait_path, _) = traits[traits.len()-1];

        // Create path for trait being mocked. Path includes bindings for all associated types.
        // Generated impl example:
        //
        //     impl<Item> ::mockers::Mocked for &'static A<Item=Item> {
        //         type MockImpl = AMock<Item>;
        //     }
        let assoc_types_ref2 = assoc_types_ref;
        let mocked_impl_item = quote!{
            impl<#(#assoc_types_ref),*> ::mockers::Mocked
                for &'static #trait_path<#(#assoc_types_ref=#assoc_types_ref2),*> {
                type MockImpl = #mock_ident_ref<#(#assoc_types_ref),*>;
            }
        };

        generated_items.push(mocked_impl_item)
    }

    Ok(quote!{ #(#generated_items)* })
}

/// Create mock structure. Structure is quite simple and basically contains only reference
/// to scenario and own ID.
/// Associated types of original trait are converted to type parameters.
/// Since type parameters are unused, we have to use PhantomData for each of them.
/// We use tuple of |PhantomData| to create just one struct field.
fn generate_mock_struct(mock_ident: &Ident, associated_type_idents: &[Ident]) -> TokenStream {
    let phantom_types: Vec<_> = associated_type_idents.iter()
        .map(|ty_param| {
            quote!{ ::std::marker::PhantomData<#ty_param> }
        })
        .collect();
    let phantom_tuple_type = quote!{ (#(#phantom_types),*) };

    quote!{
        pub struct #mock_ident<#(#associated_type_idents),*> {
            scenario: ::std::rc::Rc<::std::cell::RefCell<::mockers::ScenarioInternals>>,
            mock_id: usize,
            _phantom_data: #phantom_tuple_type,
        }
    }
}

fn generate_mock_impl(mock_ident: &Ident, mocked_class_name: &str, associated_type_idents: &[Ident],
                      custom_init_code: &TokenStream) -> TokenStream {
    let phantom_data_initializers: Vec<_> = associated_type_idents.iter()
        .map(|_| {
            quote!{ ::std::marker::PhantomData }
        })
        .collect();
    quote!{
        impl<#(#associated_type_idents),*> ::mockers::Mock for #mock_ident<#(#associated_type_idents),*> {
            fn new(id: usize, scenario_int: ::std::rc::Rc<::std::cell::RefCell<::mockers::ScenarioInternals>>) -> Self {
                #custom_init_code
                #mock_ident {
                    scenario: scenario_int,
                    mock_id: id,
                    _phantom_data: (#(#phantom_data_initializers),*),
                }
            }

            fn mocked_class_name() -> &'static str {
                #mocked_class_name
            }
        }
    }
}

struct GeneratedMethods {
    trait_impl_method: TokenStream,
    impl_method: TokenStream,
}

fn generate_trait_methods(method_ident: Ident,
                          decl: &FnDecl,
                          generics: &Generics,
                          trait_path: &Path)
                          -> Result<GeneratedMethods, String> {
    // There must be at least `self` arg.
    if decl.inputs.is_empty() {
        return Err("Methods without `self` parameter are not supported".to_string());
    }

    // Arguments without `&self`.
    let self_arg = &decl.inputs[0];
    let args: Vec<_> = decl.inputs.iter().cloned().skip(1).collect();

    match *self_arg {
        FnArg::SelfRef(..) |
        FnArg::SelfValue(..) => {}
        _ => {
            return Err("only non-static methods (with `self`, `&self` or `&mut self` argument) \
                        are supported"
                .to_string())
        }
    };

    let return_type = match decl.output {
        ReturnType::Default => Box::new(Type::Tuple(TypeTuple {
                        paren_token: syn::token::Paren::default(),
                        elems: Punctuated::new(),
                    })),
        ReturnType::Type(_, ref ty) => ty.clone(),
    };

    let mock_type_id = unsafe {
        let id = NEXT_MOCK_TYPE_ID;
        NEXT_MOCK_TYPE_ID += 1;
        id
    };

    let trait_impl_method = generate_trait_impl_method(mock_type_id,
                                                       method_ident.clone(),
                                                       generics,
                                                       self_arg,
                                                       &args,
                                                       &return_type);
    let impl_method =
        generate_impl_method_for_trait(mock_type_id, method_ident, generics, &args, &return_type, trait_path);


    //if let (Ok(tim), Ok(im)) = (trait_impl_method, impl_method) {
        Ok(GeneratedMethods {
            trait_impl_method: trait_impl_method.expect("trait_impl_method"),
            impl_method: impl_method.expect("impl_method"),
        })
    //} else {
        //Err("failed to generate impl".to_string())
    //}
}

/// Generate mocked trait method implementation for mock struct.
///
/// Implementation just packs all arguments into tuple and
/// sends them to scenario object.
///
/// For example, for trait method:
/// ```ignore
/// fn method(&self, foo: i32, bar: u16) -> u8;
/// ```
///
/// following implementation will be generated:
/// ```ignore
/// fn method(&self, foo: i32, bar: u16) -> u8 {
///     let actin = result: Box<u8> = unsafe { Box::from_raw(result_ptr as *mut u8) };
///     let method_data =
///         ::mockers::MethodData{mock_id: self.mock_id,
///                               mock_type_id: 15usize,
///                               method_name: "method",};
///     let action = self.scenario.borrow_mut().verify2(method_data, foo, bar);
///     action.call()
/// }
/// ```
/// where constant marked with `mock_id` is unique trait method ID.
fn generate_trait_impl_method(mock_type_id: usize,
                              method_ident: Ident,
                              generics: &Generics,
                              self_arg: &FnArg,
                              args: &[FnArg],
                              return_type: &Type)
                              -> Result<TokenStream, String> {
    let get_info_expr = quote!{ (self.mock_id, &self.scenario) };
    generate_stub_code(mock_type_id, &method_ident, generics, Some(self_arg), get_info_expr,
                       args, return_type, false)
}

fn generate_stub_code(mock_type_id: usize,
                      method_ident: &Ident,
                      generics: &Generics,
                      self_arg: Option<&FnArg>,
                      get_info_expr: TokenStream,
                      args: &[FnArg],
                      return_type: &Type,
                      is_unsafe: bool)
                      -> Result<TokenStream, String> {
    let method_name = method_ident.to_string();
    // Generate expression returning tuple of all method arguments.
    let arg_values: Vec<Expr> = args.iter()
        .flat_map(|i| {
            if let FnArg::Captured(ArgCaptured { pat: Pat::Ident(PatIdent { ref ident, .. }), .. }) = i {
                Some(Expr::Path(ExprPath { path: Path::from(ident.clone()), attrs: vec![], qself: None }))
            } else {
                // cx.span_err(i.pat.span, "Only identifiers are accepted in argument list");
                None
            }
        })
        .collect();
    if arg_values.len() < args.len() {
        return Err("".to_string());
    }

    let verify_fn = Ident::new(&format!("verify{}", args.len()), Span::call_site());

    let mut impl_args: Vec<FnArg> = args.iter()
        .map(|a| {
            let (ident, ty) = match *a {
                FnArg::Captured(ArgCaptured { ref ty, pat: Pat::Ident(PatIdent { ref ident, .. }), .. }) => (ident.clone(), ty.clone()),
                _ => panic!("argument pattern"),
            };
            FnArg::Captured(ArgCaptured {
                pat: Pat::Ident(PatIdent { by_ref: None, mutability: Some(Token![mut](Span::call_site())), ident, subpat: None }),
                ty,
                colon_token: Token![:]([Span::call_site()]),
            })
        })
        .collect();
    if let Some(arg) = self_arg {
        impl_args.insert(0, arg.clone());
    }

    let unsafe_t = if is_unsafe { Some(quote!{ unsafe })} else { None };
    Ok(quote!{
        #[allow(unused_mut)]
        #unsafe_t fn #method_ident #generics (#(#impl_args),*) -> #return_type {
            let (mock_id, scenario) = #get_info_expr;
            let method_data = ::mockers::MethodData { mock_id: mock_id,
                                                      mock_type_id: #mock_type_id,
                                                      method_name: #method_name, };
            let action = scenario.borrow_mut().#verify_fn(method_data, #(#arg_values),*);
            action.call()
        }
    })
}

/// Generate mock implementation method for creating expectations.
///
/// Returns `ItemImpl` for generated method or `None` in case of errors.
/// All errors are reported to `cx`.
///
/// Implementation of each method just packs all arguments into tuple and
/// sends them to scenario object.
///
/// Example of method generated for trait method `fn bar(a: u32)`:
/// ```ignore
/// #[allow(dead_code)]
/// pub fn bar_call<Arg0Match: ::mockers::MatchArg<u32>>(&self,
///                                                      arg0: Arg0Match)
///  -> ::mockers::CallMatch1<u32, ()> {
///     ::mockers::CallMatch1::new(self.mock_id, 1usize /* mock_id */,
///                                Box::new(arg0))
/// }
/// ```
fn generate_impl_method_for_trait(mock_type_id: usize,
                                  method_ident: Ident,
                                  generics: &Generics,
                                  args: &[FnArg],
                                  return_type: &Type,
                                  trait_path: &Path)
                                  -> Result<TokenStream, String> {
    use syn::PatPath;

    // Types of arguments and resul tmay refer to `Self`, which is ambiguos in the
    // context of trait implementation. All references to `Self` must be replaced
    // with `<Self as Trait>`
    let fixed_return_type = qualify_self(return_type, trait_path);
    let fixed_args = args.iter().map(|arg| {
        match arg {
            self_arg @ FnArg::SelfRef(..) => self_arg.clone(),
            self_arg @ FnArg::SelfValue(..) => self_arg.clone(),
            // TODO: do inferred arguments have to be qualified too?
            arg @ FnArg::Inferred(..) => arg.clone(),
            FnArg::Captured(ArgCaptured { pat, ty, colon_token }) => FnArg::Captured(ArgCaptured { colon_token: colon_token.clone(), pat: pat.clone(), ty: qualify_self(ty, trait_path) }),
            FnArg::Ignored(ty) => FnArg::Ignored(qualify_self(ty, trait_path)),
        }
    }).collect::<Vec<_>>();

    generate_impl_method(mock_type_id, method_ident, &generics, &fixed_args, &fixed_return_type)
}

/// Generate mock implementation method for creating expectations.
///
/// Implementation of each method just packs all arguments into tuple and
/// sends them to scenario object.
///
/// Example of method generated for trait method `fn bar(a: u32)`:
/// ```ignore
/// #[allow(dead_code)]
/// pub fn bar_call<Arg0Match: ::mockers::MatchArg<u32>>(&self,
///                                                      arg0: Arg0Match)
///  -> ::mockers::CallMatch1<u32, ()> {
///     ::mockers::CallMatch1::new(self.mock_id, 1usize /* mock_id */,
///                                Box::new(arg0))
/// }
/// ```
fn generate_impl_method(mock_type_id: usize,
                        method_ident: Ident,
                        generics: &Generics,
                        args: &[FnArg],
                        return_type: &Type)
                        -> Result<TokenStream, String> {
    use syn::Lifetime;

    // For each argument generate...
    let mut arg_matcher_types = Vec::<TokenStream>::new();
    let mut inputs = Vec::<TokenStream>::new();

    // Arguments passed to `CallMatchN::new` method inside mock method body.
    let mut new_args = Vec::<TokenStream>::new();
    new_args.push(quote!{ self.mock_id });
    new_args.push(quote!{ #mock_type_id });
    let method_name = &method_ident;
    new_args.push(quote!{ #method_name });

    // Lifetimes used for reference-type parameters.
    let mut arg_lifetimes = Vec::new();
    let mut new_arg_types = Vec::new();

    for (i, arg) in args.iter().enumerate() {
        let arg_type = match arg {
            FnArg::Captured(ArgCaptured { ref ty, .. }) => ty.clone(),
            FnArg::Ignored(ref ty) => ty.clone(),
            _ => {
                println!("{:?}", arg);
                panic!("unreachable code");
            },
        };
        let arg_type_ident = Ident::new(&format!("Arg{}Match", i), Span::call_site());
        let arg_ident = Ident::new(&format!("arg{}", i), Span::call_site());

        // To support reference parameters we must create lifetime parameter for each of them
        // and modify parameter type to adopt new lifetime.
        // Generated method signature for reference parameter looks like this:
        //
        // ```ignore
        // pub fn foo_call<'a0, Arg0Match: ::mockers::MatchArg<&'a0 u32> + 'static>
        //                (&self, arg0: Arg0Match)
        //  -> ::mockers::CallMatch1<&'a0 u32, ()>;
        // ```
        let new_arg_type = match &arg_type {
            // Parameter is reference
            Type::Reference(TypeReference { ref elem, ref mutability, .. }) => {
                // Create separate lifetime.
                let lifetime = quote!{'a#i};

                //let lifetime = Ident::new(&format!("'a{}", i), Span::call_site());
                //let lifetime = quote!{ #lifetime };
                arg_lifetimes.push(lifetime.clone());
                quote!{ &#lifetime #mutability #elem }
            }

            // Parameter is not reference
            _ => quote!{ #arg_type },
        };
        new_arg_types.push(new_arg_type.clone());

        // 1. Type parameter
        let match_arg_path = quote! { ::mockers::MatchArg<#new_arg_type>};
        arg_matcher_types.push(quote! { #arg_type_ident: #match_arg_path + 'static });
        inputs.push(quote! { #arg_ident: #arg_type_ident });

        new_args.push(quote!{ Box::new(#arg_ident) });
    }

    let call_match_ident = Ident::new(&format!("CallMatch{}", args.len()), Span::call_site());

    let mut call_match_args: Vec<_> = new_arg_types;
    call_match_args.push(quote!{ #return_type });
    let ret_type = quote!{ ::mockers::#call_match_ident<#(#call_match_args),*> };

    let output = ret_type.clone();
    let expect_method_name = Ident::new(&format!("{}_call", method_ident), Span::call_site());

    let debug_param_bound: TypeParamBound = parse_quote!{::std::fmt::Debug};
    let generic_params = [&arg_lifetimes[..],
                          &generics.type_params()
                                             .map(|p| {
                                                 let mut p = p.clone();
                                                 p.bounds.push(debug_param_bound.clone());
                                                 quote!{ #p }
                                             })
                                             .collect::<Vec<_>>()[..],
                          &arg_matcher_types[..]].concat();

    let impl_subitem: TokenStream = quote!{
        #[allow(dead_code)]
        pub fn #expect_method_name<#(#generic_params),*>(&self, #(#inputs),*) -> #output {
            ::mockers::#call_match_ident::new(#(#new_args),*)
        }
    };

    Ok(impl_subitem)
}


fn generate_extern_mock(foreign_mod: &syn::ItemForeignMod, mock_ident: &Ident) -> Result<TokenStream, String> {
    let mock_type_id = unsafe {
        let id = NEXT_MOCK_TYPE_ID;
        NEXT_MOCK_TYPE_ID += 1;
        id
    };

    let (mock_items, stub_items): (Vec<_>, Vec<_>) = foreign_mod.items.iter().map(|item| {
        match item {
            ForeignItem::Fn(ForeignItemFn { ref decl, ref ident, .. }) => {
                let ret_ty = match decl.output {
                    ReturnType::Type(_, ref ty) => ty.clone(),
                    ReturnType::Default => Box::new(Type::Tuple(TypeTuple {
                        paren_token: syn::token::Paren::default(),
                        elems: Punctuated::new(),
                    })),
                };

                let inputs: Vec<_> = decl.inputs.clone().into_iter().collect();

                let mock_method = generate_impl_method(mock_type_id, ident.clone(), &decl.generics, &inputs, &ret_ty)?;

                let get_info_expr = quote!{
                    ::mockers::EXTERN_MOCKS.with(|mocks| {
                        mocks.borrow().get(&#mock_type_id).expect("Mock instance not found").clone()
                    })
                };

                let stub_method = generate_stub_code(mock_type_id, &ident, &decl.generics, None,
                                                     get_info_expr, &inputs, &ret_ty, true)?;

                Ok((mock_method, stub_method))
            },
            _ => return Err("only functions are supported in extern blocks".to_string()),
        }
    }).collect::<Result<Vec<_>, _>>()?.into_iter().unzip();

    let mock_class_name = mock_ident.to_string();

    let mock_struct = quote!{
        pub struct #mock_ident {
            mock_id: usize,
        }
    };
    let mock_impl = quote!{
        impl ::mockers::Mock for #mock_ident {
            fn new(id: usize, scenario_int: ::std::rc::Rc<::std::cell::RefCell<::mockers::ScenarioInternals>>) -> Self {
                ::mockers::EXTERN_MOCKS.with(|mocks| {
                    let mut mocks = mocks.borrow_mut();
                    if mocks.contains_key(&#mock_type_id) {
                        panic!("Mock {} for extern block already exists", #mock_class_name);
                    }
                    mocks.insert(#mock_type_id, (id, scenario_int.clone()));
                });
                #mock_ident {
                    mock_id: id,
                }
            }

            fn mocked_class_name() -> &'static str {
                #mock_class_name
            }
        }
    };

    Ok(quote!{
        #mock_struct
        #mock_impl
        impl Drop for #mock_ident {
            fn drop(&mut self) {
                ::mockers::EXTERN_MOCKS.with(|mocks| {
                    let mut mocks = mocks.borrow_mut();
                    mocks.remove(&#mock_type_id);
                });
            }
        }
        impl #mock_ident {
            #(#mock_items)*
        }
        #(#stub_items)*
    })
}

/// Replace all unqualified references to `Self` with qualified ones.
fn qualify_self(ty: &Type, trait_path: &Path) -> Type {
    use syn::TypeArray;

    fn qualify_ty(ty: &Type, trait_path: &Path) -> Type {
        match ty {
            ty @ Type::Verbatim(..) => ty.clone(),
            Type::Group(ref t) => {
                let mut clone = t.clone();

                clone.elem = Box::new(qualify_ty(&t.elem, trait_path));

                Type::Group(clone)
            },
            Type::Slice(ref type_slice) => {
                let mut clone = type_slice.clone();

                clone.elem = Box::new(qualify_ty(&type_slice.elem, trait_path));

                Type::Slice(clone)
            },
            Type::Array(ref type_arr) => {
                let mut clone = type_arr.clone();

                clone.elem = Box::new(qualify_ty(&type_arr.elem, trait_path));

                Type::Array(clone)
            },
            Type::Ptr(ref type_ptr) => {
                let mut clone = type_ptr.clone();

                clone.elem = Box::new(qualify_ty(&type_ptr.elem, trait_path));

                Type::Ptr(clone)
            }
            Type::Reference(ref reference) => {
                let mut clone = reference.clone();

                clone.elem = Box::new(qualify_ty(&reference.elem, trait_path));

                Type::Reference(clone)
            }
            Type::BareFn(ref fnty) => {
                Type::BareFn(TypeBareFn {
                    fn_token: Token![fn](Span::call_site()),
                    paren_token: syn::token::Paren::default(),
                    unsafety: fnty.unsafety,
                    abi: fnty.abi.clone(),
                    lifetimes: fnty.lifetimes.clone(),
                    inputs: fnty.inputs
                        .iter()
                        .map(|i| qualify_bare_fn_arg(&i, trait_path))
                        .collect(),
                    output: qualify_function_ret_ty(&fnty.output, trait_path),
                    variadic: fnty.variadic,
                })
            }
            Type::Never(ref type_never) => Type::Never(type_never.clone()),
            Type::Tuple(ref type_tuple) => {
                let mut clone = type_tuple.clone();

                clone.elems = type_tuple.elems.iter().map(|t| qualify_ty(t, trait_path)).collect();

                Type::Tuple(clone)
            },
            Type::Path(TypePath { ref qself, ref path } ) => {
                if qself.is_none() &&
                   path.segments.first().map(|s| s.value().ident == "Self").unwrap_or(false) {
                    let self_seg = path.segments.first().unwrap();
                    let self_ty = Type::Path(TypePath { qself: None, path:
                                           Path {
                                               leading_colon: None,
                                               segments: {
                                                   let mut segments: Punctuated<PathSegment, _> = Punctuated::new();

                                                   segments.push((*self_seg.value()).clone());

                                                   segments
                                               },
                                           }});
                    let new_qself = QSelf {
                        ty: Box::new(self_ty),
                        position: trait_path.segments.len(),
                        as_token: Some(Token![as](Span::call_site())),
                        gt_token: Token![>]([Span::call_site()]),
                        lt_token: Token![<]([Span::call_site()]),
                    };
                    let mut new_segments = trait_path.segments.clone();
                    new_segments.extend(path.segments.iter().cloned().skip(1));
                    let a = Type::Path(TypePath {
                        qself: Some(new_qself),
                        path: Path {
                            leading_colon: None,
                            segments: new_segments,
                        },
                    });
                    a
                } else {
                    Type::Path(TypePath { qself: qself.clone(), path: qualify_path(&path, trait_path) })
                }
            }
            t @ Type::TraitObject(..) => t.clone(),
            Type::ImplTrait(ref bounds) => Type::ImplTrait(bounds.clone()),
            Type::Paren(ref t) => {
                let mut clone = t.clone();

                clone.elem = Box::new(qualify_ty(&t.elem, trait_path));

                Type::Paren(clone)
            },
            Type::Infer(ref type_infer) => Type::Infer(type_infer.clone()),
            Type::Macro(ref mac) => Type::Macro(mac.clone()),
        }
    }
    fn qualify_bare_fn_arg(arg: &BareFnArg, trait_path: &Path) -> BareFnArg {
        BareFnArg {
            name: arg.name.clone(),
            ty: qualify_ty(&arg.ty, trait_path),
        }
    }
    fn qualify_function_ret_ty(ret_ty: &ReturnType, trait_path: &Path) -> ReturnType {
        match *ret_ty {
            ReturnType::Default => ReturnType::Default,
            ReturnType::Type(ref r_arrow, ref ty) => ReturnType::Type(r_arrow.clone(), Box::new(qualify_ty(&ty, trait_path))),
        }
    }
    fn qualify_path(path: &Path, trait_path: &Path) -> Path {
        Path {
            leading_colon: path.leading_colon.clone(),
            segments: path.segments
                .iter()
                .map(|segment| {
                    PathSegment {
                        ident: segment.ident.clone(),
                        arguments: match segment.arguments {
                            PathArguments::None => PathArguments::None,
                            PathArguments::AngleBracketed(ref data) => {
                                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                    args: data.args
                                        .iter()
                                        .map(|arg| {
                                            match arg {
                                                GenericArgument::Type(ref ty) => GenericArgument::Type(qualify_ty(ty, trait_path)),
                                                _ => arg.clone(),
                                            }
                                        })
                                        .collect(),
                                    ..data.clone()
                                })
                            }
                            PathArguments::Parenthesized(ParenthesizedGenericArguments { ref inputs, ref output, ref paren_token }) => {
                                let output = match output {
                                    ReturnType::Default => ReturnType::Default,
                                    ReturnType::Type(ref r_arrow, ref ty) => {
                                        ReturnType::Type(r_arrow.clone(), Box::new(qualify_ty(ty, trait_path)))
                                    }
                                };

                                PathArguments::Parenthesized(ParenthesizedGenericArguments {
                                    paren_token: paren_token.clone(),
                                    inputs: inputs
                                        .iter()
                                        .map(|i| qualify_ty(i, trait_path))
                                        .collect(),
                                    output,
                                })
                            }
                        },
                    }
                })
                .collect(),
        }
    }

    qualify_ty(&ty, trait_path)
}

#[proc_macro]
pub fn mock(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match mock_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => panic!("{}", err),
    }
}

// Stealed from syn crate.
fn unwrap<T>(name: &'static str,
             f: fn(&str) -> synom::IResult<&str, T>,
             input: &str)
             -> Result<T, String> {
    match f(input) {
        synom::IResult::Done(mut rest, t) => {
            rest = synom::space::skip_whitespace(rest);
            if rest.is_empty() {
                Ok(t)
            } else if rest.len() == input.len() {
                // parsed nothing
                Err(format!("failed to parse {}: {:?}", name, rest))
            } else {
                Err(format!("unparsed tokens after {}: {:?}", name, rest))
            }
        }
        synom::IResult::Error => Err(format!("failed to parse {}: {:?}", name, input)),
    }
}

fn mock_impl(input: TokenStream) -> Result<TokenStream, String> {
    use syn::synom::{Synom, PResult};
    use syn::buffer::Cursor;
    use syn::Item;

    fn parse_path(i: &str) -> synom::IResult<&'static str, Path> {
        let buf = TokenBuffer::new2(TokenStream::from_str(i).unwrap());
        
        match Path::parse(buf.begin()) {
            Ok((o, _)) => synom::IResult::Done("", o),
            Err(_) => synom::IResult::Error,
        }
    }

    fn parse_ident(i: &str) -> synom::IResult<&'static str, Ident> {
        let buf = TokenBuffer::new2(TokenStream::from_str(i).unwrap());
        
        match Ident::parse(buf.begin()) {
            Ok((o, _)) => synom::IResult::Done("", o),
            Err(_) => synom::IResult::Error,
        }
    }

    fn parse_item(i: &str) -> synom::IResult<&'static str, Item> {
        let buf = TokenBuffer::new2(TokenStream::from_str(i).unwrap());
        
        match Item::parse(buf.begin()) {
            Ok((o, _)) => synom::IResult::Done("", o),
            Err(_) => synom::IResult::Error,
        }
    }

    named!(mock_args -> (Ident, Vec<TraitDesc>), do_parse!(
        ident: parse_ident >>
        punct!(",") >>
        traits: separated_list!(punct!(","), do_parse!(
            path: alt!(
                map!(keyword!("self"), |_| Path { leading_colon: None, segments: Punctuated::default() })
                | parse_path
            ) >>
            punct!(",") >>
            trait_item: parse_item >>
            (TraitDesc { mod_path: path, trait_item: trait_item })
        )) >>
        (ident, traits)
    ));

    let source = input.to_string();
    let args = unwrap("mock! arguments", mock_args, &source)?;
    let tokens = generate_mock_for_traits(args.0, &args.1, false)?;

    if cfg!(feature="debug") {
        println!("{}", tokens.to_string());
    }

    Ok(TokenStream::from(tokens))
}
