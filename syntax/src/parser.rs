use crate::ext::{DelimByParserExt, RecoverWithDelimParserExt, SourcedNodeParserExt};
use crate::nonempty::NonemptyArray;
use crate::source::{SourcedNode, SourcedSpan};
use crate::token::Delim;
use crate::{alias, ast, token::Token};
use chumsky::extra::ParserExtra;
use chumsky::input::{MapExtra, ValueInput};
use chumsky::pratt::{infix, left, prefix};
use chumsky::{input::BorrowInput, prelude::*, select_ref, Parser};

pub fn ident_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Ident>
where
    I: BorrowInput<'src, Token = Token>,
{
    // identifiers can be extracted directly from `Ident` tokens, copying
    // an internal atomic reference to an interned identifier string
    (select_ref! { Token::Ident(x) => x.clone() }).labelled("<ident>")
}

pub fn liter_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Liter>
where
    I: BorrowInput<'src, Token = Token>,
{
    // some literals can be extracted from their corresponding tokens
    let int_liter =
        select_ref! { Token::IntLiter(x) => ast::Liter::IntLiter(*x) }.labelled("<int-liter>");
    let char_liter =
        select_ref! { Token::CharLiter(x) => ast::Liter::CharLiter(*x) }.labelled("<char-liter>");
    let str_liter = select_ref! { Token::StrLiter(x) => ast::Liter::StrLiter(x.clone()) }
        .labelled("<str-liter>");

    // some literals can be created from keywords
    let bool_liter = choice((
        just(Token::True).to(ast::Liter::BoolLiter(true)),
        just(Token::False).to(ast::Liter::BoolLiter(false)),
    ))
    .labelled("<bool-liter>");
    let pair_liter = just(Token::Null)
        .to(ast::Liter::PairLiter)
        .labelled("<pair-liter>");

    let token = choice((int_liter, char_liter, str_liter, bool_liter, pair_liter));
    token
}

pub fn array_elem_parser<'src, I, Ident, Expr>(
    ident: Ident,
    expr: Expr,
) -> impl alias::Parser<'src, I, ast::ArrayElem>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan>,
    Ident: alias::Parser<'src, I, ast::Ident>,
    Expr: alias::Parser<'src, I, ast::Expr>,
{
    let array_elem_indices = expr
        .delim_by(Delim::Bracket)
        .sn()
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(NonemptyArray::try_from_boxed_slice)
        .map(Result::unwrap);
    let array_elem = group((ident.sn(), array_elem_indices))
        .map_group(ast::ArrayElem::new)
        .labelled("<array-elem>");

    array_elem
}

pub fn expr_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Expr>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
{
    recursive(|expr| {
        let ident = ident_parser();
        let liter = liter_parser();
        let array_elem = array_elem_parser(ident.clone(), expr.clone());

        // parse parenthesized expressions
        let paren_expr = expr.delim_by(Delim::Paren).sn();

        // 'Atoms' are expressions that contain no ambiguity
        let atom = choice((
            liter.map(ast::Expr::Liter),
            // array elements begin with identifiers, so
            // give them precedence over identifiers
            array_elem.map(ast::Expr::ArrayElem),
            ident.map(ast::Expr::Ident),
            paren_expr.map(ast::Expr::Paren),
        ));

        // Perform simplistic error recovery on Atom expressions
        let atom = atom
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with_delim(Delim::Paren, ast::Expr::Error)
            // Attempt to recover anything that looks like an array-element but contains errors
            .recover_with_delim(Delim::Bracket, ast::Expr::Error);

        // unary and binary operator parsers
        let unary_oper = choice((
            just(Token::Bang).to(ast::UnaryOper::Not),
            just(Token::Minus).to(ast::UnaryOper::Minus),
            just(Token::Len).to(ast::UnaryOper::Len),
            just(Token::Ord).to(ast::UnaryOper::Ord),
            just(Token::Chr).to(ast::UnaryOper::Chr),
        ))
        .sn()
        .labelled("<unary-oper>");
        let product_oper = choice((
            just(Token::Star).to(ast::BinaryOper::Mul),
            just(Token::ForwardSlash).to(ast::BinaryOper::Div),
            just(Token::Percent).to(ast::BinaryOper::Mod),
        ))
        .sn()
        .labelled("<binary-oper>");
        let sum_oper = choice((
            just(Token::Plus).to(ast::BinaryOper::Add),
            just(Token::Minus).to(ast::BinaryOper::Sub),
        ))
        .sn()
        .labelled("<binary-oper>");
        let arith_cmp_oper = choice((
            just(Token::Lte).to(ast::BinaryOper::Lte),
            just(Token::Lt).to(ast::BinaryOper::Lt),
            just(Token::Gte).to(ast::BinaryOper::Gte),
            just(Token::Gt).to(ast::BinaryOper::Gt),
        ))
        .sn()
        .labelled("<binary-oper>");
        let eq_cmp_oper = choice((
            just(Token::EqualsEquals).to(ast::BinaryOper::Eq),
            just(Token::BangEquals).to(ast::BinaryOper::Neq),
        ))
        .sn()
        .labelled("<binary-oper>");
        let land_oper = just(Token::And)
            .to(ast::BinaryOper::And)
            .sn()
            .labelled("<binary-oper>");
        let lor_oper = just(Token::Or)
            .to(ast::BinaryOper::Or)
            .sn()
            .labelled("<binary-oper>");

        // procedure to fold PRATT patterns into binary expressions
        let binary_fold = |lhs, op, rhs, extra: &mut MapExtra<'src, '_, I, _>| {
            SourcedNode::new(ast::Expr::Binary(lhs, op, rhs), extra.span())
        };

        // a PRATT parser for prefix and infix operator expressions
        let expr = atom.sn().pratt((
            // We want unary operations to happen before any binary ones, so we need their precedence
            // is set to be the highest. But amongst themselves the precedence is the same.
            prefix(7, unary_oper, |op, rhs, extra| {
                SourcedNode::new(ast::Expr::Unary(op, rhs), extra.span())
            }),
            // Product ops (multiply, divide, and mod) have equal precedence, and the highest
            // binary operator precedence overall
            infix(left(6), product_oper, binary_fold),
            // Sum ops (add and subtract) have equal precedence, just below the precedence
            // of product ops
            infix(left(5), sum_oper, binary_fold),
            // Arithmetic comparisons (<, <=, >, >=) have equal precedence, just below the
            // precedence of sum ops
            infix(left(4), arith_cmp_oper, binary_fold),
            // Equality comparisons (== and !=) have equal precedence, just below the
            // precedence of arithmetic comparisons
            infix(left(3), eq_cmp_oper, binary_fold),
            // logical AND (&&) has more precedence than logical OR (||), where AND has
            // precedence just below that of equality comparisons
            infix(left(2), land_oper, binary_fold),
            infix(left(1), lor_oper, binary_fold),
        ));

        // as of now, no other type of expression exists
        expr.map(SourcedNode::into_inner) // TODO: this removes span from output. If this is not desirable, undo this line
    })
}

pub fn type_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Type>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
{
    recursive(|r#type| {
        // base types have no recursion
        let base_type = choice((
            just(Token::Int).to(ast::BaseType::Int),
            just(Token::Bool).to(ast::BaseType::Bool),
            just(Token::Char).to(ast::BaseType::Char),
            just(Token::String).to(ast::BaseType::String),
        ))
        .sn()
        .labelled("<base-type>");

        // array types are left-recursive, and `chumsky` is a PEG parser meaning it does not
        // handle left-recursion by default (i.e. it will recurse infinitely) so we need to use
        // memoization in order to prevent this and allow correct left-recursive grammar parsing
        let array_type = r#type
            .foldl_with(
                // here we have to try and match on zero-or-more `[]` occurrences; its the only
                // way to get the parser to consume the array-related brackets properly;
                // setting a minimum with `at_least(1)` makes it not work for some reason
                group((
                    just(Token::Open(Delim::Bracket)),
                    just(Token::Close(Delim::Bracket)),
                ))
                .ignored()
                .repeated(),
                |ty, _, extra| {
                    ast::Type::ArrayType(SourcedNode::new(ast::ArrayType::new(ty), extra.span()))
                },
            )
            // once we collected any potential array-types in full, we should fail the parsing
            // route if there weren't any actual array-types; this should never ACTUALLY cause
            // errors at the end, it's just a somewhat hacky way to do parser control-flow
            .try_map(|ty, span| match ty {
                ast::Type::ArrayType(ty) => Ok(ty.into_inner()),
                _ => Err(Rich::custom(
                    span,
                    format!("The type `{:?}` is not an array-type", ty),
                )),
            })
            .sn()
            .memoized()
            .labelled("<array-type>");

        // pair-element type parser
        let pair_elem_type = choice((
            array_type.clone().map(ast::PairElemType::ArrayType),
            base_type.clone().map(ast::PairElemType::BaseType),
            just(Token::Pair).to_span().map(ast::PairElemType::Pair),
        ))
        .labelled("<pair-elem-type>");
        let pair_type = just(Token::Pair)
            .ignore_then(
                group((
                    pair_elem_type.clone().then_ignore(just(Token::Comma)),
                    pair_elem_type,
                ))
                .map_group(ast::Type::PairType)
                .delim_by(Delim::Paren)
                // Attempt to recover anything that looks like a (parenthesised) pair type but contains errors
                .recover_with_delim(Delim::Paren, ast::Type::Error),
            )
            .labelled("<pair-type>");

        // a type is either a base type, array type, or pair type;
        // an array type and all other types look the same until the very last, so we should
        // give precedence to array types to make sure they are not incorrectly missed
        let r#type = choice((
            array_type.map(ast::Type::ArrayType),
            base_type.map(ast::Type::BaseType),
            pair_type,
        ))
        .labelled("<type>");

        r#type
    })
}

// pub fn type_parser() -> impl Parser<Token, Spanned<Type>, Error = Simple<Token>> + Clone {
//     // base types have no recursion
//     let rrbase_type = choice((
//         just(Token::Keyword(Keyword::Int)).to(BaseType::Int),
//         just(Token::Keyword(Keyword::Bool)).to(BaseType::Bool),
//         just(Token::Keyword(Keyword::Char)).to(BaseType::Char),
//         just(Token::Keyword(Keyword::String)).to(BaseType::String),
//     ))
//     .labelled("<base-type>");
//
//     // array-types are left-recursive thus the parser will loop infinitely if not eliminated;
//     // we can eliminate by rewriting production rules from the following:
//     //
//     // <type>           => <base-type> | <array-type> | <pair-type>
//     // <base-type>      => 'int' | 'bool' | 'char' | 'string'
//     // <array-type>     => <type> '[' ']'
//     // <pair-type>      => 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
//     // <pair-elem-type> => <base-type> | <array-type> | 'pair'
//     //
//     // into the following right-recursive set of production rules:
//     //
//     // <type>             => <base-type> | <array-type> | <pair-type>
//     // <base-type>        => 'int' | 'bool' | 'char' | 'string'
//     // <type-excl-array>   => <base-type> | <pair-type>
//     // <array-type>       => <type-excl-array> '[' ']' <array-type-prime>
//     // <array-type-prime> => '[' ']' <array-type-prime> | ϵ
//     // <pair-type>        => 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
//     // <pair-elem-type>   => <base-type> | <array-type> | 'pair'
//     //
//     // where epsilon (ϵ) means empty string/token-stream. In order to conveniently implement
//     // these new production rules, new temporary types are be created which capture these rules
//
//     #[derive(Clone, Debug)]
//     enum RRType {
//         BaseType(BaseType),
//         RRArrayType(RRArrayType),
//         RRPairType(RRPairType),
//     }
//
//     #[derive(Clone, Debug)]
//     enum RRTypeExclArray {
//         BaseType(BaseType),
//         RRPairType(Box<RRPairType>),
//     }
//
//     #[derive(Clone, Debug)]
//     struct RRArrayType(RRTypeExclArray, RRArrayTypePrime);
//
//     #[derive(Debug, Clone)]
//     enum RRArrayTypePrime {
//         OpenClosesBracket(Box<RRArrayTypePrime>),
//         Epsilon,
//     }
//
//     #[derive(Clone, Debug)]
//     struct RRPairType(RRPairElemType, RRPairElemType);
//
//     #[derive(Clone, Debug)]
//     enum RRPairElemType {
//         BaseType(BaseType),
//         RRArrayType(RRArrayType),
//         Pair,
//     }
//
//     // also create converters to turn the right-recursive structures back into normal AST nodes
//
//     fn convert_rrtype(rrtype: RRType) -> Type {
//         match rrtype {
//             RRType::BaseType(b) => Type::BaseType(b),
//             RRType::RRArrayType(a) => Type::ArrayType(convert_rrarray_type(a)),
//             RRType::RRPairType(RRPairType(a, b)) => {
//                 Type::PairType(convert_rrpair_elem_type(a), convert_rrpair_elem_type(b))
//             }
//         }
//     }
//
//     fn convert_rrarray_type(rrarray_type: RRArrayType) -> ArrayType {
//         let RRArrayType(element_type, mut prime) = rrarray_type;
//
//         // extract the innermost array type, then build atop it by unwrapping the RRArrayTypePrime
//         let mut array_type = ArrayType {
//             elem_type: Box::new(match element_type {
//                 RRTypeExclArray::BaseType(b) => Type::BaseType(b),
//                 RRTypeExclArray::RRPairType(p) => {
//                     Type::PairType(convert_rrpair_elem_type(p.0), convert_rrpair_elem_type(p.1))
//                 }
//             }),
//         };
//
//         // iterate and unwrap RRArrayTypePrime until we reach epsilon
//         while let RRArrayTypePrime::OpenClosesBracket(boxed_prime) = &prime {
//             array_type = ArrayType {
//                 elem_type: Box::new(Type::ArrayType(array_type)),
//             };
//             prime = boxed_prime.as_ref().clone()
//         }
//
//         array_type
//     }
//
//     fn convert_rrpair_elem_type(rrpair_elem_type: RRPairElemType) -> PairElemType {
//         match rrpair_elem_type {
//             RRPairElemType::BaseType(b) => PairElemType::BaseType(b),
//             RRPairElemType::RRArrayType(a) => PairElemType::ArrayType(convert_rrarray_type(a)),
//             RRPairElemType::Pair => PairElemType::Pair,
//         }
//     }
//
//     // create parser for: <array-type-prime> => '[' ']' <array-type-prime> | ϵ, in order to capture this right-recursion
//     let rrarray_type_prime = recursive(
//         |prime: Recursive<'_, Token, RRArrayTypePrime, Simple<Token>>| {
//             just(Token::Symbol(Symbol::OpenBracket))
//                 .ignore_then(just(Token::Symbol(Symbol::CloseBracket)))
//                 .ignore_then(prime)
//                 .map(|p| RRArrayTypePrime::OpenClosesBracket(Box::new(p)))
//                 .or_not()
//                 .map(|p| p.unwrap_or(RRArrayTypePrime::Epsilon))
//         },
//     );
//
//     // pair-type parser factory
//     fn rrpair_type_factory<E: Error<Token>, P>(
//         rrpair_elem_type: P,
//     ) -> impl Parser<Token, RRPairType, Error = E> + Clone
//     where
//         P: Parser<Token, RRPairElemType, Error = E> + Clone,
//     {
//         just(Token::Keyword(Keyword::Pair))
//             .ignore_then(just(Token::Symbol(Symbol::OpenParen)))
//             .ignore_then(
//                 rrpair_elem_type
//                     .clone()
//                     .then_ignore(just(Token::Symbol(Symbol::Comma)))
//                     .then(rrpair_elem_type)
//                     .then_ignore(just(Token::Symbol(Symbol::CloseParen))),
//             )
//             .map(|(a, b)| RRPairType(a, b))
//     }
//
//     // base-type and pair-type variant array-type parsers
//     let rrbase_type_variant_array_type = rrbase_type
//         .clone()
//         .then_ignore(just(Token::Symbol(Symbol::OpenBracket)))
//         .then_ignore(just(Token::Symbol(Symbol::CloseBracket)))
//         .then(rrarray_type_prime.clone())
//         .map(|(b, prime)| RRArrayType(RRTypeExclArray::BaseType(b), prime));
//     fn pair_type_variant_array_type_factory<E: Error<Token>, P, A>(
//         rrpair_elem_type: P,
//         rrarray_type_prime: A,
//     ) -> impl Parser<Token, RRArrayType, Error = E> + Clone
//     where
//         P: Parser<Token, RRPairElemType, Error = E> + Clone,
//         A: Parser<Token, RRArrayTypePrime, Error = E> + Clone,
//     {
//         rrpair_type_factory(rrpair_elem_type)
//             .then_ignore(just(Token::Symbol(Symbol::OpenBracket)))
//             .then_ignore(just(Token::Symbol(Symbol::CloseBracket)))
//             .then(rrarray_type_prime)
//             .map(|(p, prime)| RRArrayType(RRTypeExclArray::RRPairType(Box::new(p)), prime))
//     }
//
//     // pair-elem-type parser, which tires to re-use as many building blocks as possible
//     // without incurring extra recursion
//     let rrpair_elem_type = recursive(
//         |pair_elem_type: Recursive<'_, Token, RRPairElemType, Simple<Token>>| {
//             choice((
//                 rrbase_type.clone().map(RRPairElemType::BaseType),
//                 rrbase_type_variant_array_type
//                     .clone()
//                     .map(RRPairElemType::RRArrayType),
//                 pair_type_variant_array_type_factory(pair_elem_type, rrarray_type_prime.clone())
//                     .map(RRPairElemType::RRArrayType),
//                 just(Token::Keyword(Keyword::Pair)).to(RRPairElemType::Pair),
//             ))
//         },
//     );
//
//     // trivially create pair-type parser and array-type parser
//     let rrpair_type = rrpair_type_factory(rrpair_elem_type.clone());
//     let rrarray_type = rrbase_type_variant_array_type.or(pair_type_variant_array_type_factory(
//         rrpair_elem_type,
//         rrarray_type_prime,
//     ));
//
//     // when creating the type parser, give precedence to array-types, so they are
//     // parsed first to the maximal extent if they exist, otherwise fall back to base/pair types
//     let rrtype = rrarray_type
//         .map(RRType::RRArrayType)
//         .or(rrbase_type.map(RRType::BaseType))
//         .or(rrpair_type.map(RRType::RRPairType));
//
//     // TODO: figure out how and where to place error recovery for the type parser
//
//     // return the final parsed type
//     rrtype.map_with_span(|t, s| Spanned::new(convert_rrtype(t), s))
// }
//
// pub fn stat_parser_factory<P>(
//     stat_chain: P,
// ) -> impl Parser<Token, Spanned<Stat>, Error = Simple<Token>> + Clone
// where
//     P: Parser<Token, Spanned<StatChain>, Error = Simple<Token>> + Clone,
// {
//     let expr = expr_parser();
//     let r#type = type_parser();
//
//     // identifiers can be parsed directly from `Ident` tokens
//     let ident = select! { Token::Ident(s) => Ident(s) }.labelled("<ident>");
//
//     // skip parser
//     let skip = just(Token::Keyword(Keyword::Skip)).to(Stat::Skip);
//
//     let expr_sequence = expr
//         .clone()
//         .separated_by(just(Token::Symbol(Symbol::Comma)));
//
//     // array literal parser
//     let array_liter = expr_sequence
//         .clone()
//         .delimited_by(
//             just(Token::Symbol(Symbol::OpenBracket)),
//             just(Token::Symbol(Symbol::CloseBracket)),
//         )
//         .map(|exprs| AssignRhs::ArrayLiter(exprs.into_iter().map(|e| e.inner).collect()));
//
//     // newpair parser
//     let newpair = just(Token::Keyword(Keyword::Newpair))
//         .ignore_then(
//             expr.clone()
//                 .then_ignore(just(Token::Symbol(Symbol::Comma)))
//                 .then(expr.clone())
//                 .delimited_by(
//                     just(Token::Symbol(Symbol::OpenParen)),
//                     just(Token::Symbol(Symbol::CloseParen)),
//                 ),
//         )
//         .map(|(a, b)| AssignRhs::Newpair(a.inner, b.inner));
//
//     // pair-elem parser
//     let pair_elem = choice((
//         just(Token::Keyword(Keyword::Fst))
//             .ignore_then(expr.clone())
//             .map(|e| PairElem::Fst(e.inner)),
//         just(Token::Keyword(Keyword::Snd))
//             .ignore_then(expr.clone())
//             .map(|e| PairElem::Snd(e.inner)),
//     ));
//
//     // function call parser
//     let function_call = just(Token::Keyword(Keyword::Call))
//         .ignore_then(ident)
//         .then(expr_sequence.delimited_by(
//             just(Token::Symbol(Symbol::OpenParen)),
//             just(Token::Symbol(Symbol::CloseParen)),
//         ))
//         .map(|(id, exprs)| AssignRhs::Call {
//             func_name: id,
//             args: exprs.into_iter().map(|e| e.inner).collect(),
//         });
//
//     // array element parser
//     let array_elem_index = expr.clone().delimited_by(
//         just(Token::Symbol(Symbol::OpenBracket)),
//         just(Token::Symbol(Symbol::CloseBracket)),
//     );
//     let array_elem = ident
//         .then(array_elem_index.clone())
//         .then(array_elem_index.repeated())
//         .map(|((array_name, first_index), other_indices)| ArrayElem {
//             array_name,
//             first_index: Box::new(first_index.inner),
//             other_indices: other_indices.iter().map(|s| s.inner.clone()).collect(),
//         })
//         .labelled("<array-elem>");
//
//     // parser which resolves array-element vs. identifier ambiguity, namely:
//     // an identifier followed by `([<expr>])+` is an array-element, and otherwise
//     // its just an identifier
//     let ident_and_array_elem = array_elem
//         .map(AssignLhs::ArrayElem)
//         .or(ident.map(AssignLhs::Ident));
//
//     // assign-lhs parser
//     let assign_lhs = ident_and_array_elem.or(pair_elem.clone().map(AssignLhs::PairElem));
//
//     // assign-rhs parser
//     let assign_rhs = choice((
//         expr.clone().map(|e| AssignRhs::Expr(e.inner)),
//         array_liter,
//         newpair,
//         pair_elem.map(AssignRhs::PairElem),
//         function_call,
//     ));
//
//     // variable definition parser
//     let variable_definition = r#type
//         .then(ident)
//         .then_ignore(just(Token::Symbol(Symbol::Equals)))
//         .then(assign_rhs.clone())
//         .map(|((t, id), rhs)| Stat::VarDefinition {
//             var_type: t.inner,
//             name: id,
//             value: rhs,
//         });
//
//     // assignment parser
//     let assignment = assign_lhs
//         .clone()
//         .then_ignore(just(Token::Symbol(Symbol::Equals)))
//         .then(assign_rhs)
//         .map(|(lhs, rhs)| Stat::Assignment { lhs, rhs });
//
//     // if-then-else parser
//     let if_then_else = just(Token::Keyword(Keyword::If))
//         .ignore_then(expr.clone())
//         .then_ignore(just(Token::Keyword(Keyword::Then)))
//         .then(stat_chain.clone())
//         .then_ignore(just(Token::Keyword(Keyword::Else)))
//         .then(stat_chain.clone())
//         .then_ignore(just(Token::Keyword(Keyword::Fi)))
//         .map(|((if_cond, then_body), else_body)| Stat::IfThenElse {
//             if_cond: if_cond.inner,
//             then_body: then_body.inner,
//             else_body: else_body.inner,
//         });
//
//     // while-loop parser
//     let while_do = just(Token::Keyword(Keyword::While))
//         .ignore_then(expr.clone())
//         .then_ignore(just(Token::Keyword(Keyword::Do)))
//         .then(stat_chain.clone())
//         .then_ignore(just(Token::Keyword(Keyword::Done)))
//         .map(|(while_cond, body)| Stat::WhileDo {
//             while_cond: while_cond.inner,
//             body: body.inner,
//         });
//
//     // begin-end scope
//     let scoped = just(Token::Keyword(Keyword::Begin))
//         .ignore_then(stat_chain.clone())
//         .then_ignore(just(Token::Keyword(Keyword::End)))
//         .map(|st| Stat::Scoped(st.inner));
//
//     // statement parser
//     let stat = choice((
//         skip,
//         variable_definition,
//         assignment,
//         just(Token::Keyword(Keyword::Read))
//             .ignore_then(assign_lhs)
//             .map(Stat::Read),
//         just(Token::Keyword(Keyword::Free))
//             .ignore_then(expr.clone())
//             .map(|e| Stat::Free(e.inner)),
//         just(Token::Keyword(Keyword::Return))
//             .ignore_then(expr.clone())
//             .map(|e| Stat::Return(e.inner)),
//         just(Token::Keyword(Keyword::Exit))
//             .ignore_then(expr.clone())
//             .map(|e| Stat::Exit(e.inner)),
//         just(Token::Keyword(Keyword::Print))
//             .ignore_then(expr.clone())
//             .map(|e| Stat::Print(e.inner)),
//         just(Token::Keyword(Keyword::Println))
//             .ignore_then(expr)
//             .map(|e| Stat::Println(e.inner)),
//         if_then_else,
//         while_do,
//         scoped,
//     ));
//
//     // TODO: figure out labels and error recovery
//
//     stat.map_with_span(|stat, span| Spanned::new(stat, span))
// }
//
// pub fn parser() -> impl Parser<Token, Spanned<Program>, Error = Simple<Token>> + Clone {
//     let r#type = type_parser();
//
//     // statement chain parser
//     let stat_chain = recursive(
//         |stat_chain: Recursive<'_, Token, Spanned<StatChain>, Simple<Token>>| {
//             let stat = stat_parser_factory(stat_chain);
//
//             // parse statement chains
//             let stat_chain = stat
//                 .separated_by(just(Token::Symbol(Symbol::Semicolon)))
//                 .at_least(1)
//                 .map(|stats| {
//                     StatChain::try_new(stats.iter().map(|s| s.inner.clone()).collect()).unwrap()
//                 });
//
//             // map the parsed result to span
//             stat_chain.map_with_span(|st, span| Spanned::new(st, span))
//         },
//     );
//
//     // identifiers can be parsed directly from `Ident` tokens
//     let ident = select! { Token::Ident(s) => Ident(s) }.labelled("<ident>");
//
//     // func param parser
//     let func_param = r#type.clone().then(ident).map(|(t, id)| FuncParam {
//         param_type: t.inner,
//         name: id,
//     });
//
//     // function parser
//     let function = r#type
//         .then(ident)
//         .then(
//             func_param
//                 .separated_by(just(Token::Symbol(Symbol::Comma)))
//                 .delimited_by(
//                     just(Token::Symbol(Symbol::OpenParen)),
//                     just(Token::Symbol(Symbol::CloseParen)),
//                 ),
//         )
//         .then_ignore(just(Token::Keyword(Keyword::Is)))
//         .then(stat_chain.clone())
//         .then_ignore(just(Token::Keyword(Keyword::End)))
//         .map(|(((t, id), params), body)| Func {
//             return_type: t.inner,
//             name: id,
//             params: params.into_boxed_slice(),
//             body: body.inner,
//         });
//
//     // program parser
//     let program = just(Token::Keyword(Keyword::Begin))
//         .ignore_then(function.repeated())
//         .then(stat_chain)
//         .then_ignore(just(Token::Keyword(Keyword::End)))
//         .map(|(funcs, body)| Program {
//             functions: funcs.into_boxed_slice(),
//             body: body.inner,
//         });
//
//     // TODO: figure out error recovery
//
//     // map programme to spanned
//     program.map_with_span(|p, s| Spanned::new(p, s))
// }
