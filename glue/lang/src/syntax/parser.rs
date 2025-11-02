use miette::Report;
use pest::{Parser as PestParser, iterators::Pair};

use crate::metadata::SourceCodeMetadata;

#[derive(pest_derive::Parser)]
#[grammar = "syntax/grammar.pest"]
struct P;

#[allow(clippy::upper_case_acronyms)]
#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum LSyntaxKind {
    // Tokens
    STRING_LITERAL,
    STRING_LITERAL_INNER,
    CHAR,
    TRUE_LITERAL,
    FALSE_LITERAL,
    BOOL_LITERAL,
    LITERAL,
    BUILTIN_TYPE,
    DOC_COMMENT,

    // Nodes
    TYPE_ATOM,
    DOC_BLOCK,
    DOC_BLOCK_LINE,
    DOC_CONTENT,
    TYPE_ATOM_MODIFIERS,
    IDENT,
    TYPE,
    DECORATOR_NAMED_ARG,
    DECORATOR_POSITIONAL_ARG,
    DECORATOR_ARGS,
    DECORATOR,
    FIELD_NAME,
    FIELD,
    FIELD_DEFAULT_VALUE,
    ENUM_VARIANT,
    ENUM_VARIANTS,
    ENUM,
    MODEL_BODY,
    MODEL,
    ANON_MODEL,
    ENDPOINT,
    PROGRAM,

    // Whitespace/comments
    TRIVIA,

    ERROR,
}

impl From<u16> for LSyntaxKind {
    fn from(v: u16) -> Self {
        unsafe { std::mem::transmute(v) }
    }
}
impl From<LSyntaxKind> for u16 {
    fn from(k: LSyntaxKind) -> u16 {
        k as u16
    }
}

impl From<LSyntaxKind> for rowan::SyntaxKind {
    fn from(k: LSyntaxKind) -> Self {
        rowan::SyntaxKind(k as u16)
    }
}
impl From<rowan::SyntaxKind> for LSyntaxKind {
    fn from(k: rowan::SyntaxKind) -> Self {
        unsafe { std::mem::transmute(k.0) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lang;

impl rowan::Language for Lang {
    type Kind = LSyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> LSyntaxKind {
        raw.0.into()
    }
    fn kind_to_raw(kind: LSyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}

pub type LNode = rowan::SyntaxNode<Lang>;
pub type LToken = rowan::SyntaxToken<Lang>;

pub struct Parser;

#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub ast_root: LNode,
}

#[derive(Debug)]
pub enum ParserError {
    GeneralError(Report),
}

impl ParserError {
    pub fn report(&self) -> &Report {
        match self {
            ParserError::GeneralError(report) => report,
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self
    }

    pub fn parse(&self, metadata: &SourceCodeMetadata) -> Result<ParsedProgram, ParserError> {
        let src = &metadata.file_contents;
        let root = Self::parse_to_rowan(src)?;
        Ok(ParsedProgram { ast_root: root })
    }

    fn map_rule(r: Rule) -> LSyntaxKind {
        use LSyntaxKind::*;

        match r {
            Rule::program => PROGRAM,
            Rule::model => MODEL,
            Rule::anon_model => ANON_MODEL,
            Rule::model_body => MODEL_BODY,
            Rule::enum_def => ENUM,
            Rule::enum_variants => ENUM_VARIANTS,
            Rule::enum_variant => ENUM_VARIANT,
            Rule::field => FIELD,
            Rule::field_name => FIELD_NAME,
            Rule::field_default_value => FIELD_DEFAULT_VALUE,
            Rule::decorator => DECORATOR,
            Rule::decorator_args => DECORATOR_ARGS,
            Rule::decorator_named_arg => DECORATOR_NAMED_ARG,
            Rule::decorator_positional_arg => DECORATOR_POSITIONAL_ARG,
            Rule::type_expr => TYPE,
            Rule::type_atom => TYPE_ATOM,
            Rule::type_atom_modifiers => TYPE_ATOM_MODIFIERS,
            Rule::endpoint => ENDPOINT,
            Rule::ident => IDENT,
            Rule::builtin_type => BUILTIN_TYPE,
            Rule::literal => LITERAL,
            Rule::bool_literal => BOOL_LITERAL,
            Rule::true_literal => TRUE_LITERAL,
            Rule::false_literal => FALSE_LITERAL,
            Rule::string_literal => STRING_LITERAL,
            Rule::string_literal_inner => STRING_LITERAL_INNER,
            Rule::char => CHAR,
            Rule::doc_block => DOC_BLOCK,
            Rule::doc_block_line => DOC_BLOCK_LINE,
            _ => ERROR,
        }
    }

    fn parse_to_rowan(src: &str) -> Result<rowan::SyntaxNode<Lang>, ParserError> {
        let pairs = P::parse(Rule::program, src).map_err(|e| ParserError::GeneralError(e.into_miette().into()))?;

        let mut b = rowan::GreenNodeBuilder::new();
        b.start_node(LSyntaxKind::PROGRAM.into());

        let mut pos = 0;
        for p in pairs {
            Self::build_pair(&mut b, src, &mut pos, p);
        }
        b.finish_node();

        Ok(rowan::SyntaxNode::new_root(b.finish()))
    }

    fn build_pair(b: &mut rowan::GreenNodeBuilder, src: &str, pos: &mut usize, pair: Pair<Rule>) {
        use LSyntaxKind::TRIVIA;

        let span = pair.as_span();
        let (start, end) = (span.start(), span.end());

        // emit exact gap before this pair (handles silent/optional rules)
        if *pos < start {
            b.token(TRIVIA.into(), &src[*pos..start]);
        }

        let kind = Self::map_rule(pair.as_rule());
        let inner = pair.clone().into_inner();

        if inner.clone().next().is_none() {
            // leaf = actual text slice; keep real offsets
            b.token(kind.into(), &src[start..end]);
            *pos = end;
            return;
        }

        // composite node — process children in byte order
        b.start_node(kind.into());
        let mut subpos = start;
        for ch in inner {
            Self::build_pair(b, src, &mut subpos, ch);
        }

        // trailing trivia within this node
        if subpos < end {
            b.token(TRIVIA.into(), &src[subpos..end]);
        }

        *pos = end;
        b.finish_node();
    }
}
