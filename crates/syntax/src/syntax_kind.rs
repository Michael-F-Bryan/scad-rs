// Generated by `scad_codegen::syntax::tests`. DO NOT EDIT!

#[doc = r" The different types of terminals and non-terminals in the"]
#[doc = r" OpenSCAD language grammar."]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(nonstandard_style)]
#[non_exhaustive]
pub enum SyntaxKind {
    #[doc = "The `&&` symbol."]
    AND = 0u16,
    #[doc = "The `!` symbol."]
    BANG = 1u16,
    #[doc = "The `^` symbol."]
    CARET = 2u16,
    #[doc = "The `:` symbol."]
    COLON = 3u16,
    #[doc = "The `,` symbol."]
    COMMA = 4u16,
    #[doc = "A comment."]
    COMMENT = 5u16,
    #[doc = "The `.` symbol."]
    DOT = 6u16,
    #[doc = "The `else` keyword."]
    ELSE_KW = 7u16,
    #[doc = "End of input."]
    EOF = 8u16,
    #[doc = "The `=` symbol."]
    EQUALS = 9u16,
    #[doc = "A lexer error."]
    ERROR = 10u16,
    #[doc = "The `false` keyword."]
    FALSE_KW = 11u16,
    #[doc = "The path to a file."]
    FILE = 12u16,
    #[doc = "A float literal."]
    FLOAT = 13u16,
    #[doc = "The `for` keyword."]
    FOR_KW = 14u16,
    #[doc = "The `function` keyword."]
    FUNCTION_KW = 15u16,
    #[doc = "The `>` symbol."]
    GREATER_THAN = 16u16,
    #[doc = "The `>=` symbol."]
    GREATER_THAN_EQUALS = 17u16,
    #[doc = "The `#` symbol."]
    HASH = 18u16,
    #[doc = "An identifier."]
    IDENT = 19u16,
    #[doc = "The `if` keyword."]
    IF_KW = 20u16,
    #[doc = "The `include` keyword."]
    INCLUDE_KW = 21u16,
    #[doc = "An integer literal."]
    INTEGER = 22u16,
    #[doc = "The `<` symbol."]
    LESS_THAN = 23u16,
    #[doc = "The `<=` symbol."]
    LESS_THAN_EQUALS = 24u16,
    #[doc = "The `let` keyword."]
    LET_KW = 25u16,
    #[doc = "The `[` symbol."]
    L_BRACKET = 26u16,
    #[doc = "The `{` symbol."]
    L_CURLY = 27u16,
    #[doc = "The `(` symbol."]
    L_PAREN = 28u16,
    #[doc = "The `-` symbol."]
    MINUS = 29u16,
    #[doc = "The `module` keyword."]
    MODULE_KW = 30u16,
    #[doc = "The `||` symbol."]
    OR = 31u16,
    #[doc = "The `%` symbol."]
    PERCENT = 32u16,
    #[doc = "The `+` symbol."]
    PLUS = 33u16,
    #[doc = "The `?` symbol."]
    QUESTION_MARK = 34u16,
    #[doc = "The `]` symbol."]
    R_BRACKET = 35u16,
    #[doc = "The `}` symbol."]
    R_CURLY = 36u16,
    #[doc = "The `)` symbol."]
    R_PAREN = 37u16,
    #[doc = "The `;` symbol."]
    SEMICOLON = 38u16,
    #[doc = "The `/` symbol."]
    SLASH = 39u16,
    #[doc = "The `*` symbol."]
    STAR = 40u16,
    #[doc = "A string literal."]
    STRING = 41u16,
    #[doc = "The `true` keyword."]
    TRUE_KW = 42u16,
    #[doc = "The `undef` keyword."]
    UNDEF_KW = 43u16,
    #[doc = "The `use` keyword."]
    USE_KW = 44u16,
    #[doc = "One or more whitespace characters (spaces, tabs, newlines, etc.)."]
    WHITESPACE = 45u16,
    PACKAGE = 46u16,
    INCLUDE = 47u16,
    USE = 48u16,
    ASSIGNMENT_STATEMENT = 49u16,
    NAMED_FUNCTION_DEFINITION = 50u16,
    NAMED_MODULE_DEFINITION = 51u16,
    MODULE_INSTANTIATION = 52u16,
    IF_STATEMENT = 53u16,
    BRACED_ACTIONS = 54u16,
    ASSIGNMENT = 55u16,
    EXPRESSIONS = 56u16,
    LIST_EXPR = 57u16,
    UNARY_EXPR = 58u16,
    TERNARY_EXPR = 59u16,
    PAREN_EXPR = 60u16,
    LIST_COMPREHENSION_EXPR = 61u16,
    LET_CLAUSE = 62u16,
    BIN_EXPR = 63u16,
    LOOKUP_EXPR = 64u16,
    INDEX_EXPR = 65u16,
    FUNCTION_CALL = 66u16,
    RANGE_EXPRESSION_FROM_TO = 67u16,
    RANGE_EXPRESSION_FROM_TO_STEP = 68u16,
    FOR_CLAUSE = 69u16,
    IF_CLAUSE = 70u16,
    ASSIGNMENTS_OPT = 71u16,
    ASSIGNMENTS = 72u16,
    PARAMETERS = 73u16,
    ARGUMENTS = 74u16,
    CHILDREN = 75u16,
    BRACED_CHILDREN = 76u16,
}
impl SyntaxKind {
    #[doc = r" All the possible [`SyntaxKind`] variants."]
    pub const VARIANTS: [SyntaxKind; 77usize] = [
        SyntaxKind::AND,
        SyntaxKind::BANG,
        SyntaxKind::CARET,
        SyntaxKind::COLON,
        SyntaxKind::COMMA,
        SyntaxKind::COMMENT,
        SyntaxKind::DOT,
        SyntaxKind::ELSE_KW,
        SyntaxKind::EOF,
        SyntaxKind::EQUALS,
        SyntaxKind::ERROR,
        SyntaxKind::FALSE_KW,
        SyntaxKind::FILE,
        SyntaxKind::FLOAT,
        SyntaxKind::FOR_KW,
        SyntaxKind::FUNCTION_KW,
        SyntaxKind::GREATER_THAN,
        SyntaxKind::GREATER_THAN_EQUALS,
        SyntaxKind::HASH,
        SyntaxKind::IDENT,
        SyntaxKind::IF_KW,
        SyntaxKind::INCLUDE_KW,
        SyntaxKind::INTEGER,
        SyntaxKind::LESS_THAN,
        SyntaxKind::LESS_THAN_EQUALS,
        SyntaxKind::LET_KW,
        SyntaxKind::L_BRACKET,
        SyntaxKind::L_CURLY,
        SyntaxKind::L_PAREN,
        SyntaxKind::MINUS,
        SyntaxKind::MODULE_KW,
        SyntaxKind::OR,
        SyntaxKind::PERCENT,
        SyntaxKind::PLUS,
        SyntaxKind::QUESTION_MARK,
        SyntaxKind::R_BRACKET,
        SyntaxKind::R_CURLY,
        SyntaxKind::R_PAREN,
        SyntaxKind::SEMICOLON,
        SyntaxKind::SLASH,
        SyntaxKind::STAR,
        SyntaxKind::STRING,
        SyntaxKind::TRUE_KW,
        SyntaxKind::UNDEF_KW,
        SyntaxKind::USE_KW,
        SyntaxKind::WHITESPACE,
        SyntaxKind::PACKAGE,
        SyntaxKind::INCLUDE,
        SyntaxKind::USE,
        SyntaxKind::ASSIGNMENT_STATEMENT,
        SyntaxKind::NAMED_FUNCTION_DEFINITION,
        SyntaxKind::NAMED_MODULE_DEFINITION,
        SyntaxKind::MODULE_INSTANTIATION,
        SyntaxKind::IF_STATEMENT,
        SyntaxKind::BRACED_ACTIONS,
        SyntaxKind::ASSIGNMENT,
        SyntaxKind::EXPRESSIONS,
        SyntaxKind::LIST_EXPR,
        SyntaxKind::UNARY_EXPR,
        SyntaxKind::TERNARY_EXPR,
        SyntaxKind::PAREN_EXPR,
        SyntaxKind::LIST_COMPREHENSION_EXPR,
        SyntaxKind::LET_CLAUSE,
        SyntaxKind::BIN_EXPR,
        SyntaxKind::LOOKUP_EXPR,
        SyntaxKind::INDEX_EXPR,
        SyntaxKind::FUNCTION_CALL,
        SyntaxKind::RANGE_EXPRESSION_FROM_TO,
        SyntaxKind::RANGE_EXPRESSION_FROM_TO_STEP,
        SyntaxKind::FOR_CLAUSE,
        SyntaxKind::IF_CLAUSE,
        SyntaxKind::ASSIGNMENTS_OPT,
        SyntaxKind::ASSIGNMENTS,
        SyntaxKind::PARAMETERS,
        SyntaxKind::ARGUMENTS,
        SyntaxKind::CHILDREN,
        SyntaxKind::BRACED_CHILDREN,
    ];
    #[doc = r" Is this [`SyntaxKind`] a piece of punctuation?"]
    #[doc = r""]
    #[doc = r" ```rust"]
    #[doc = r" # use scad_syntax::T;"]
    #[doc = r" let kind = T![;];"]
    #[doc = r" assert!(kind.is_punctuation())"]
    #[doc = r" ```"]
    pub const fn is_punctuation(self) -> bool {
        matches!(
            self,
            SyntaxKind::AND
                | SyntaxKind::BANG
                | SyntaxKind::CARET
                | SyntaxKind::COLON
                | SyntaxKind::COMMA
                | SyntaxKind::DOT
                | SyntaxKind::EQUALS
                | SyntaxKind::GREATER_THAN
                | SyntaxKind::GREATER_THAN_EQUALS
                | SyntaxKind::HASH
                | SyntaxKind::LESS_THAN
                | SyntaxKind::LESS_THAN_EQUALS
                | SyntaxKind::L_BRACKET
                | SyntaxKind::L_CURLY
                | SyntaxKind::L_PAREN
                | SyntaxKind::MINUS
                | SyntaxKind::OR
                | SyntaxKind::PERCENT
                | SyntaxKind::PLUS
                | SyntaxKind::QUESTION_MARK
                | SyntaxKind::R_BRACKET
                | SyntaxKind::R_CURLY
                | SyntaxKind::R_PAREN
                | SyntaxKind::SEMICOLON
                | SyntaxKind::SLASH
                | SyntaxKind::STAR
        )
    }
    #[doc = r" Is this [`SyntaxKind`] a keyword?"]
    #[doc = r""]
    #[doc = r" ```rust"]
    #[doc = r" # use scad_syntax::T;"]
    #[doc = r" let kind = T![function];"]
    #[doc = r" assert!(kind.is_keyword())"]
    #[doc = r" ```"]
    pub const fn is_keyword(self) -> bool {
        matches!(
            self,
            SyntaxKind::ELSE_KW
                | SyntaxKind::FALSE_KW
                | SyntaxKind::FOR_KW
                | SyntaxKind::FUNCTION_KW
                | SyntaxKind::IF_KW
                | SyntaxKind::INCLUDE_KW
                | SyntaxKind::LET_KW
                | SyntaxKind::MODULE_KW
                | SyntaxKind::TRUE_KW
                | SyntaxKind::UNDEF_KW
                | SyntaxKind::USE_KW
        )
    }
    #[doc = r" Given a textual symbol try to get the associated"]
    #[doc = r" [`SyntaxKind`]."]
    #[doc = r""]
    #[doc = r" # Examples"]
    #[doc = r""]
    #[doc = r" ```rust"]
    #[doc = r" # use scad_syntax::SyntaxKind;"]
    #[doc = r#" let plus = SyntaxKind::from_symbol("+").unwrap();"#]
    #[doc = r" assert_eq!(plus, SyntaxKind::PLUS);"]
    #[doc = r" ```"]
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "&&" => Some(SyntaxKind::AND),
            "!" => Some(SyntaxKind::BANG),
            "^" => Some(SyntaxKind::CARET),
            ":" => Some(SyntaxKind::COLON),
            "," => Some(SyntaxKind::COMMA),
            "." => Some(SyntaxKind::DOT),
            "=" => Some(SyntaxKind::EQUALS),
            ">" => Some(SyntaxKind::GREATER_THAN),
            ">=" => Some(SyntaxKind::GREATER_THAN_EQUALS),
            "#" => Some(SyntaxKind::HASH),
            "<" => Some(SyntaxKind::LESS_THAN),
            "<=" => Some(SyntaxKind::LESS_THAN_EQUALS),
            "[" => Some(SyntaxKind::L_BRACKET),
            "{" => Some(SyntaxKind::L_CURLY),
            "(" => Some(SyntaxKind::L_PAREN),
            "-" => Some(SyntaxKind::MINUS),
            "||" => Some(SyntaxKind::OR),
            "%" => Some(SyntaxKind::PERCENT),
            "+" => Some(SyntaxKind::PLUS),
            "?" => Some(SyntaxKind::QUESTION_MARK),
            "]" => Some(SyntaxKind::R_BRACKET),
            "}" => Some(SyntaxKind::R_CURLY),
            ")" => Some(SyntaxKind::R_PAREN),
            ";" => Some(SyntaxKind::SEMICOLON),
            "/" => Some(SyntaxKind::SLASH),
            "*" => Some(SyntaxKind::STAR),
            _ => None,
        }
    }
    #[doc = r" get the [`SyntaxKind`] that corresponds to a particular"]
    #[doc = r" variant."]
    #[doc = r""]
    #[doc = r" ```rust"]
    #[doc = r" # use scad_syntax::SyntaxKind;"]
    #[doc = r" let kind = SyntaxKind::IDENT;"]
    #[doc = r" let code: u16 = kind.into();"]
    #[doc = r""]
    #[doc = r" let round_tripped = SyntaxKind::from_code(code).unwrap();"]
    #[doc = r""]
    #[doc = r" assert_eq!(round_tripped, kind);"]
    #[doc = r" ```"]
    pub fn from_code(n: u16) -> Option<Self> {
        match n {
            0u16 => Some(SyntaxKind::AND),
            1u16 => Some(SyntaxKind::BANG),
            2u16 => Some(SyntaxKind::CARET),
            3u16 => Some(SyntaxKind::COLON),
            4u16 => Some(SyntaxKind::COMMA),
            5u16 => Some(SyntaxKind::COMMENT),
            6u16 => Some(SyntaxKind::DOT),
            7u16 => Some(SyntaxKind::ELSE_KW),
            8u16 => Some(SyntaxKind::EOF),
            9u16 => Some(SyntaxKind::EQUALS),
            10u16 => Some(SyntaxKind::ERROR),
            11u16 => Some(SyntaxKind::FALSE_KW),
            12u16 => Some(SyntaxKind::FILE),
            13u16 => Some(SyntaxKind::FLOAT),
            14u16 => Some(SyntaxKind::FOR_KW),
            15u16 => Some(SyntaxKind::FUNCTION_KW),
            16u16 => Some(SyntaxKind::GREATER_THAN),
            17u16 => Some(SyntaxKind::GREATER_THAN_EQUALS),
            18u16 => Some(SyntaxKind::HASH),
            19u16 => Some(SyntaxKind::IDENT),
            20u16 => Some(SyntaxKind::IF_KW),
            21u16 => Some(SyntaxKind::INCLUDE_KW),
            22u16 => Some(SyntaxKind::INTEGER),
            23u16 => Some(SyntaxKind::LESS_THAN),
            24u16 => Some(SyntaxKind::LESS_THAN_EQUALS),
            25u16 => Some(SyntaxKind::LET_KW),
            26u16 => Some(SyntaxKind::L_BRACKET),
            27u16 => Some(SyntaxKind::L_CURLY),
            28u16 => Some(SyntaxKind::L_PAREN),
            29u16 => Some(SyntaxKind::MINUS),
            30u16 => Some(SyntaxKind::MODULE_KW),
            31u16 => Some(SyntaxKind::OR),
            32u16 => Some(SyntaxKind::PERCENT),
            33u16 => Some(SyntaxKind::PLUS),
            34u16 => Some(SyntaxKind::QUESTION_MARK),
            35u16 => Some(SyntaxKind::R_BRACKET),
            36u16 => Some(SyntaxKind::R_CURLY),
            37u16 => Some(SyntaxKind::R_PAREN),
            38u16 => Some(SyntaxKind::SEMICOLON),
            39u16 => Some(SyntaxKind::SLASH),
            40u16 => Some(SyntaxKind::STAR),
            41u16 => Some(SyntaxKind::STRING),
            42u16 => Some(SyntaxKind::TRUE_KW),
            43u16 => Some(SyntaxKind::UNDEF_KW),
            44u16 => Some(SyntaxKind::USE_KW),
            45u16 => Some(SyntaxKind::WHITESPACE),
            46u16 => Some(SyntaxKind::PACKAGE),
            47u16 => Some(SyntaxKind::INCLUDE),
            48u16 => Some(SyntaxKind::USE),
            49u16 => Some(SyntaxKind::ASSIGNMENT_STATEMENT),
            50u16 => Some(SyntaxKind::NAMED_FUNCTION_DEFINITION),
            51u16 => Some(SyntaxKind::NAMED_MODULE_DEFINITION),
            52u16 => Some(SyntaxKind::MODULE_INSTANTIATION),
            53u16 => Some(SyntaxKind::IF_STATEMENT),
            54u16 => Some(SyntaxKind::BRACED_ACTIONS),
            55u16 => Some(SyntaxKind::ASSIGNMENT),
            56u16 => Some(SyntaxKind::EXPRESSIONS),
            57u16 => Some(SyntaxKind::LIST_EXPR),
            58u16 => Some(SyntaxKind::UNARY_EXPR),
            59u16 => Some(SyntaxKind::TERNARY_EXPR),
            60u16 => Some(SyntaxKind::PAREN_EXPR),
            61u16 => Some(SyntaxKind::LIST_COMPREHENSION_EXPR),
            62u16 => Some(SyntaxKind::LET_CLAUSE),
            63u16 => Some(SyntaxKind::BIN_EXPR),
            64u16 => Some(SyntaxKind::LOOKUP_EXPR),
            65u16 => Some(SyntaxKind::INDEX_EXPR),
            66u16 => Some(SyntaxKind::FUNCTION_CALL),
            67u16 => Some(SyntaxKind::RANGE_EXPRESSION_FROM_TO),
            68u16 => Some(SyntaxKind::RANGE_EXPRESSION_FROM_TO_STEP),
            69u16 => Some(SyntaxKind::FOR_CLAUSE),
            70u16 => Some(SyntaxKind::IF_CLAUSE),
            71u16 => Some(SyntaxKind::ASSIGNMENTS_OPT),
            72u16 => Some(SyntaxKind::ASSIGNMENTS),
            73u16 => Some(SyntaxKind::PARAMETERS),
            74u16 => Some(SyntaxKind::ARGUMENTS),
            75u16 => Some(SyntaxKind::CHILDREN),
            76u16 => Some(SyntaxKind::BRACED_CHILDREN),
            _ => None,
        }
    }
}
impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(k: rowan::SyntaxKind) -> Self {
        SyntaxKind::from_code(k.0).unwrap()
    }
}
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(k: SyntaxKind) -> Self {
        rowan::SyntaxKind(k.into())
    }
}
impl From<SyntaxKind> for u16 {
    fn from(k: SyntaxKind) -> Self {
        k as u16
    }
}
impl From<SyntaxKind> for m_lexer::TokenKind {
    fn from(k: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(k.into())
    }
}
#[doc = r" A helper macro for getting the [`SyntaxKind`] that corresponds"]
#[doc = r" to a particular token."]
#[macro_export]
macro_rules! T {
    (&&) => {
        $crate::SyntaxKind::AND
    };
    (!) => {
        $crate::SyntaxKind::BANG
    };
    (^) => {
        $crate::SyntaxKind::CARET
    };
    (:) => {
        $crate::SyntaxKind::COLON
    };
    (,) => {
        $crate::SyntaxKind::COMMA
    };
    (.) => {
        $crate::SyntaxKind::DOT
    };
    (else) => {
        $crate::SyntaxKind::ELSE_KW
    };
    (=) => {
        $crate::SyntaxKind::EQUALS
    };
    (false) => {
        $crate::SyntaxKind::FALSE_KW
    };
    (for) => {
        $crate::SyntaxKind::FOR_KW
    };
    (function) => {
        $crate::SyntaxKind::FUNCTION_KW
    };
    (>) => {
        $crate::SyntaxKind::GREATER_THAN
    };
    (>=) => {
        $crate::SyntaxKind::GREATER_THAN_EQUALS
    };
    (#) => {
        $crate::SyntaxKind::HASH
    };
    (if) => {
        $crate::SyntaxKind::IF_KW
    };
    (include) => {
        $crate::SyntaxKind::INCLUDE_KW
    };
    (<) => {
        $crate::SyntaxKind::LESS_THAN
    };
    (<=) => {
        $crate::SyntaxKind::LESS_THAN_EQUALS
    };
    (let) => {
        $crate::SyntaxKind::LET_KW
    };
    ("[") => {
        $crate::SyntaxKind::L_BRACKET
    };
    ("{") => {
        $crate::SyntaxKind::L_CURLY
    };
    ("(") => {
        $crate::SyntaxKind::L_PAREN
    };
    (-) => {
        $crate::SyntaxKind::MINUS
    };
    (module) => {
        $crate::SyntaxKind::MODULE_KW
    };
    (||) => {
        $crate::SyntaxKind::OR
    };
    (%) => {
        $crate::SyntaxKind::PERCENT
    };
    (+) => {
        $crate::SyntaxKind::PLUS
    };
    (?) => {
        $crate::SyntaxKind::QUESTION_MARK
    };
    ("]") => {
        $crate::SyntaxKind::R_BRACKET
    };
    ("}") => {
        $crate::SyntaxKind::R_CURLY
    };
    (")") => {
        $crate::SyntaxKind::R_PAREN
    };
    (;) => {
        $crate::SyntaxKind::SEMICOLON
    };
    (/) => {
        $crate::SyntaxKind::SLASH
    };
    (*) => {
        $crate::SyntaxKind::STAR
    };
    (true) => {
        $crate::SyntaxKind::TRUE_KW
    };
    (undef) => {
        $crate::SyntaxKind::UNDEF_KW
    };
    (use) => {
        $crate::SyntaxKind::USE_KW
    };
}
