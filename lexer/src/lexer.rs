use logos::{Lexer as LogosLexer, Source, Span};
use pre_token::{PreToken, TypeSizeAndFlag};
use token::{Token, TypeSize};

#[derive(Clone)]
pub struct Lexer<'source> {
    lex: LogosLexer<'source, PreToken>,
    pub extras: TypeSize,
}

impl<'source> Lexer<'source> {
    /// Create a new `Lexer`.
    pub fn new(source: &'source str) -> Self {
        Self::with_extras(source, TypeSize::default())
    }

    /// Create a new `Lexer` with the provided `Extras`.
    pub fn with_extras(source: &'source str, extras: TypeSize) -> Self {
        Self {
            lex: LogosLexer::with_extras(source, TypeSizeAndFlag(extras.0, extras.1, false)),
            extras: extras,
        }
    }

    /// Source from which this Lexer is reading tokens.
    #[inline]
    pub fn source(&self) -> &'source str {
        self.lex.source()
    }

    /// Get the range for the current token in `Source`.
    #[inline]
    pub fn span(&self) -> Span {
        self.lex.span()
    }

    /// Get a string slice of the current token.
    #[inline]
    pub fn slice(&self) -> &'source <str as Source>::Slice {
        self.lex.slice()
    }

    /// Get a slice of remaining source, starting at the end of current token.
    #[inline]
    pub fn remainder(&self) -> &'source <str as Source>::Slice {
        self.lex.remainder()
    }

    /// Bumps the end of currently lexed token by `n` bytes.
    ///
    /// # Panics
    ///
    /// Panics if adding `n` to current offset would place the `Lexer` beyond the last byte,
    /// or in the middle of an UTF-8 code point.
    pub fn bump(&mut self, n: usize) {
        self.lex.bump(n)
    }

    fn parse_pre_token(&self, pre_token: PreToken) -> Option<Token> {
        match pre_token {
            PreToken::Semicolon => Some(Token::Semicolon),
            PreToken::Colon => Some(Token::Colon),
            PreToken::Comma => Some(Token::Comma),
            PreToken::Accessor => Some(Token::Accessor),
            PreToken::ParenOpen => Some(Token::ParenOpen),
            PreToken::ParenClose => Some(Token::ParenClose),
            PreToken::BraceOpen => Some(Token::BraceOpen),
            PreToken::BraceClose => Some(Token::BraceClose),
            PreToken::BracketOpen => Some(Token::BracketOpen),
            PreToken::BracketClose => Some(Token::BracketClose),
            PreToken::Arrow => Some(Token::Arrow),
            PreToken::Identifier => Some(Token::Identifier),
            PreToken::IdentifierBuiltin => Some(Token::IdentifierBuiltin),
            PreToken::DeclarationContract => Some(Token::DeclarationContract),
            PreToken::DeclarationLibrary => Some(Token::DeclarationLibrary),
            PreToken::DeclarationInterface => Some(Token::DeclarationInterface),
            PreToken::DeclarationEnum => Some(Token::DeclarationEnum),
            PreToken::DeclarationStruct => Some(Token::DeclarationStruct),
            PreToken::DeclarationModifier => Some(Token::DeclarationModifier),
            PreToken::DeclarationEvent => Some(Token::DeclarationEvent),
            PreToken::DeclarationFunction => Some(Token::DeclarationFunction),
            PreToken::DeclarationVar => Some(Token::DeclarationVar),
            PreToken::KeywordAnonymous => Some(Token::KeywordAnonymous),
            PreToken::KeywordAs => Some(Token::KeywordAs),
            PreToken::KeywordAssembly => Some(Token::KeywordAssembly),
            PreToken::KeywordBreak => Some(Token::KeywordBreak),
            PreToken::KeywordConstant => Some(Token::KeywordConstant),
            PreToken::KeywordContinue => Some(Token::KeywordContinue),
            PreToken::KeywordDo => Some(Token::KeywordDo),
            PreToken::KeywordDelete => Some(Token::KeywordDelete),
            PreToken::KeywordElse => Some(Token::KeywordElse),
            PreToken::KeywordExternal => Some(Token::KeywordExternal),
            PreToken::KeywordFor => Some(Token::KeywordFor),
            PreToken::KeywordHex => Some(Token::KeywordHex),
            PreToken::KeywordIf => Some(Token::KeywordIf),
            PreToken::KeywordIndexed => Some(Token::KeywordIndexed),
            PreToken::KeywordInternal => Some(Token::KeywordInternal),
            PreToken::KeywordImport => Some(Token::KeywordImport),
            PreToken::KeywordIs => Some(Token::KeywordIs),
            PreToken::KeywordMapping => Some(Token::KeywordMapping),
            PreToken::KeywordMemory => Some(Token::KeywordMemory),
            PreToken::KeywordNew => Some(Token::KeywordNew),
            PreToken::KeywordPayable => Some(Token::KeywordPayable),
            PreToken::KeywordPublic => Some(Token::KeywordPublic),
            PreToken::KeywordPragma => Some(Token::KeywordPragma),
            PreToken::KeywordPrivate => Some(Token::KeywordPrivate),
            PreToken::KeywordPure => Some(Token::KeywordPure),
            PreToken::KeywordReturn => Some(Token::KeywordReturn),
            PreToken::KeywordReturns => Some(Token::KeywordReturns),
            PreToken::KeywordStorage => Some(Token::KeywordStorage),
            PreToken::KeywordSuper => Some(Token::KeywordSuper),
            PreToken::KeywordThis => Some(Token::KeywordThis),
            PreToken::KeywordThrow => Some(Token::KeywordThrow),
            PreToken::KeywordUsing => Some(Token::KeywordUsing),
            PreToken::KeywordView => Some(Token::KeywordView),
            PreToken::KeywordWhile => Some(Token::KeywordWhile),
            PreToken::ReservedWord => Some(Token::ReservedWord),
            PreToken::TypeBool => Some(Token::TypeBool),
            PreToken::TypeAddress => Some(Token::TypeAddress),
            PreToken::TypeString => Some(Token::TypeString),
            PreToken::TypeByte => Some(Token::TypeByte),
            PreToken::TypeBytes => Some(Token::TypeBytes),
            PreToken::TypeInt => Some(Token::TypeInt),
            PreToken::TypeUint => Some(Token::TypeUint),
            PreToken::TypeFixedOrIdentifier => {
                if self.lex.extras.2 {
                    Some(Token::Identifier)
                } else {
                    Some(Token::TypeFixed)
                }
            }
            PreToken::TypeUfixedOrIdentifier => {
                if self.lex.extras.2 {
                    Some(Token::Identifier)
                } else {
                    Some(Token::TypeUfixed)
                }
            }
            PreToken::LiteralTrue => Some(Token::LiteralTrue),
            PreToken::LiteralFalse => Some(Token::LiteralFalse),
            PreToken::LiteralHex => Some(Token::LiteralHex),
            PreToken::LiteralRationalOrLiteralInteger => {
                if self.lex.extras.2 {
                    Some(Token::LiteralInteger)
                } else {
                    Some(Token::LiteralRational)
                }
            }
            PreToken::LiteralString => Some(Token::LiteralString),
            PreToken::UnitEther => Some(Token::UnitEther),
            PreToken::UnitFinney => Some(Token::UnitFinney),
            PreToken::UnitSzabo => Some(Token::UnitSzabo),
            PreToken::UnitWei => Some(Token::UnitWei),
            PreToken::UnitTimeYears => Some(Token::UnitTimeYears),
            PreToken::UnitTimeWeeks => Some(Token::UnitTimeWeeks),
            PreToken::UnitTimeDays => Some(Token::UnitTimeDays),
            PreToken::UnitTimeHours => Some(Token::UnitTimeHours),
            PreToken::UnitTimeMinutes => Some(Token::UnitTimeMinutes),
            PreToken::UnitTimeSeconds => Some(Token::UnitTimeSeconds),
            PreToken::AssemblyBind => Some(Token::AssemblyBind),
            PreToken::AssemblyAssign => Some(Token::AssemblyAssign),
            PreToken::OperatorIncrement => Some(Token::OperatorIncrement),
            PreToken::OperatorDecrement => Some(Token::OperatorDecrement),
            PreToken::OperatorLogicalNot => Some(Token::OperatorLogicalNot),
            PreToken::OperatorBitNot => Some(Token::OperatorBitNot),
            PreToken::OperatorMultiplication => Some(Token::OperatorMultiplication),
            PreToken::OperatorDivision => Some(Token::OperatorDivision),
            PreToken::OperatorRemainder => Some(Token::OperatorRemainder),
            PreToken::OperatorExponent => Some(Token::OperatorExponent),
            PreToken::OperatorAddition => Some(Token::OperatorAddition),
            PreToken::OperatorSubtraction => Some(Token::OperatorSubtraction),
            PreToken::OperatorBitShiftLeft => Some(Token::OperatorBitShiftLeft),
            PreToken::OperatorBitShiftRight => Some(Token::OperatorBitShiftRight),
            PreToken::OperatorLesser => Some(Token::OperatorLesser),
            PreToken::OperatorLesserEquals => Some(Token::OperatorLesserEquals),
            PreToken::OperatorGreater => Some(Token::OperatorGreater),
            PreToken::OperatorGreaterEquals => Some(Token::OperatorGreaterEquals),
            PreToken::OperatorEquality => Some(Token::OperatorEquality),
            PreToken::OperatorInequality => Some(Token::OperatorInequality),
            PreToken::OperatorBitAnd => Some(Token::OperatorBitAnd),
            PreToken::OperatorBitXor => Some(Token::OperatorBitXor),
            PreToken::OperatorBitOr => Some(Token::OperatorBitOr),
            PreToken::OperatorLogicalAnd => Some(Token::OperatorLogicalAnd),
            PreToken::OperatorLogicalOr => Some(Token::OperatorLogicalOr),
            PreToken::OperatorConditional => Some(Token::OperatorConditional),
            PreToken::Assign => Some(Token::Assign),
            PreToken::AssignAddition => Some(Token::AssignAddition),
            PreToken::AssignSubtraction => Some(Token::AssignSubtraction),
            PreToken::AssignMultiplication => Some(Token::AssignMultiplication),
            PreToken::AssignDivision => Some(Token::AssignDivision),
            PreToken::AssignRemainder => Some(Token::AssignRemainder),
            PreToken::AssignBitShiftLeft => Some(Token::AssignBitShiftLeft),
            PreToken::AssignBitShiftRight => Some(Token::AssignBitShiftRight),
            PreToken::AssignBitAnd => Some(Token::AssignBitAnd),
            PreToken::AssignBitXor => Some(Token::AssignBitXor),
            PreToken::AssignBitOr => Some(Token::AssignBitOr),
            PreToken::Comment => None,
            PreToken::UnexpectedToken => Some(Token::UnexpectedToken),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Token> {
        loop {
            match self.lex.next() {
                None => return None,
                Some(pre_token) => {
                    match self.parse_pre_token(pre_token) {
                        None => {
                            // underlying pre_token is ignored by the Lexer
                            continue
                        },
                        Some(token) => {
                            // TODO: replace extras to tokens with corresponding fields
                            self.extras.0 = self.lex.extras.0;
                            self.extras.1 = self.lex.extras.1;
                            return Some(token)
                        }
                    }
                }
            }
        }
    }
}
