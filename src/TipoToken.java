public enum TipoToken {
    ID,       // letra(letra+digito)*
    NREAL,    // digito+.digito*
    NINT,     // digito+
    NSTRING,  // "qualquer_coisa*"
    TYPE,     // "Int" | "Float" | "Bool"
    READ,     // "getLine"
    WRITE,    // "print"
    ASSIGN,   // "<-"
    IF,       // "if"
    THEN,     // "then"
    ELSE,     // "else"
    OP,       // '+' | '-' | '*' | '/'
    PAREN,    // '(' | ')'
    COLON,    // ':'
    EQUAL,    // '='
    EOF,      // end of file
    UNKNOWN   // qualquer outra coisa
}
