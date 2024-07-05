public enum TipoToken {
    ID,       // identificador (letra(letra+dígito)*)
    NREAL,    // número real (dígito+.dígito*)
    NINT,     // número inteiro (dígito+)
    NSTRING,  // string ("qualquer_coisa*")
    TYPE,     // tipo ("Int" | "Float" | "Bool")
    READ,     // leitura ("getLine")
    WRITE,    // escrita ("print")
    ASSIGN,   // atribuição ("<-" ou "=")
    IF,       // condicional ("if")
    THEN,     // então ("then")
    ELSE,     // senão ("else")
    OP,       // operador ('+' | '-' | '*' | '/')
    PAREN,    // parênteses ('(' | ')')
    COLON,    // dois pontos (':')
    EQUAL,    // igualdade ('=')
    EOF,      // fim do arquivo
    UNKNOWN   // qualquer outra coisa (desconhecido)
}
