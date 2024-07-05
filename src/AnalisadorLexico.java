import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class AnalisadorLexico {
    private String entrada;   // String que contém o código-fonte de entrada
    private int pos;          // Posição atual de leitura na string de entrada
    private int tamanho;      // Tamanho total da string de entrada

    // Construtor que inicializa o analisador léxico com a entrada fornecida
    public AnalisadorLexico(String entrada) {
        this.entrada = entrada;
        this.pos = 0;  // Inicialmente, começa na posição zero
        this.tamanho = entrada.length();  // Tamanho total da entrada
    }

    // Método para ler e retornar o próximo token da entrada
    public Token leToken() {
        // Verifica se a posição atual é igual ou ultrapassa o tamanho da entrada
        if (pos >= tamanho) {
            // Se sim, retorna um token de fim de arquivo (EOF)
            return new Token(TipoToken.EOF, "");
        }

        char charAtual = entrada.charAt(pos);  // Obtém o caractere atual na posição 'pos'

        // Ignora espaços em branco e quebras de linha
        while (Character.isWhitespace(charAtual)) {
            pos++;  // Avança para o próximo caractere
            // Verifica se atingiu o final da entrada
            if (pos >= tamanho) {
                return new Token(TipoToken.EOF, "");  // Retorna EOF se acabou a entrada
            }
            charAtual = entrada.charAt(pos);  // Obtém o próximo caractere atualizado
        }

        // Ignora comentários iniciados com "--"
        if (charAtual == '-' && pos + 1 < tamanho && entrada.charAt(pos + 1) == '-') {
            // Avança até encontrar o fim da linha (quebra de linha)
            while (pos < tamanho && entrada.charAt(pos) != '\n') {
                pos++;  // Avança para o próximo caractere
            }
            pos++;  // Pula a quebra de linha
            return leToken();  // Chama recursivamente para continuar a leitura
        }

        int inicio = pos;  // Marca o início do token atual na posição 'pos'

        // Verifica se o caractere atual é uma letra (para identificadores e palavras-chave)
        if (Character.isLetter(charAtual)) {
            // Avança enquanto o caractere atual for letra ou dígito
            while (pos < tamanho && Character.isLetterOrDigit(entrada.charAt(pos))) {
                pos++;  // Avança para o próximo caractere
            }
            // Obtém o valor completo do token
            String valor = entrada.substring(inicio, pos);
            // Verifica se é uma palavra-chave conhecida e retorna o token correspondente
            switch (valor) {
                case "Int":
                case "Float":
                case "Bool":
                    return new Token(TipoToken.TYPE, valor);
                case "getLine":
                    return new Token(TipoToken.READ, valor);
                case "print":
                    return new Token(TipoToken.WRITE, valor);
                case "if":
                    return new Token(TipoToken.IF, valor);
                case "then":
                    return new Token(TipoToken.THEN, valor);
                case "else":
                    return new Token(TipoToken.ELSE, valor);
                default:
                    return new Token(TipoToken.ID, valor);  // Caso seja um identificador
            }
        }

        // Verifica se o caractere atual é um dígito (para números inteiros e reais)
        if (Character.isDigit(charAtual)) {
            // Avança enquanto o caractere atual for um dígito
            while (pos < tamanho && Character.isDigit(entrada.charAt(pos))) {
                pos++;  // Avança para o próximo caractere
            }
            // Verifica se há parte decimal (número real)
            if (pos < tamanho && entrada.charAt(pos) == '.') {
                pos++;  // Avança para o próximo caractere (após o ponto)
                // Continua avançando enquanto forem encontrados dígitos
                while (pos < tamanho && Character.isDigit(entrada.charAt(pos))) {
                    pos++;  // Avança para o próximo caractere
                }
                // Retorna um token de número real (com ponto decimal)
                return new Token(TipoToken.NREAL, entrada.substring(inicio, pos));
            }
            // Retorna um token de número inteiro
            return new Token(TipoToken.NINT, entrada.substring(inicio, pos));
        }

        // Verifica se o caractere atual é aspas duplas (para strings)
        if (charAtual == '"') {
            pos++;  // Avança para o próximo caractere (após a primeira aspas)
            // Avança até encontrar o fechamento das aspas
            while (pos < tamanho && entrada.charAt(pos) != '"') {
                pos++;  // Avança para o próximo caractere
            }
            // Verifica se encontrou o fechamento das aspas
            if (pos < tamanho) {
                pos++;  // Avança para incluir o caractere de fechamento das aspas
                // Retorna um token de string
                return new Token(TipoToken.NSTRING, entrada.substring(inicio, pos));
            } else {
                // Caso não encontre o fechamento das aspas, retorna um token desconhecido
                return new Token(TipoToken.UNKNOWN, entrada.substring(inicio, pos));
            }
        }

        // Verifica operadores e outros caracteres especiais
        switch (charAtual) {
            case '+': case '-': case '*': case '/':
                pos++;  // Avança para o próximo caractere
                // Retorna um token de operador
                return new Token(TipoToken.OP, String.valueOf(charAtual));
            case '<':
                // Verifica se é um operador de atribuição "<-"
                if (pos + 1 < tamanho && entrada.charAt(pos + 1) == '-') {
                    pos += 2;  // Avança para além do operador de atribuição
                    return new Token(TipoToken.ASSIGN, "<-");
                }
                pos++;  // Avança para o próximo caractere
                // Retorna um token de operador
                return new Token(TipoToken.OP, "<");
            case '>':
                pos++;  // Avança para o próximo caractere
                // Retorna um token de operador
                return new Token(TipoToken.OP, ">");
            case '(':
            case ')':
                pos++;  // Avança para o próximo caractere
                // Retorna um token de parênteses
                return new Token(TipoToken.PAREN, String.valueOf(charAtual));
            case ':':
                pos++;  // Avança para o próximo caractere
                // Retorna um token de dois pontos
                return new Token(TipoToken.COLON, String.valueOf(charAtual));
            case '=':
                pos++;  // Avança para o próximo caractere
                // Retorna um token de igualdade
                return new Token(TipoToken.EQUAL, String.valueOf(charAtual));
        }

        // Caso nenhum token válido tenha sido identificado, retorna um token desconhecido
        pos++;  // Avança para o próximo caractere
        return new Token(TipoToken.UNKNOWN, String.valueOf(charAtual));
    }
}
