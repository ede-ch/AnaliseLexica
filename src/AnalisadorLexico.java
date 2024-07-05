import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class AnalisadorLexico {
    private String entrada;
    private int pos;
    private int tamanho;

    public AnalisadorLexico(String entrada) {
        this.entrada = entrada;
        this.pos = 0;
        this.tamanho = entrada.length();
    }

    public Token leToken() {
        if (pos >= tamanho) {
            return new Token(TipoToken.EOF, "");
        }

        char charAtual = entrada.charAt(pos);

        // Ignora espaços em branco e quebras de linha
        while (Character.isWhitespace(charAtual)) {
            pos++;
            if (pos >= tamanho) {
                return new Token(TipoToken.EOF, "");
            }
            charAtual = entrada.charAt(pos);
        }

        // Ignora comentários
        if (charAtual == '-' && pos + 1 < tamanho && entrada.charAt(pos + 1) == '-') {
            while (pos < tamanho && entrada.charAt(pos) != '\n') {
                pos++;
            }
            pos++; // Pula a quebra de linha
            return leToken();
        }

        int inicio = pos;

        if (Character.isLetter(charAtual)) {
            while (pos < tamanho && Character.isLetterOrDigit(entrada.charAt(pos))) {
                pos++;
            }
            String valor = entrada.substring(inicio, pos);
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
                    return new Token(TipoToken.ID, valor);
            }
        }

        if (Character.isDigit(charAtual)) {
            while (pos < tamanho && Character.isDigit(entrada.charAt(pos))) {
                pos++;
            }
            if (pos < tamanho && entrada.charAt(pos) == '.') {
                pos++;
                while (pos < tamanho && Character.isDigit(entrada.charAt(pos))) {
                    pos++;
                }
                return new Token(TipoToken.NREAL, entrada.substring(inicio, pos));
            }
            return new Token(TipoToken.NINT, entrada.substring(inicio, pos));
        }

        if (charAtual == '"') {
            pos++;
            while (pos < tamanho && entrada.charAt(pos) != '"') {
                pos++;
            }
            if (pos < tamanho) {
                pos++; // Inclui o caractere de fechamento "
                return new Token(TipoToken.NSTRING, entrada.substring(inicio, pos));
            } else {
                return new Token(TipoToken.UNKNOWN, entrada.substring(inicio, pos));
            }
        }

        switch (charAtual) {
            case '+': case '-': case '*': case '/':
                pos++;
                return new Token(TipoToken.OP, String.valueOf(charAtual));
            case '<':
                if (pos + 1 < tamanho && entrada.charAt(pos + 1) == '-') {
                    pos += 2;
                    return new Token(TipoToken.ASSIGN, "<-");
                }
                pos++;
                return new Token(TipoToken.OP, "<");
            case '>':
                pos++;
                return new Token(TipoToken.OP, ">");
            case '(':
            case ')':
                pos++;
                return new Token(TipoToken.PAREN, String.valueOf(charAtual));
            case ':':
                pos++;
                return new Token(TipoToken.COLON, String.valueOf(charAtual));
            case '=':
                pos++;
                return new Token(TipoToken.EQUAL, String.valueOf(charAtual));
        }

        // Token inválido
        pos++;
        return new Token(TipoToken.UNKNOWN, String.valueOf(charAtual));
    }
}
