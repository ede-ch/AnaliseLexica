public class Token {
    private TipoToken tipo;  // Tipo do token (como ID, NREAL, NINT, etc.)
    private String valor;    // Valor do token (o texto que representa o token)

    // Construtor para inicializar um objeto Token com tipo e valor
    public Token(TipoToken tipo, String valor) {
        this.tipo = tipo;
        this.valor = valor;
    }

    // Método para obter o tipo do token
    public TipoToken getTipo() {
        return tipo;
    }

    // Método para obter o valor do token
    public String getValor() {
        return valor;
    }

    // Método toString para retornar uma representação em string do Token
    @Override
    public String toString() {
        return "Token{" +
                "tipo=" + tipo +
                ", valor='" + valor + '\'' +
                '}';
    }
}
