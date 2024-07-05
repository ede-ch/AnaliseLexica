import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Main {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(
                new FileReader("src/resources/exemplo1.hs", StandardCharsets.UTF_8));
            StringBuilder conteudo = new StringBuilder();
            String linha;
            while ((linha = reader.readLine()) != null) {
                conteudo.append(linha).append("\n");
            }
            AnalisadorLexico analisador = new AnalisadorLexico(conteudo.toString());
            Token token;
            while ((token = analisador.leToken()).getTipo() != TipoToken.EOF) {
                System.out.println("Token encontrado: " + token.getTipo() + " - " + token.getValor());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
