import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Main {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(
                new FileReader("src/resources/exemplo2.hs", StandardCharsets.UTF_8));
            StringBuilder conteudo = new StringBuilder();
            String linha;
            
            // Lê cada linha do arquivo e armazena no StringBuilder 'conteudo'
            while ((linha = reader.readLine()) != null) {
                conteudo.append(linha).append("\n");
            }
            
            // Cria um analisador léxico com o conteúdo lido do arquivo
            AnalisadorLexico analisador = new AnalisadorLexico(conteudo.toString());
            Token token;
            
            // Lê e exibe cada token identificado pelo analisador léxico
            while ((token = analisador.leToken()).getTipo() != TipoToken.EOF) {
                System.out.println("Token encontrado: " + token.getTipo() + " - " + token.getValor());
            }
            
            reader.close();  // Fecha o leitor de arquivo
        } catch (Exception e) {
            e.printStackTrace();  // Imprime informações de exceção, se ocorrer
        }
    }
}
