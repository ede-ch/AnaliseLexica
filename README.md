# Analisador Léxico para Haskell

## Especificação Léxica de Haskell

O analisador léxico desenvolvido em Java para a linguagem Haskell reconhece os seguintes tokens:

- **Identificadores (`ID`)**: Sequências de letras e dígitos, começando com uma letra.
- **Números Reais (`NREAL`)**: Números decimais com parte decimal opcional.
- **Números Inteiros (`NINT`)**: Números inteiros sem parte decimal.
- **Strings (`NSTRING`)**: Sequências delimitadas por aspas duplas.
- **Palavras-chave (`TYPE`, `READ`, `WRITE`, `IF`, `THEN`, `ELSE`)**: Palavras reservadas da linguagem.
- **Operadores (`OP`)**: Operadores aritméticos e de atribuição, como `+`, `-`, `*`, `/`, `<-`, etc.
- **Delimitadores (`PAREN`, `COLON`)**: Parênteses `(` e `)`, dois pontos `:`, entre outros.
- **Operador de Igualdade (`EQUAL`)**: O operador `=`.
- **Fim de Arquivo (`EOF`)**: Indica o final do código fonte.
- **Tokens Desconhecidos (`UNKNOWN`)**: Qualquer outro caractere não reconhecido.

## Configuração de Execução

Para executar o analisador léxico para Haskell desenvolvido em Java, siga estas instruções:

### Passo 1: Clonar o Repositório

Clone o repositório para o seu ambiente local:

´´´ bash
git clone <AnalisadorLexico>

### Passo 2: Configurar o Caminho do Arquivo de Entrada

No arquivo Main.java, localizado na pasta src, ajuste o caminho do arquivo de entrada para corresponder ao local onde você salvou o arquivo .hs que deseja analisar. 
Certifique-se de alterar "caminho/para/seu/arquivo.hs" para o caminho absoluto ou relativo correto do seu arquivo .hs.

### Passo 3: Compilar o Programa

Compile o programa Java. Você pode usar o javac para compilar os arquivos .java:

javac -d bin src/Main.java

### Passo 4:  Executar o Programa

Após compilar, execute o programa compilado:

java -cp bin Main

