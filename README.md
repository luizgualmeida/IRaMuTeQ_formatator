# 📄 IRaMuTeQ Formatator (PT & EN)

> Aplicativo web em Shiny que transforma seus dados de pesquisa em um corpus pronto para análise no [IRaMuTeQ](http://www.iramuteq.org/).

---

## 🧠 O que é este app?

O **IRaMuTeQ Formatator** lê arquivos de planilha com dados qualitativos (entrevistas, respostas abertas, depoimentos) e formata automaticamente o texto no padrão `.txt` exigido pelo IRaMuTeQ, incluindo cabeçalhos, tags de metadados, remoção de stopwords, acentos, substituições personalizadas e muito mais.

---

## 🖥️ Requisitos

Antes de rodar o app, certifique-se de ter o **R** instalado (versão 4.1 ou superior recomendada).

> Download do R: https://cran.r-project.org/

Você também precisará dos seguintes pacotes do R. Cole o bloco abaixo no console do R para instalar todos de uma vez:

```r
install.packages(c(
  "shiny",
  "bs4Dash",
  "readxl",
  "readr", 
  "dplyr",
  "janitor",
  "stringr",
  "DT",
  "plotly",
  "shinycssloaders",
  "shinyWidgets",
  "purrr"
))
```

---

## 🚀 Como rodar o app

1. Baixe ou clone o repositório para o seu computador.
2. Abra o arquivo `formatator.R` no **RStudio** (ou qualquer IDE de R).
3. Clique no botão **Run App**, ou execute o seguinte comando no console:

```r
shiny::runApp("formatator.R")
```

4. O app abrirá automaticamente no seu navegador.
5. O arquivo **materias_com_textos.csv** contém matérias de jornais cobrindo o caso de financiamento de parte do filme Dark Horse (biografia de Jair Bolsonaro) pelo Banco Master. Utilize o arquivo como teste do app.

As colunas do banco de dados são:
 - título - Título da matéria
 - veiculo	- Nome do veículo jornalístico
 - classificacao	- Classificação de orientação política do jornal
 - url	- Link para a matéria
 - TEXTO - Texto completo da matéria


---

## 📋 Passo a passo completo

O app é dividido em **5 abas**. Siga-as na ordem indicada:

---

### Aba 1 — 📁 Upload & Config

É aqui que tudo começa.

#### 1.1 Faça o upload do seu arquivo

Clique em **Procurar** e selecione o arquivo com seus dados. Formatos aceitos:

| Formato | Extensão |
|---------|----------|
| Excel   | `.xlsx`  |
| CSV     | `.csv`   |
| OpenDocument Spreadsheet | `.ods` |

Seu arquivo deve ter **uma linha por documento** (entrevista, resposta, etc.) e **uma coluna por variável**.

#### 1.2 Visualize seus dados

Após o upload, uma tabela com prévia dos dados será exibida para confirmar que o carregamento ocorreu corretamente.

#### 1.3 Configure as colunas

Você precisa indicar ao app o papel de cada coluna:

| Seleção | Finalidade |
|---------|-----------|
| **Colunas de texto** | Colunas com entrevistas, respostas abertas ou narrativas — o conteúdo que será analisado |
| **Colunas de metadados** | Variáveis categóricas como grupo etário, sexo, cidade, grupo de intervenção, etc. |

> Você pode selecionar **múltiplas colunas de texto** e **múltiplas colunas de metadados**.

#### 1.4 Editar rótulos das colunas (opcional)

Após selecionar as colunas, aparece um editor de rótulos. Por padrão, os cabeçalhos do IRaMuTeQ usam os nomes originais das colunas. Aqui você pode renomeá-los para etiquetas mais curtas ou descritivas.

Exemplo: nome da coluna `resposta_entrevista_q1` → rótulo `q1`

#### 1.5 Opções de processamento

Um painel com botões de liga/desliga permite ativar ou desativar cada etapa do pré-processamento:

| Opção | Padrão | Descrição |
|-------|--------|-----------|
| **Converter para minúscula** | ✅ Ativo | Transforma todo o texto em letras minúsculas |
| **Remover acentos** | ✅ Ativo | Remove diacríticos (á → a, ç → c, etc.) |
| **Remover pontuação** | ✅ Ativo | Remove `.,!?;:` etc. (underscores `_` são preservados) |
| **Remover números** | ❌ Inativo | Remove todos os dígitos numéricos |
| **Limpar espaços** | ✅ Ativo | Colapsa múltiplos espaços em um único |
| **Remover textos curtos** | ❌ Inativo | Filtra textos com menos de 10 caracteres ou 5 palavras |
| **Tamanho mínimo de palavra** | 3 | Palavras menores que este número de caracteres são removidas |
| **Usar lista de stopwords** | ✅ Ativo | Remove palavras funcionais comuns do português |
| **Stopwords adicionais** | (vazio) | Insira palavras extras para remover, separadas por vírgula |

> **Importante:** A ordem de processamento é fixa e otimizada: remoção de acentos → minúscula → substituições personalizadas → remoção de pontuação → remoção de números → limpeza de espaços → remoção de palavras curtas → stopwords. Isso garante que os dicionários de substituição sempre funcionem corretamente.

#### 1.6 Substituições personalizadas (opcional)

Faça o upload de um arquivo de dicionário `.csv` ou `.xlsx` com duas colunas:

| Coluna | Exemplo |
|--------|---------|
| `original` | `lava jato` |
| `substituto` | `lava_jato` |

O app normaliza automaticamente a coluna `original` (remove acentos + minúscula), então entradas como `"Lava Jato"` ou `"lava jato"` funcionam igualmente bem.

Use para:
- Unir expressões compostas: `lava jato` → `lava_jato`
- Padronizar variações ortográficas: `covid`, `coronavirus`, `corona virus` → todos viram `coronavirus`
- Substituir entidades nomeadas ou nomes próprios

Clique em **Baixar modelo** para obter um arquivo de exemplo pronto para preencher.

---

### Aba 2 — 🔍 Validação

Depois de configurar as colunas, esta aba exibe:

- **Estatísticas gerais** — total de documentos, total de palavras, vocabulário único, média de palavras por documento
- **Avisos de validação** — células vazias, textos muito curtos
- **Gráfico de distribuição de metadados** — gráfico de barras de uma variável categórica selecionada
- **Prévia do formato IRaMuTeQ** — mostra os 3 primeiros documentos no exato formato que será gerado, permitindo verificar se os cabeçalhos e tags de metadados estão corretos antes de processar

---

### Aba 3 — ⚙️ Processamento

Clique no botão **Processar Dados** para executar o pipeline completo.

Uma barra de progresso e mensagens de status acompanham o andamento. Ao finalizar, uma prévia dos 3 primeiros segmentos processados é exibida.

> Se nenhum texto for gerado (por exemplo, todos foram muito curtos), uma mensagem de erro explicará o que aconteceu.

---

### Aba 4 — 📊 Análise

Após o processamento, esta aba mostra:

- **Top 20 palavras mais frequentes** — gráfico de barras interativo
- **Estatísticas detalhadas** — contagem de palavras, riqueza de vocabulário, segmentos gerados
- **Relatório completo de processamento** — resumo de todas as configurações usadas e indicadores de qualidade dos dados

---

### Aba 5 — ⬇️ Download

Dois arquivos ficam disponíveis para download:

| Arquivo | Descrição |
|---------|-----------|
| **Corpus (`.txt`)** | O corpus formatado, pronto para importar no IRaMuTeQ |
| **Relatório (`.txt`)** | Resumo de todas as etapas de processamento e estatísticas |

---

## 🧾 Formato do corpus para o IRaMuTeQ

O arquivo `.txt` gerado segue esta estrutura para cada documento:

```
**** *coluna_1 *metadado_sexo_F *metadado_idade_25-34

texto da entrevista ou resposta aqui

```

- `****` — marca o início de um novo documento
- `*coluna_1` — identificador da coluna de texto e número da linha
- `*metadado_sexo_F` — cada variável de metadado no formato `*variavel_valor`

---

## 💡 Dicas

- Os valores de metadados **não devem conter espaços** — o app une nome da variável e valor automaticamente com underscore.
- Se precisar personalizar a lista de stopwords, use o botão **Lista de Stopwords** para visualizar a lista embutida, e acrescente palavras no campo **Stopwords adicionais**.
- Para melhores resultados no IRaMuTeQ, procure ter pelo menos **20 a 30 palavras por documento** após o pré-processamento.
- O dicionário de substituições é processado **após** a remoção de acentos e a conversão para minúscula, então você não precisa se preocupar com diferenças de codificação entre o dicionário e os seus dados.

---

## 📬 Dúvidas ou problemas?

Abra uma _issue_ no GitHub ou entre em contato diretamente com o desenvolvedor.

---

*Desenvolvido com ☕ usando R + Shiny*

---
# Englis version
---

# 📄 IRaMuTeQ Formatator

> A Shiny web app that transforms your research data into a corpus ready for analysis in [IRaMuTeQ](http://www.iramuteq.org/).

---

## 🧠 What is this app?

**IRaMuTeQ Formatator** reads spreadsheet files containing qualitative interview or survey data and automatically formats them into the `.txt` structure required by IRaMuTeQ — including headers, metadata tags, stopword removal, accent stripping, custom substitutions, and more.

---

## 🖥️ Requirements

Before running the app, make sure you have **R** installed (version 4.1 or higher is recommended).

> Download R: https://cran.r-project.org/

You will also need the following R packages. Copy and paste the block below into your R console to install them all at once:

```r
install.packages(c(
  "shiny",
  "bs4Dash",
  "readxl",
  "dplyr",
  "janitor",
  "stringr",
  "DT",
  "plotly",
  "shinycssloaders",
  "shinyWidgets",
  "purrr"
))
```

---

## 🚀 How to run the app

1. Download or clone the repository to your computer.
2. Open the file `formatator_en.R` in **RStudio** (or any R IDE).
3. Click the **Run App** button, or run the following command in the console:

```r
shiny::runApp("formatator_en.R")
```

4. The app will open in your browser automatically.

---

## 📋 Step-by-step walkthrough

The app is organized into **5 tabs**. Follow them in order:

---

### Tab 1 — 📁 Upload & Config

This is where everything starts.

#### 1.1 Upload your file

Click **Browse** and select your data file. Supported formats:

| Format | Extension |
|--------|-----------|
| Excel  | `.xlsx`   |
| CSV    | `.csv`    |
| OpenDocument Spreadsheet | `.ods` |

Your file should have **one row per document** (interview, response, etc.) and **one column per variable**.

#### 1.2 Preview your data

After uploading, a table preview will appear so you can confirm the data loaded correctly.

#### 1.3 Configure columns

You must tell the app which columns contain what:

| Selection | Purpose |
|-----------|---------|
| **Text columns** | Columns with interviews, open answers, or narratives — the content to be analyzed |
| **Metadata columns** | Categorical variables such as age group, gender, city, treatment group, etc. |

> You can select **multiple text columns** and **multiple metadata columns**.

#### 1.4 Edit column labels (optional)

After selecting columns, a label editor appears. By default, IRaMuTeQ headers use the original column names. Here you can rename them to shorter or more descriptive labels that will appear in the formatted output.

Example: column name `interview_answer_q1` → label `q1`

#### 1.5 Processing options

A panel with toggle switches lets you enable or disable each preprocessing step:

| Option | Default | Description |
|--------|---------|-------------|
| **Convert to lowercase** | ✅ On | Transforms all text to lowercase |
| **Remove accents** | ✅ On | Strips diacritics (á → a, ç → c, etc.) |
| **Remove punctuation** | ✅ On | Removes `.,!?;:` etc. (underscores `_` are preserved) |
| **Remove numbers** | ❌ Off | Removes all numeric digits |
| **Clean extra spaces** | ✅ On | Collapses multiple spaces into one |
| **Remove short texts** | ❌ Off | Filters out texts with fewer than 10 characters or 5 words |
| **Minimum word length** | 3 | Words shorter than this value (in characters) are removed |
| **Use stopword list** | ✅ On | Removes common Portuguese function words |
| **Additional stopwords** | (empty) | Enter extra words to remove, separated by commas |

> **Important:** The order of processing is fixed and optimized: accent removal → lowercase → custom substitutions → punctuation removal → number removal → space cleanup → short word removal → stopwords. This ensures substitution dictionaries always work correctly.

#### 1.6 Custom substitutions (optional)

Upload a `.csv` or `.xlsx` dictionary file with two columns:

| Column | Example |
|--------|---------|
| `original` | `lava jato` |
| `substituto` | `lava_jato` |

The app will automatically normalize the `original` column (remove accents + lowercase) so entries like `"Lava Jato"` or `"lava jato"` both work.

Use this to:
- Join multi-word expressions: `lava jato` → `lava_jato`
- Standardize spelling variants: `covid`, `coronavirus`, `corona virus` → all become `coronavirus`
- Replace named entities or proper nouns

Click **Download template** to get a ready-to-fill example file.

---

### Tab 2 — 🔍 Validation

After configuring columns, this tab shows:

- **Summary statistics** — total documents, total words, unique vocabulary, average words per document
- **Validation warnings** — empty cells, very short texts
- **Metadata distribution chart** — bar chart of a selected categorical variable
- **IRaMuTeQ format preview** — shows the first 3 documents in the exact format that will be generated, so you can verify headers and metadata tags look correct before processing

---

### Tab 3 — ⚙️ Processing

Click the **Process Data** button to run the full pipeline.

A progress bar and status messages will guide you. When finished, a preview of the first 3 processed segments is shown.

> If no text is generated (e.g., all texts were too short), an error message will explain what happened.

---

### Tab 4 — 📊 Analysis

After processing, this tab shows:

- **Top 20 most frequent words** — interactive bar chart
- **Detailed statistics** — word counts, vocabulary richness, segments generated
- **Full processing report** — summary of all settings used and data quality indicators

---

### Tab 5 — ⬇️ Download

Two files are available for download:

| File | Description |
|------|-------------|
| **Corpus (`.txt`)** | The formatted corpus ready to import into IRaMuTeQ |
| **Report (`.txt`)** | A summary of all processing steps and statistics |

---

## 🧾 IRaMuTeQ corpus format

The output `.txt` file follows this structure for each document:

```
**** *column_1 *metadata_gender_F *metadata_age_25-34

text of the interview or response here

```

- `****` — marks the start of a new document
- `*column_1` — identifier for the text column and row number
- `*metadata_gender_F` — each metadata variable in the format `*variable_value`

---

## 💡 Tips

- Metadata values **must not contain spaces** — the app uses underscores automatically when joining variable name and value.
- If your stopword list needs customization, use the **Stopwords** button to view the built-in list, and add extra words in the **Additional stopwords** text box.
- For best results in IRaMuTeQ, aim for at least **20–30 words per document** after preprocessing.
- The substitution dictionary is processed **after** accent removal and lowercasing, so you don't need to worry about encoding differences between your dictionary and your data.

---

## 📬 Questions or issues?

Open an issue on GitHub or contact the developer directly.

---

*Built with ☕ using R + Shiny*
