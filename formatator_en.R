# =============================================================================
# Formatador IRaMuTeQ - Versão Corrigida
# Correção principal: substituições personalizadas aplicadas APÓS normalização
# (remoção de acentos + conversão para minúscula), garantindo que o match
# entre dicionário e texto funcione corretamente.
# =============================================================================

library(shiny)
library(bs4Dash)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(purrr)

# =============================================================================
# STOPWORDS
# =============================================================================

stopwords_br <- c(
  # Artigos e contrações
  'a', 'o', 'as', 'os', 'um', 'uma', 'uns', 'umas', 'ao', 'à', 'aos', 'às',
  'do', 'da', 'dos', 'das', 'no', 'na', 'nos', 'nas', 'pelo', 'pela', 'pelos', 'pelas',
  
  # Pronomes
  'eu', 'tu', 'ele', 'ela', 'nos', 'vos', 'eles', 'elas', 'me', 'mim', 'comigo',
  'te', 'ti', 'contigo', 'se', 'si', 'consigo', 'lhe', 'lhes',
  'meu', 'minha', 'meus', 'minhas', 'teu', 'tua', 'teus', 'tuas', 'seu', 'sua',
  'seus', 'suas', 'nosso', 'nossa', 'nossos', 'nossas', 'vosso', 'vossa', 'vossos', 'vossas',
  
  # Preposições
  'de', 'em', 'por', 'para', 'com', 'sem', 'sob', 'sobre', 'entre', 'ate',
  'desde', 'contra', 'perante', 'atraves', 'alem', 'dentro', 'fora', 'perto',
  'longe', 'durante',
  
  # Conjunções
  'e', 'mas', 'ou', 'porque', 'pois', 'que', 'se', 'como', 'quando', 'embora',
  'porem', 'todavia', 'contudo', 'entao', 'tambem', 'apesar', 'caso', 'portanto', 'logo',
  
  # Verbos auxiliares e comuns
  'ser', 'estar', 'ter', 'haver', 'ir', 'vir', 'fazer', 'poder', 'dever',
  'querer', 'saber', 'esta', 'estao', 'tem', 'tenho',
  
  # Demonstrativos
  'esse', 'essa', 'isso', 'este', 'esta', 'isto', 'aquele', 'aquela', 'aquilo',
  'aqueles', 'aquelas', 'deste', 'desta', 'destes', 'destas', 'disso', 'daquilo',
  'nisto', 'naquilo',
  
  # Localizadores
  'la', 'aqui', 'ali', 'onde', 'aonde',
  
  # Interjeições
  'ah', 'oh', 'ei', 'oi', 'ola', 'opa', 'eita', 'nossa', 'caramba', 'poxa',
  'uau', 'xi', 'ih', 'ue', 'hein',
  
  # Gírias e expressões coloquiais
  'cara', 'mano', 'mina', 'vei', 'velho', 'brother', 'parceiro', 'camarada',
  'bacana', 'maneiro', 'massa', 'show', 'top', 'legal', 'beleza', 'joia', 'firmeza',
  
  # Abreviações e siglas comuns
  'pq', 'tb', 'tbm', 'vc', 'ce', 'mt', 'mto', 'mta', 'td', 'tdo', 'tda',
  'hj', 'amg', 'bjs', 'bjss', 'vlw', 'flw', 'blz', 'ne', 'ta', 'to'
)

# =============================================================================
# FUNÇÕES AUXILIARES
# =============================================================================

#' Remove acentos de um vetor de strings
remover_acentos <- function(texto) {
  texto <- as.character(texto)
  texto <- str_replace_all(texto, "[áàâãäå]", "a")
  texto <- str_replace_all(texto, "[éèêë]",   "e")
  texto <- str_replace_all(texto, "[íìîï]",   "i")
  texto <- str_replace_all(texto, "[óòôõö]",  "o")
  texto <- str_replace_all(texto, "[úùûü]",   "u")
  texto <- str_replace_all(texto, "[ç]",      "c")
  texto <- str_replace_all(texto, "[ñ]",      "n")
  texto <- str_replace_all(texto, "[ÁÀÂÃÄÅ]", "A")
  texto <- str_replace_all(texto, "[ÉÈÊË]",   "E")
  texto <- str_replace_all(texto, "[ÍÌÎÏ]",   "I")
  texto <- str_replace_all(texto, "[ÓÒÔÕÖ]",  "O")
  texto <- str_replace_all(texto, "[ÚÙÛÜ]",   "U")
  texto <- str_replace_all(texto, "[Ç]",      "C")
  texto <- str_replace_all(texto, "[Ñ]",      "N")
  texto
}

#' Valida os dados carregados e retorna lista de problemas
validar_dados <- function(df, cols_texto, cols_meta) {
  if (length(cols_texto) == 0) return(list())
  
  cols_texto %>%
    set_names() %>%
    map(function(col) {
      if (!col %in% names(df)) return(list())
      col_data <- df[[col]]
      
      vazios        <- sum(is.na(col_data) | col_data == "" | str_trim(col_data) == "", na.rm = TRUE)
      textos_curtos <- sum(str_length(as.character(col_data)) < 10, na.rm = TRUE)
      
      probs <- list()
      if (vazios > 0)
        probs[[paste0("vazio_", col)]] <- paste0("⚠️ Column '", col, "' has ", vazios, " empty cells")
      if (textos_curtos > 0)
        probs[[paste0("curto_", col)]] <- paste0("📏 Column '", col, "' has ", textos_curtos, " texts < 10 characters")
      probs
    }) %>%
    flatten()
}

#' Calcula estatísticas gerais do corpus
estatisticas_corpus <- function(df, cols_texto, cols_meta = NULL) {
  if (length(cols_texto) == 0) return(list())
  
  texto_completo <- df %>%
    select(all_of(cols_texto)) %>%
    pmap_chr(~ paste(..., collapse = " ")) %>%
    paste(collapse = " ")
  
  palavras <- str_extract_all(str_to_lower(texto_completo), "\\b\\w+\\b")[[1]]
  palavras <- palavras[str_length(palavras) > 0]
  
  list(
    total_documentos  = nrow(df),
    colunas_texto     = length(cols_texto),
    colunas_meta      = length(cols_meta %||% c()),
    total_palavras    = length(palavras),
    vocabulario_unico = length(unique(palavras)),
    media_palavras_doc = round(length(palavras) / nrow(df), 2)
  )
}

#' Remove stopwords de um vetor de textos
remover_stopwords_vectorizado <- function(textos, stopwords, config) {
  if (!config$usar_stopwords || length(stopwords) == 0) return(textos)
  
  all_sw <- stopwords
  if (!is.null(config$stopwords_custom) && nzchar(config$stopwords_custom)) {
    extras <- str_trim(str_split(config$stopwords_custom, ",")[[1]])
    extras <- remover_acentos(str_to_lower(extras[nzchar(extras)]))
    all_sw <- c(stopwords, extras)
  }
  
  padrao <- paste0("\\b(", paste(all_sw, collapse = "|"), ")\\b")
  str_replace_all(textos, regex(padrao, ignore_case = TRUE), " ") %>%
    str_squish()
}

#' Constrói a string de metadados para cada linha do df
criar_metadados_batch <- function(df, metadados_cols, indices, rotulos_meta = NULL) {
  if (is.null(metadados_cols) || length(metadados_cols) == 0)
    return(rep("", length(indices)))
  
  metadados_cols <- metadados_cols[metadados_cols %in% names(df)]
  if (length(metadados_cols) == 0)
    return(rep("", length(indices)))
  
  map_chr(indices, function(i) {
    if (i > nrow(df)) return("")
    
    vals <- map_chr(metadados_cols, function(col) {
      valor <- df[i, col]
      if (is.na(valor) || is.null(valor)) return("")
      
      rotulo <- if (!is.null(rotulos_meta) &&
                    col %in% names(rotulos_meta) &&
                    !is.null(rotulos_meta[[col]]) &&
                    nzchar(rotulos_meta[[col]])) {
        rotulos_meta[[col]]
      } else {
        col
      }
      paste0("*", rotulo, "_", valor)
    })
    
    vals <- vals[nzchar(vals)]
    if (length(vals) > 0) paste0(" ", paste(vals, collapse = " ")) else ""
  })
}

# =============================================================================
# FUNÇÃO PRINCIPAL DE PROCESSAMENTO — pipeline com ordem corrigida
# =============================================================================
#
# Ordem de operações (CRÍTICA para que substituições funcionem):
#
#   1. Remover acentos          ← normaliza o texto
#   2. Converter para minúscula ← normaliza o texto
#   3. Aplicar substituições    ← texto já está no mesmo formato do dicionário
#   4. Remover pontuação        ← preserva underscores criados nas substituições
#   5. Remover números
#   6. Limpar espaços
#   7. Remover palavras curtas
#   8. Remover stopwords
#
processar_coluna_otimizada <- function(col_name,
                                       df,
                                       config,
                                       metadados_cols,
                                       rotulos_texto  = NULL,
                                       rotulos_meta   = NULL,
                                       substituicoes  = NULL) {
  
  if (!col_name %in% names(df)) return(character(0))
  
  textos <- as.character(df[[col_name]])
  
  # ── 1. Remover acentos ────────────────────────────────────────────────────
  if (config$remover_acentos) {
    textos <- remover_acentos(textos)
  }
  
  # ── 2. Converter para minúscula ───────────────────────────────────────────
  if (config$converter_minuscula) {
    textos <- str_to_lower(textos)
  }
  
  # ── 3. Substituições personalizadas (após normalização) ───────────────────
  if (!is.null(substituicoes) && nrow(substituicoes) > 0) {
    
    esc_regex <- function(x) str_replace_all(x, "([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1")
    
    # Agrupar regras pelo mesmo substituto.
    # Ex: flavio → flavio_bolsonaro  ┐
    #     bolsonaro → flavio_bolsonaro ┘ → mesmo grupo
    grupos <- split(substituicoes, substituicoes$substituto)
    
    for (novo in names(grupos)) {
      partes <- as.character(grupos[[novo]]$original)
      partes <- partes[nzchar(partes)]
      if (length(partes) == 0) next
      
      partes_esc <- esc_regex(partes)
      
      # Gerar todas as permutações de 2+ partes separadas por espaço.
      # Isso cria padrões como "flavio\s+bolsonaro" e "bolsonaro\s+flavio"
      # que casam o composto INTEIRO como uma unidade → substituição única.
      compostos <- character(0)
      if (length(partes) >= 2) {
        indices <- seq_along(partes)
        for (r in 2:min(length(partes), 4)) {
          grid   <- do.call(expand.grid, rep(list(indices), r))
          validas <- apply(grid, 1, function(row) length(unique(row)) == r)
          grid   <- grid[validas, , drop = FALSE]
          for (i in seq_len(nrow(grid))) {
            compostos <- c(compostos,
                           paste(partes_esc[as.integer(grid[i, ])], collapse = "\\s+"))
          }
        }
        compostos <- unique(compostos)
        # Compostos mais longos têm prioridade na alternância do regex
        compostos <- compostos[order(-nchar(compostos))]
      }
      
      # Padrão final: compostos primeiro (maior → menor), partes individuais depois.
      # O regex tenta as alternativas da esquerda para a direita, então o composto
      # "flavio\s+bolsonaro" é tentado antes de "flavio" ou "bolsonaro" sozinhos.
      alternativas <- c(compostos, partes_esc)
      padrao <- paste0("\\b(", paste(alternativas, collapse = "|"), ")\\b")
      
      textos <- str_replace_all(textos, regex(padrao), novo)
    }
  }
  
  # ── 4. Remover pontuação (preserva _ de compostos como lava_jato) ─────────
  if (config$remover_pontuacao) {
    # Remove pontuação geral mas mantém underscore
    textos <- str_replace_all(textos, "[!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]^`{|}~]", " ")
  }
  
  # ── 5. Remover números ────────────────────────────────────────────────────
  if (config$remover_numeros) {
    textos <- str_replace_all(textos, "\\d+", "")
  }
  
  # ── 6. Limpar espaços extras ──────────────────────────────────────────────
  if (config$remover_espacos_extras) {
    textos <- str_squish(textos)
  }
  
  # ── 7. Remover palavras muito curtas ──────────────────────────────────────
  if (config$tamanho_minimo > 1) {
    padrao <- paste0("\\b\\w{1,", config$tamanho_minimo - 1, "}\\b")
    textos <- str_replace_all(textos, padrao, " ")
    textos <- str_squish(textos)
  }
  
  # ── 8. Remover stopwords ──────────────────────────────────────────────────
  if (config$usar_stopwords) {
    textos <- remover_stopwords_vectorizado(textos, stopwords_br, config)
  }
  
  # ── Filtrar textos muito curtos ───────────────────────────────────────────
  indices <- seq_len(length(textos))
  
  if (config$remover_textos_curtos) {
    textos  <- str_trim(textos)
    validos <- str_length(textos) >= 10 & str_count(textos, "\\S+") >= 5
    textos  <- textos[validos]
    indices <- indices[validos]
  }
  
  if (length(textos) == 0) return(character(0))
  
  # ── Rótulo da coluna de texto ─────────────────────────────────────────────
  custom_label <- if (!is.null(rotulos_texto) &&
                      col_name %in% names(rotulos_texto) &&
                      !is.null(rotulos_texto[[col_name]]) &&
                      nzchar(rotulos_texto[[col_name]])) {
    rotulos_texto[[col_name]]
  } else {
    col_name
  }
  
  # ── Montar metadados e cabeçalhos ─────────────────────────────────────────
  meta_strings <- criar_metadados_batch(df, metadados_cols, indices, rotulos_meta)
  headers      <- paste0("**** *", custom_label, "_", indices, meta_strings, "\n\n")
  
  paste0(headers, textos, "\n\n")
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  
  dashboardHeader(
    title = tags$div(
      tags$i(class = "fas fa-file-alt", style = "margin-right: 10px;"),
      "IRaMuTeQ Formatator",
      style = "font-weight: 600;"
    ),
    rightUi = tagList(
      tags$li(class = "dropdown",
              actionButton("tutorial_btn",
                           label = tags$span(tags$i(class = "fas fa-info-circle"), " About"),
                           class = "btn-info btn-sm",
                           style = "margin-right: 5px;")
      ),
      tags$li(class = "dropdown",
              actionButton("ajuda_btn",
                           label = tags$span(tags$i(class = "fas fa-question-circle"), " Help"),
                           class = "btn-outline-info btn-sm")
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📁 Upload & Config",  tabName = "upload",   icon = icon("upload")),
      menuItem("🔍 Validation",        tabName = "validacao", icon = icon("check-circle")),
      menuItem("⚙️ Processing",        tabName = "process",   icon = icon("cogs")),
      menuItem("📊 Analysis",          tabName = "analise",   icon = icon("chart-bar")),
      menuItem("⬇️ Download",          tabName = "download",  icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f8f9fa; }
        .main-header .navbar { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .card { box-shadow: 0 0 20px rgba(0,0,0,0.05); border: none; }
        .card-header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; }
        .btn-primary { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; }
        .btn-success { background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); border: none; }
        .btn-warning { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); border: none; }
        .progress-bar { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
        .alert { border-left: 4px solid; border-radius: 0 4px 4px 0; }
        .selector-container { display: flex; gap: 20px; margin-top: 15px; }
        .selector-box { flex: 1; padding: 15px; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); }
        .config-panel { background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0; }
        .stat-card { text-align: center; padding: 20px; }
        .stat-number { font-size: 2em; font-weight: bold; color: #667eea; }
        .stat-label { color: #6c757d; margin-top: 5px; }
        .preview-box { background: #f8f9fa; padding: 15px; border-radius: 8px; font-family: monospace; white-space: pre-wrap; max-height: 400px; overflow-y: auto; }
        .problem-item  { padding: 8px 12px; margin: 5px 0; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px; }
        .success-item  { padding: 8px 12px; margin: 5px 0; background: #d1ecf1; border-left: 4px solid #17a2b8; border-radius: 4px; }
      "))
    ),
    
    tabItems(
      
      # ── Tab 1: Upload & Config ──────────────────────────────────────────────
      tabItem(tabName = "upload",
              fluidRow(
                box(title = tags$span(tags$i(class = "fas fa-cloud-upload-alt"), " File Upload"),
                    width = 12, status = "primary", solidHeader = TRUE,
                    fileInput("file",
                              label = tags$div(
                                tags$h5("Choose your data file"),
                                tags$p("Supported formats: .csv, .xlsx, .ods", class = "text-muted")
                              ),
                              accept = c(".csv", ".xlsx", ".ods"),
                              buttonLabel = "Browse...",
                              placeholder = "No file selected"),
                    conditionalPanel(
                      condition = "output.file_uploaded",
                      div(class = "alert alert-success", role = "alert",
                          tags$i(class = "fas fa-check-circle"),
                          " File loaded successfully!")
                    )
                )
              ),
              
              conditionalPanel(
                condition = "output.file_uploaded",
                
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-table"), " Data Preview"),
                      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(DT::dataTableOutput("preview_table"), type = 4)
                  )
                ),
                
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-columns"), " Column Configuration"),
                      width = 12, status = "info", solidHeader = TRUE,
                      div(class = "selector-container",
                          div(class = "selector-box",
                              tags$h5(tags$i(class = "fas fa-file-text"), " Text Columns"),
                              tags$p("Select columns containing interviews/responses", class = "text-muted"),
                              uiOutput("col_selector")
                          ),
                          div(class = "selector-box",
                              tags$h5(tags$i(class = "fas fa-tags"), " Metadata Columns"),
                              tags$p("Categorical variables (age, sex, group, etc.)", class = "text-muted"),
                              uiOutput("meta_selector"),
                              br(),
                              box(title = tags$span(tags$i(class = "fas fa-edit"), " Edit labels for IRaMuTeQ header"),
                                  width = 12, status = "secondary", solidHeader = TRUE,
                                  uiOutput("label_editor")
                              )
                          )
                      )
                  )
                ),
                
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-sliders-h"), " Processing Settings"),
                      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "config-panel",
                          fluidRow(
                            column(3, materialSwitch("converter_minuscula",    "Convert to lowercase",           value = TRUE,  status = "primary")),
                            column(3, materialSwitch("remover_acentos",        "Remove accents",                 value = TRUE,  status = "primary")),
                            column(3, materialSwitch("remover_pontuacao",      "Remove punctuation",             value = TRUE,  status = "primary")),
                            column(3, materialSwitch("remover_numeros",        "Remove numbers",                 value = FALSE, status = "primary")),
                            column(3, materialSwitch("remover_espacos_extras", "Clean extra spaces",             value = TRUE,  status = "primary")),
                            column(3, materialSwitch("remover_textos_curtos",  "Remove texts < 10 characters",   value = FALSE, status = "danger"))
                          ),
                          br(),
                          fluidRow(
                            column(4,
                                   numericInput("tamanho_minimo", "Minimum word length:",
                                                value = 3, min = 1, max = 10, step = 1),
                                   helpText("Shorter words will be removed")
                            ),
                            column(4, materialSwitch("usar_stopwords", "Use stopwords list", value = TRUE, status = "success")),
                            column(4,
                                   conditionalPanel(
                                     condition = "input.usar_stopwords",
                                     actionButton("edit_stopwords", "Stopwords List",
                                                  class = "btn-outline-secondary btn-sm")
                                   )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.usar_stopwords",
                            textAreaInput("stopwords_custom",
                                          "Additional stopwords (comma-separated):",
                                          value = "", height = "80px",
                                          placeholder = "E.g.: then, like, right, anyway...")
                          )
                      )
                  )
                ),
                
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-exchange-alt"), " Custom Substitutions"),
                      width = 12, status = "secondary", solidHeader = TRUE, collapsible = TRUE,
                      fluidRow(
                        column(6,
                               fileInput("dicionario_file",
                                         label = tags$div(
                                           tags$h6("Upload Substitution Dictionary"),
                                           tags$p("CSV/Excel file with columns: 'original' and 'substituto'",
                                                  class = "text-muted")
                                         ),
                                         accept = c(".csv", ".xlsx"),
                                         buttonLabel = "Browse...",
                                         placeholder = "No file selected")
                        ),
                        column(6,
                               tags$div(
                                 tags$h6("📋 How to create the file:"),
                                 tags$ul(
                                   tags$li("Column 'original': text to be replaced (with or without accents — the app normalizes automatically)"),
                                   tags$li("Column 'substituto': replacement text"),
                                   tags$li("Example: 'lava jato' → 'lava_jato'")
                                 ),
                                 downloadButton("download_modelo_subs", "📥 Download template",
                                                class = "btn btn-outline-info btn-sm")
                               )
                        )
                      ),
                      # Feedback visual do dicionário carregado
                      uiOutput("dicionario_status")
                  )
                )
              )
      ),
      
      # ── Tab 2: Validação ────────────────────────────────────────────────────
      tabItem(tabName = "validacao",
              conditionalPanel(
                condition = "output.file_uploaded",
                fluidRow(
                  column(3, valueBoxOutput("total_docs",      width = NULL)),
                  column(3, valueBoxOutput("total_palavras",  width = NULL)),
                  column(3, valueBoxOutput("vocabulario_unico", width = NULL)),
                  column(3, valueBoxOutput("media_palavras",  width = NULL))
                ),
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-exclamation-triangle"), " Data Validation"),
                      width = 6, status = "warning", solidHeader = TRUE,
                      withSpinner(uiOutput("problemas_validacao"), type = 4)
                  ),
                  box(title = tags$span(tags$i(class = "fas fa-chart-pie"), " Metadata Distribution"),
                      width = 6, status = "info", solidHeader = TRUE,
                      uiOutput("meta_distribution_ui"),
                      withSpinner(plotlyOutput("meta_distribution", height = "300px"), type = 4)
                  )
                ),
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-eye"), " IRaMuTeQ Format Preview"),
                      width = 12, status = "success", solidHeader = TRUE,
                      withSpinner(uiOutput("preview_iramuteq"), type = 4)
                  )
                )
              )
      ),
      
      # ── Tab 3: Processamento ────────────────────────────────────────────────
      tabItem(tabName = "process",
              conditionalPanel(
                condition = "output.file_uploaded && output.cols_selected",
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-play"), " Processar Corpus"),
                      width = 12, status = "primary", solidHeader = TRUE,
                      div(style = "text-align: center; padding: 20px;",
                          actionButton("processar_btn",
                                       tags$span(tags$i(class = "fas fa-rocket"), " Process Data"),
                                       class = "btn-primary btn-lg",
                                       style = "padding: 15px 30px; font-size: 16px;")
                      ),
                      conditionalPanel(
                        condition = "input.processar_btn > 0",
                        br(),
                        withSpinner(uiOutput("processing_status"), type = 4)
                      )
                  )
                ),
                conditionalPanel(
                  condition = "output.processing_done",
                  fluidRow(
                    box(title = tags$span(tags$i(class = "fas fa-file-code"), " Processed Corpus — Preview"),
                        width = 12, status = "success", solidHeader = TRUE,
                        div(class = "preview-box",
                            verbatimTextOutput("corpus_preview")
                        )
                    )
                  )
                )
              )
      ),
      
      # ── Tab 4: Análise ──────────────────────────────────────────────────────
      tabItem(tabName = "analise",
              conditionalPanel(
                condition = "output.processing_done",
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-chart-bar"), " Corpus Analysis"),
                      width = 6, status = "info", solidHeader = TRUE,
                      withSpinner(plotlyOutput("word_frequency", height = "400px"), type = 4)
                  ),
                  box(title = tags$span(tags$i(class = "fas fa-list-ol"), " Detailed Statistics"),
                      width = 6, status = "primary", solidHeader = TRUE,
                      withSpinner(verbatimTextOutput("detailed_stats"), type = 4)
                  )
                ),
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-file-alt"), " Processing Report"),
                      width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
                      withSpinner(verbatimTextOutput("relatorio_completo"), type = 4)
                  )
                )
              )
      ),
      
      # ── Tab 5: Download ─────────────────────────────────────────────────────
      tabItem(tabName = "download",
              conditionalPanel(
                condition = "output.processing_done",
                fluidRow(
                  box(title = tags$span(tags$i(class = "fas fa-download"), " Available Downloads"),
                      width = 12, status = "success", solidHeader = TRUE,
                      div(style = "text-align: center; padding: 30px;",
                          div(style = "display: inline-block; margin: 20px;",
                              downloadButton("download_completo",
                                             tags$div(
                                               tags$i(class = "fas fa-file-text", style = "font-size: 24px;"),
                                               tags$br(), "Full Corpus", tags$br(),
                                               tags$small("(.txt for IRaMuTeQ)")
                                             ),
                                             class = "btn-primary btn-lg",
                                             style = "padding: 20px; min-width: 200px; height: 120px;")
                          ),
                          div(style = "display: inline-block; margin: 20px;",
                              downloadButton("download_relatorio",
                                             tags$div(
                                               tags$i(class = "fas fa-chart-line", style = "font-size: 24px;"),
                                               tags$br(), "Report", tags$br(),
                                               tags$small("(full analysis)")
                                             ),
                                             class = "btn-info btn-lg",
                                             style = "padding: 20px; min-width: 200px; height: 120px;")
                          )
                      ),
                      div(class = "alert alert-info", role = "alert",
                          tags$i(class = "fas fa-info-circle"),
                          " The corpus file is ready to be imported directly into IRaMuTeQ!")
                  )
                )
              )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  # ── Reactive values ─────────────────────────────────────────────────────────
  data_original        <- reactiveVal(NULL)
  data                 <- reactiveVal(NULL)
  corpus_processado    <- reactiveVal(NULL)
  substituicoes        <- reactiveVal(NULL)
  stats_finais         <- reactiveVal(NULL)
  problemas_encontrados <- reactiveVal(NULL)
  linhas_removidas_rv  <- reactiveVal(0)
  
  # ── Flags de estado ─────────────────────────────────────────────────────────
  output$file_uploaded   <- reactive({ !is.null(data()) })
  output$cols_selected   <- reactive({ !is.null(input$cols) && length(input$cols) > 0 })
  output$processing_done <- reactive({ !is.null(corpus_processado()) })
  
  outputOptions(output, "file_uploaded",   suspendWhenHidden = FALSE)
  outputOptions(output, "cols_selected",   suspendWhenHidden = FALSE)
  outputOptions(output, "processing_done", suspendWhenHidden = FALSE)
  
  # ── Modelo de dicionário ────────────────────────────────────────────────────
  output$download_modelo_subs <- downloadHandler(
    filename = function() "modelo_dicionario_substituicoes.csv",
    content  = function(file) {
      write.csv(
        data.frame(
          original   = c("lava jato",   "corona virus", "saude publica"),
          substituto = c("lava_jato",   "coronavirus",  "saude_publica")
        ),
        file, row.names = FALSE
      )
    }
  )
  
  # ── Carregar dicionário de substituições ────────────────────────────────────
  # CORREÇÃO: normalizar 'original' igual ao pipeline do texto
  # (sem acento + minúscula) para que o fixed() case corretamente.
  observeEvent(input$dicionario_file, {
    req(input$dicionario_file)
    
    ext <- tools::file_ext(input$dicionario_file$name)
    
    df_subs <- tryCatch({
      if (ext == "csv") {
        read.csv(input$dicionario_file$datapath,
                 stringsAsFactors = FALSE, encoding = "UTF-8")
      } else {
        readxl::read_excel(input$dicionario_file$datapath)
      }
    }, error = function(e) {
      showNotification(paste("Error reading dictionary:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(df_subs)) return()
    
    df_subs <- janitor::clean_names(df_subs)
    
    if (!all(c("original", "substituto") %in% names(df_subs))) {
      showNotification("File must have columns 'original' and 'substituto'.", type = "error")
      return()
    }
    
    # Normalizar: remover acentos + minúscula na coluna 'original'
    # (a coluna 'substituto' pode conter underscore — não aplicar minúscula nela
    #  a menos que desejado; mas remover acentos é seguro)
    df_subs$original   <- str_to_lower(remover_acentos(as.character(df_subs$original)))
    df_subs$substituto <- remover_acentos(as.character(df_subs$substituto))
    
    # Remover linhas inválidas
    df_subs <- df_subs %>%
      filter(nzchar(original), nzchar(substituto))
    
    substituicoes(df_subs)
    showNotification(
      paste0("✅ Dictionary loaded: ", nrow(df_subs), " substitutions."),
      type = "message"
    )
  })
  
  # Feedback visual do dicionário
  output$dicionario_status <- renderUI({
    subs <- substituicoes()
    if (is.null(subs)) return(NULL)
    div(class = "alert alert-success", style = "margin-top: 10px;",
        tags$i(class = "fas fa-check-circle"),
        paste0(" Dictionary active: ", nrow(subs), " substitution rules loaded."),
        br(),
        tags$small("Example: '", subs$original[1], "' → '", subs$substituto[1], "'")
    )
  })
  
  # ── Modais ──────────────────────────────────────────────────────────────────
  observeEvent(input$ajuda_btn, {
    showModal(modalDialog(
      title = tags$h4(tags$i(class = "fas fa-question-circle"), " User Guide"),
      size  = "l",
      tags$ol(
        tags$li(tags$strong("Upload:"),        " Load your .xlsx, .csv or .ods file"),
        tags$li(tags$strong("Configuration:"), " Select text and metadata columns"),
        tags$li(tags$strong("Validation:"),    " Check your data quality"),
        tags$li(tags$strong("Processing:"),    " Configure options and process the corpus"),
        tags$li(tags$strong("Download:"),      " Download the formatted file for IRaMuTeQ")
      ),
      tags$hr(),
      tags$h5("💡 Custom substitutions:"),
      tags$p("The dictionary is automatically normalized (no accents, lowercase),
              so you can write 'Lava Jato' or 'lava jato' — both work."),
      easyClose = TRUE,
      footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$tutorial_btn, {
    showModal(modalDialog(
      title = tags$h4(tags$i(class = "fas fa-info-circle"), " About"),
      size  = "l",
      tags$p("🎯 This app transforms your research data into a format compatible with IRaMuTeQ"),
      tags$hr(),
      tags$div(class = "row",
               tags$div(class = "col-md-6",
                        tags$h6("✅ Accepted formats:"),
                        tags$ul(tags$li("Excel (.xlsx)"), tags$li("CSV (.csv)"), tags$li("OpenDocument (.ods)"))
               ),
               tags$div(class = "col-md-6",
                        tags$h6("🔧 What the app does:"),
                        tags$ul(
                          tags$li("Formats text for IRaMuTeQ"),
                          tags$li("Removes stopwords"),
                          tags$li("Applies custom substitutions"),
                          tags$li("Validates data quality"),
                          tags$li("Generates analysis reports")
                        )
               )
      ),
      easyClose = TRUE,
      footer = modalButton("Let's get started!")
    ))
  })
  
  observeEvent(input$edit_stopwords, {
    showModal(modalDialog(
      title = "Edit Stopwords List",
      textAreaInput("temp_stopwords", "Stopwords (one per line):",
                    value = paste(stopwords_br, collapse = "\n"),
                    height = "300px"),
      footer = tagList(modalButton("Close")),
      size = "l"
    ))
  })
  
  # ── Carregamento do arquivo de dados ────────────────────────────────────────
  observeEvent(input$file, {
    req(input$file)
    
    withProgress(message = "Loading file...", {
      tryCatch({
        incProgress(0.3, detail = "Reading file...")
        ext <- tools::file_ext(input$file$name)
        
        df <- if (ext == "csv") {
          read.csv(input$file$datapath, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
            clean_names()
        } else {
          read_excel(input$file$datapath) %>%
            clean_names()
        }
        
        df <- df %>%
          filter(if_any(everything(), ~ !(is.na(.) | . == "")))
        
        incProgress(0.7, detail = "Processing data...")
        data_original(df)
        data(df)
        incProgress(1, detail = "Done!")
        
        showNotification(
          tags$span(tags$i(class = "fas fa-check"), " File loaded successfully!"),
          type = "message", duration = 3
        )
      }, error = function(e) {
        showNotification(
          tags$span(tags$i(class = "fas fa-exclamation-triangle"),
                    " Error loading file: ", e$message),
          type = "error", duration = 5
        )
      })
    })
  })
  
  # ── Preview da tabela ───────────────────────────────────────────────────────
  output$preview_table <- DT::renderDataTable({
    req(data())
    DT::datatable(
      data(),
      options = list(
        pageLength = 5, scrollX = TRUE, dom = 'frtip',
        language = list(
          search = "Search:", lengthMenu = "Show _MENU_ entries",
          info = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate = list(previous = "Previous", `next` = "Next")
        )
      ),
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(columns = 1:ncol(data()), fontSize = '12px')
  })
  
  # ── Seletores de colunas ────────────────────────────────────────────────────
  output$col_selector <- renderUI({
    req(data())
    opcoes <- setdiff(names(data()), input$metadados)
    checkboxGroupInput("cols", "Select text columns:",
                       choices = opcoes, selected = input$cols)
  })
  
  output$meta_selector <- renderUI({
    req(data())
    opcoes <- setdiff(names(data()), input$cols)
    checkboxGroupInput("metadados", "Select metadata columns:",
                       choices = opcoes, selected = input$metadados)
  })
  
  # ── Editor de rótulos ───────────────────────────────────────────────────────
  output$label_editor <- renderUI({
    req(data(), input$cols)
    
    campos <- list()
    
    for (col in input$cols) {
      campos[[length(campos) + 1]] <- textInput(
        inputId = paste0("label_texto_", col),
        label   = paste("Label for text column:", col),
        value   = col
      )
    }
    
    for (meta in input$metadados) {
      campos[[length(campos) + 1]] <- textInput(
        inputId = paste0("label_meta_", meta),
        label   = paste("Label for metadata column:", meta),
        value   = meta
      )
    }
    
    do.call(tagList, campos)
  })
  
  # ── Converter metadados para fator ──────────────────────────────────────────
  observe({
    req(data_original())
    df_orig <- data_original()
    df_cur  <- df_orig
    
    metas <- input$metadados
    if (!is.null(metas) && length(metas) > 0) {
      metas_ok <- metas[metas %in% names(df_cur)]
      if (length(metas_ok) > 0)
        df_cur <- df_cur %>% mutate(across(all_of(metas_ok), as.factor))
    }
    
    data(df_cur)
  })
  
  # ── Value boxes ─────────────────────────────────────────────────────────────
  stats_reactive <- reactive({
    req(data(), input$cols)
    tryCatch(estatisticas_corpus(data(), input$cols),
             error = function(e) list(total_palavras = 0, vocabulario_unico = 0,
                                      media_palavras_doc = 0, total_documentos = 0))
  })
  
  output$total_docs <- renderValueBox({
    valueBox(if (!is.null(data())) nrow(data()) else 0,
             "Documents", icon = icon("file-alt"), color = "primary")
  })
  
  output$total_palavras <- renderValueBox({
    valueBox(stats_reactive()$total_palavras,
             "Total Words", icon = icon("font"), color = "success")
  })
  
  output$vocabulario_unico <- renderValueBox({
    valueBox(stats_reactive()$vocabulario_unico,
             "Unique Vocabulary", icon = icon("list"), color = "warning")
  })
  
  output$media_palavras <- renderValueBox({
    valueBox(stats_reactive()$media_palavras_doc,
             "Avg Words/Doc", icon = icon("calculator"), color = "danger")
  })
  
  # ── Validação ───────────────────────────────────────────────────────────────
  output$problemas_validacao <- renderUI({
    req(data(), input$cols)
    
    probs <- tryCatch(
      validar_dados(data(), input$cols, input$metadados),
      error = function(e) list(erro = paste("Validation error:", e$message))
    )
    problemas_encontrados(probs)
    
    if (length(probs) == 0) {
      div(class = "success-item",
          tags$i(class = "fas fa-check-circle", style = "color: #28a745;"),
          " No issues found in the data!")
    } else {
      tagList(
        tags$h6("Issues found:", style = "color: #856404;"),
        imap(probs, ~ div(class = "problem-item", .x))
      )
    }
  })
  
  # ── Distribuição de metadados ───────────────────────────────────────────────
  output$meta_distribution_ui <- renderUI({
    req(data(), input$metadados)
    cats <- keep(input$metadados, ~ .x %in% names(data()) &&
                   (is.character(data()[[.x]]) || is.factor(data()[[.x]])))
    if (length(cats) == 0)
      return(div(class = "alert alert-info", "No valid categorical metadata found"))
    selectInput("var_distribuicao", "Select variable:",
                choices = cats, selected = cats[1])
  })
  
  output$meta_distribution <- renderPlotly({
    req(data(), input$var_distribuicao)
    if (!input$var_distribuicao %in% names(data())) return(NULL)
    tryCatch({
      fd <- data() %>%
        count(.data[[input$var_distribuicao]], name = "freq") %>%
        arrange(desc(freq))
      
      plot_ly(fd, x = ~get(input$var_distribuicao), y = ~freq,
              type = "bar", marker = list(color = "#667eea"),
              text = ~paste("n =", freq), textposition = "outside") %>%
        layout(title = "Metadata distribution",
               xaxis = list(title = input$var_distribuicao),
               yaxis = list(title = "Frequency"), showlegend = FALSE)
    }, error = function(e) NULL)
  })
  
  # ── Preview IRaMuTeQ ────────────────────────────────────────────────────────
  output$preview_iramuteq <- renderUI({
    req(data(), input$cols)
    tryCatch({
      n_ex    <- min(3, nrow(data()))
      cols_pv <- input$cols[1:min(2, length(input$cols))]
      exemplos <- list()
      
      for (i in 1:n_ex) {
        for (col in cols_pv) {
          meta_str <- ""
          if (!is.null(input$metadados) && length(input$metadados) > 0) {
            mv <- map_chr(input$metadados, ~ {
              if (.x %in% names(data()) && i <= nrow(data())) {
                v <- data()[i, .x]
                if (!is.na(v)) paste0("*", .x, "_", v) else ""
              } else ""
            })
            mv <- mv[nzchar(mv)]
            if (length(mv) > 0) meta_str <- paste("", paste(mv, collapse = " "))
          }
          
          header <- paste0("**** *", col, "_", i, meta_str, "\n\n")
          
          if (col %in% names(data()) && i <= nrow(data())) {
            txt <- str_trunc(as.character(data()[i, col]), 150)
            exemplos[[length(exemplos) + 1]] <- div(
              tags$code(header, style = "color: #d63384; font-weight: bold;"),
              tags$br(),
              tags$span(txt, style = "color: #495057;"),
              tags$hr(style = "margin: 15px 0;")
            )
          }
        }
      }
      
      div(
        tags$h6("Format that will be generated for IRaMuTeQ:"),
        div(class = "preview-box", style = "max-height: 300px; overflow-y: auto;", exemplos)
      )
    }, error = function(e) {
      div(class = "alert alert-warning", "Error generating preview: ", e$message)
    })
  })
  
  # ── PROCESSAMENTO PRINCIPAL ─────────────────────────────────────────────────
  observeEvent(input$processar_btn, {
    req(data(), input$cols)
    
    config <- list(
      converter_minuscula    = input$converter_minuscula    %||% TRUE,
      remover_acentos        = input$remover_acentos        %||% TRUE,
      remover_pontuacao      = input$remover_pontuacao      %||% TRUE,
      remover_numeros        = input$remover_numeros        %||% FALSE,
      remover_espacos_extras = input$remover_espacos_extras %||% TRUE,
      tamanho_minimo         = input$tamanho_minimo         %||% 3,
      usar_stopwords         = input$usar_stopwords         %||% TRUE,
      remover_textos_curtos  = input$remover_textos_curtos  %||% FALSE,
      stopwords_custom       = input$stopwords_custom       %||% ""
    )
    
    showNotification(
      tags$span(tags$i(class = "fas fa-cog fa-spin"), " Processing corpus..."),
      id = "processing", type = "message", duration = NULL
    )
    
    withProgress(message = "Processing corpus...", {
      tryCatch({
        df <- data()
        
        cols_validas <- input$cols[input$cols %in% names(df)]
        if (length(cols_validas) == 0) stop("No valid text columns found.")
        
        df <- df %>%
          filter(if_any(all_of(cols_validas), ~ !(is.na(.) | str_trim(as.character(.)) == "")))
        
        incProgress(0.2, detail = "Preparing...")
        
        textos_formatados <- list()
        removidas         <- 0L
        
        for (col_name in cols_validas) {
          incProgress(
            0.2 + 0.6 * (match(col_name, cols_validas) / length(cols_validas)),
            detail = paste("Processing", col_name, "...")
          )
          
          rotulos_texto <- setNames(
            lapply(cols_validas, function(cn) input[[paste0("label_texto_", cn)]]),
            cols_validas
          )
          rotulos_meta <- setNames(
            lapply(input$metadados %||% character(0),
                   function(mn) input[[paste0("label_meta_", mn)]]),
            input$metadados %||% character(0)
          )
          
          resultado <- processar_coluna_otimizada(
            col_name      = col_name,
            df            = df,
            config        = config,
            metadados_cols = input$metadados,
            rotulos_texto  = rotulos_texto,
            rotulos_meta   = rotulos_meta,
            substituicoes  = substituicoes()
          )
          
          antes     <- length(resultado)
          resultado <- resultado[!is.na(resultado) & nzchar(resultado)]
          removidas <- removidas + (antes - length(resultado))
          
          if (length(resultado) > 0)
            textos_formatados <- c(textos_formatados, resultado)
        }
        
        incProgress(0.9, detail = "Finishing up...")
        
        if (length(textos_formatados) == 0)
          stop("No text segments were generated. Please check your settings.")
        
        corpus_final <- unlist(textos_formatados)
        corpus_processado(corpus_final)
        linhas_removidas_rv(removidas)
        
        stats <- estatisticas_corpus(df, cols_validas, input$metadados)
        stats_finais(stats)
        
        incProgress(1, detail = "Done!")
        removeNotification("processing")
        showNotification(
          tags$span(tags$i(class = "fas fa-check"),
                    paste0(" Corpus processed! ", length(corpus_final), " segments generated.")),
          type = "message", duration = 4
        )
        
      }, error = function(e) {
        removeNotification("processing")
        showNotification(
          tags$span(tags$i(class = "fas fa-exclamation-triangle"),
                    " Processing error: ", e$message),
          type = "error", duration = 10
        )
        message("Detailed error: ", e$message)
      })
    })
  })
  
  # ── Status do processamento ─────────────────────────────────────────────────
  output$processing_status <- renderUI({
    if (!is.null(corpus_processado())) {
      div(class = "alert alert-success",
          tags$i(class = "fas fa-check-circle"),
          " Processing complete! Go to 'Analysis' or 'Download'.")
    }
  })
  
  output$corpus_preview <- renderText({
    req(corpus_processado())
    tryCatch(
      paste(head(corpus_processado(), 3), collapse = "\n---\n"),
      error = function(e) paste("Error generating preview:", e$message)
    )
  })
  
  # ── Gráfico de frequência ───────────────────────────────────────────────────
  output$word_frequency <- renderPlotly({
    req(corpus_processado())
    tryCatch({
      fd <- corpus_processado() %>%
        paste(collapse = " ") %>%
        str_to_lower() %>%
        str_extract_all("\\b\\w{3,}\\b") %>%
        .[[1]] %>%
        tibble(palavra = .) %>%
        count(palavra, sort = TRUE) %>%
        slice_head(n = 20)
      
      plot_ly(fd, x = ~reorder(palavra, n), y = ~n, type = "bar",
              marker = list(color = "#11998e"),
              text = ~paste("n =", n), textposition = "outside") %>%
        layout(title = "Top 20 Most Frequent Words",
               xaxis = list(title = "Words"),
               yaxis = list(title = "Frequency"), showlegend = FALSE)
    }, error = function(e) NULL)
  })
  
  # ── Estatísticas detalhadas ─────────────────────────────────────────────────
  output$detailed_stats <- renderText({
    req(stats_finais(), corpus_processado())
    s <- stats_finais()
    
    paste(c(
      "=== CORPUS STATISTICS ===",
      "",
      paste0("📊 Total documents:    ", s$total_documentos),
      paste0("📝 Text columns:       ", s$colunas_texto),
      paste0("🏷️  Metadata columns:  ", s$colunas_meta),
      paste0("📖 Total words:        ", s$total_palavras),
      paste0("🔤 Unique vocabulary:  ", s$vocabulario_unico),
      paste0("📈 Avg words/doc:      ", s$media_palavras_doc),
      paste0("📄 Segments generated: ", length(corpus_processado())),
      "",
      "=== DATA QUALITY ===",
      paste0("✅ Issues found:       ", length(problemas_encontrados() %||% list())),
      paste0("🗑️  Segments removed:  ", linhas_removidas_rv()),
      paste0("🎯 Vocabulary ratio:   ",
             round((s$vocabulario_unico / max(s$total_palavras, 1)) * 100, 1), "%")
    ), collapse = "\n")
  })
  
  # ── Relatório completo ──────────────────────────────────────────────────────
  output$relatorio_completo <- renderText({
    req(data(), corpus_processado(), stats_finais())
    s <- stats_finais()
    
    paste(c(
      "╔══════════════════════════════════════════╗",
      "║          PROCESSING REPORT               ║",
      "║            IRaMuTeQ Formatator           ║",
      "╚══════════════════════════════════════════╝",
      "",
      paste("📅 Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("📁 File: ", nrow(data()), "rows ×", ncol(data()), "columns"),
      "",
      "🔧 SETTINGS:",
      paste("  • Text columns:", paste(input$cols, collapse = ", ")),
      paste("  • Metadata:", paste(input$metadados %||% "None", collapse = ", ")),
      paste("  • Lowercase:",         ifelse(input$converter_minuscula,    "✅", "❌")),
      paste("  • Remove accents:",    ifelse(input$remover_acentos,        "✅", "❌")),
      paste("  • Remove punctuation:",ifelse(input$remover_pontuacao,      "✅", "❌")),
      paste("  • Remove numbers:",    ifelse(input$remover_numeros,        "✅", "❌")),
      paste("  • Stopwords:",         ifelse(input$usar_stopwords,         "✅", "❌")),
      paste("  • Minimum length:",    input$tamanho_minimo, "characters"),
      paste("  • Substitutions:",     ifelse(!is.null(substituicoes()),
                                            paste(nrow(substituicoes()), "rules"), "None")),
      "",
      "📊 RESULTS:",
      paste("  • Segments generated:", length(corpus_processado())),
      paste("  • Total words:       ", s$total_palavras),
      paste("  • Unique vocabulary: ", s$vocabulario_unico),
      paste("  • Avg words/doc:     ", s$media_palavras_doc),
      paste("  • Segments removed:  ", linhas_removidas_rv()),
      "",
      "⚠️  ISSUES IDENTIFIED:",
      if (length(problemas_encontrados() %||% list()) == 0) {
        "  ✅ No issues found!"
      } else {
        paste("  •", unlist(problemas_encontrados()), collapse = "\n")
      },
      "",
      "✅ STATUS: Corpus ready for import into IRaMuTeQ!",
      "═══════════════════════════════════════════════"
    ), collapse = "\n")
  })
  
  # ── Downloads ───────────────────────────────────────────────────────────────
  output$download_completo <- downloadHandler(
    filename = function() paste0("corpus_iramuteq_", format(Sys.Date(), "%Y%m%d"), ".txt"),
    content  = function(file) {
      req(corpus_processado())
      writeLines(corpus_processado(), file, useBytes = TRUE)
    }
  )
  
  output$download_relatorio <- downloadHandler(
    filename = function() paste0("relatorio_iramuteq_", format(Sys.Date(), "%Y%m%d"), ".txt"),
    content  = function(file) {
      isolate({
        s <- stats_finais()
        txt <- c(
          "PROCESSING REPORT - IRaMuTeQ Formatator",
          "=================================================",
          paste("Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("Original file:", nrow(data()), "rows ×", ncol(data()), "columns"),
          "",
          "SETTINGS:",
          paste("- Text columns:", paste(input$cols, collapse = ", ")),
          paste("- Metadata:", paste(input$metadados %||% "None", collapse = ", ")),
          paste("- Substitutions:", ifelse(!is.null(substituicoes()),
                                           paste(nrow(substituicoes()), "rules"), "None")),
          "",
          "STATISTICS:",
          paste("- Segments generated:", length(corpus_processado())),
          paste("- Total words:", s$total_palavras),
          paste("- Unique vocabulary:", s$vocabulario_unico),
          paste("- Avg words/doc:", s$media_palavras_doc),
          paste("- Segments removed:", linhas_removidas_rv()),
          "",
          "STATUS: Processing completed successfully!"
        )
        writeLines(txt, file, useBytes = TRUE)
      })
    }
  )
}

# =============================================================================
shinyApp(ui, server)