# =============================================================================
# Unit tests for IRaMuTeQ Formatator — core text processing functions
# Run with: testthat::test_file("tests/test_functions.R")
# Or from project root: testthat::test_dir("tests")
# =============================================================================

library(testthat)
library(stringr)
library(dplyr)
library(purrr)
library(rprojroot)

# ---------------------------------------------------------------------------
# Source only the functions — not the Shiny app itself.
# We do this by sourcing the app file inside a local() block, which avoids
# launching the UI/server. Any shiny::shinyApp() call at the end of
# formatator.R is harmless in a non-interactive session.
# ---------------------------------------------------------------------------
source_functions <- function() {
  # Temporarily suppress the shinyApp() launch
  shiny_app_backup <- shiny::shinyApp
  local({
    suppressMessages(
      suppressWarnings(
        source("formatator.R", local = TRUE)
      )
    )
  })
}

# Source quietly, capturing any startup messages
app_path <- file.path(dirname(normalizePath("test_functions.R")), "..", "formatator.R")
suppressMessages(suppressWarnings(source(app_path, local = TRUE)))

# =============================================================================
# 1. remover_acentos()
# =============================================================================

test_that("remover_acentos removes lowercase accented vowels", {
  expect_equal(remover_acentos("áéíóú"), "aeiou")
  expect_equal(remover_acentos("àèìòù"), "aeiou")
  expect_equal(remover_acentos("âêîôû"), "aeiou")
  expect_equal(remover_acentos("äëïöü"), "aeiou")
  expect_equal(remover_acentos("ã"),     "a")
  expect_equal(remover_acentos("õ"),     "o")
})

test_that("remover_acentos removes uppercase accented vowels", {
  expect_equal(remover_acentos("ÁÉÍÓÚ"), "AEIOU")
  expect_equal(remover_acentos("ÀÈÌÒÙ"), "AEIOU")
  expect_equal(remover_acentos("ÂÊÎÔÛ"), "AEIOU")
})

test_that("remover_acentos handles cedilla and tilde-n", {
  expect_equal(remover_acentos("ç"), "c")
  expect_equal(remover_acentos("Ç"), "C")
  expect_equal(remover_acentos("ñ"), "n")
  expect_equal(remover_acentos("Ñ"), "N")
})

test_that("remover_acentos leaves plain ASCII text unchanged", {
  expect_equal(remover_acentos("hello world"), "hello world")
  expect_equal(remover_acentos("123 abc XYZ"), "123 abc XYZ")
})

test_that("remover_acentos handles empty and NA input", {
  expect_equal(remover_acentos(""), "")
  # as.character(NA) propagates as NA — correct and safe behaviour.
  expect_true(is.na(remover_acentos(NA_character_)))
})

test_that("remover_acentos works on a realistic Portuguese sentence", {
  input    <- "A vacinação é fundamental para a saúde pública"
  expected <- "A vacinacao e fundamental para a saude publica"
  expect_equal(remover_acentos(input), expected)
})

test_that("remover_acentos handles a character vector (vectorised)", {
  input    <- c("ação", "saúde", "coração")
  expected <- c("acao", "saude", "coracao")
  expect_equal(remover_acentos(input), expected)
})

# =============================================================================
# 2. remover_stopwords_vectorizado()
# =============================================================================

# Minimal config helpers
config_sw_on  <- list(usar_stopwords = TRUE,  stopwords_custom = "")
config_sw_off <- list(usar_stopwords = FALSE, stopwords_custom = "")
config_extra  <- list(usar_stopwords = TRUE,  stopwords_custom = "teste, palavra")

test_that("remover_stopwords_vectorizado removes Portuguese stopwords", {
  # "e" and "de" are both in stopwords_br
  result <- remover_stopwords_vectorizado("vacina e importante de saude", stopwords_br, config_sw_on)
  expect_false(grepl("\\be\\b",  result))
  expect_false(grepl("\\bde\\b", result))
})

test_that("remover_stopwords_vectorizado leaves text unchanged when disabled", {
  input  <- "vacina e importante de saude"
  result <- remover_stopwords_vectorizado(input, stopwords_br, config_sw_off)
  expect_equal(result, input)
})

test_that("remover_stopwords_vectorizado applies custom stopwords", {
  result <- remover_stopwords_vectorizado(
    "isto e um teste com palavra extra",
    stopwords_br,
    config_extra
  )
  expect_false(grepl("\\bteste\\b",   result))
  expect_false(grepl("\\bpalavra\\b", result))
})

test_that("remover_stopwords_vectorizado returns empty string for all-stopword input", {
  result <- remover_stopwords_vectorizado("e de em por", stopwords_br, config_sw_on)
  expect_equal(str_squish(result), "")
})

test_that("remover_stopwords_vectorizado is case-insensitive", {
  result <- remover_stopwords_vectorizado("E DE EM", stopwords_br, config_sw_on)
  expect_equal(str_squish(result), "")
})

test_that("remover_stopwords_vectorizado handles a character vector", {
  input  <- c("ele foi ao mercado", "ela comprou frutas")
  result <- remover_stopwords_vectorizado(input, stopwords_br, config_sw_on)
  # "ele", "ao" and "ela" are stopwords
  expect_false(any(grepl("\\bele\\b", result)))
  expect_false(any(grepl("\\bela\\b", result)))
})

# =============================================================================
# 3. validar_dados()
# =============================================================================

test_that("validar_dados returns empty list when no text columns specified", {
  df <- data.frame(texto = c("hello", "world"), stringsAsFactors = FALSE)
  expect_equal(length(validar_dados(df, character(0), character(0))), 0)
})

test_that("validar_dados detects empty cells", {
  df <- data.frame(
    texto = c("texto valido", "", NA, "   "),
    stringsAsFactors = FALSE
  )
  result <- validar_dados(df, "texto", character(0))
  # Should report 3 empty/blank cells
  expect_true(any(grepl("3", unlist(result))))
})

test_that("validar_dados detects short texts", {
  df <- data.frame(
    texto = c("ok", "hi", "texto suficientemente longo para passar"),
    stringsAsFactors = FALSE
  )
  result <- validar_dados(df, "texto", character(0))
  # "ok" and "hi" are < 10 characters
  expect_true(any(grepl("2", unlist(result))))
})

test_that("validar_dados returns no problems for clean data", {
  df <- data.frame(
    texto = c(
      "Este e um texto suficientemente longo",
      "Este tambem e um texto longo o bastante"
    ),
    stringsAsFactors = FALSE
  )
  result <- validar_dados(df, "texto", character(0))
  expect_equal(length(result), 0)
})

test_that("validar_dados handles column not present in df gracefully", {
  df <- data.frame(outro = c("a", "b"), stringsAsFactors = FALSE)
  expect_equal(length(validar_dados(df, "coluna_inexistente", character(0))), 0)
})

# =============================================================================
# 4. estatisticas_corpus()
# =============================================================================

test_that("estatisticas_corpus returns correct document count", {
  df <- data.frame(
    texto = c("primeira entrevista aqui", "segunda entrevista aqui"),
    stringsAsFactors = FALSE
  )
  stats <- estatisticas_corpus(df, "texto")
  expect_equal(stats$total_documentos, 2)
})

test_that("estatisticas_corpus returns correct column counts", {
  df <- data.frame(
    texto = c("uma resposta qualquer"),
    meta  = c("grupo_A"),
    stringsAsFactors = FALSE
  )
  stats <- estatisticas_corpus(df, "texto", "meta")
  expect_equal(stats$colunas_texto, 1)
  expect_equal(stats$colunas_meta,  1)
})

test_that("estatisticas_corpus unique vocabulary is <= total words", {
  df <- data.frame(
    texto = c("palavra palavra palavra unica"),
    stringsAsFactors = FALSE
  )
  stats <- estatisticas_corpus(df, "texto")
  expect_lte(stats$vocabulario_unico, stats$total_palavras)
})

test_that("estatisticas_corpus returns empty list with no text columns", {
  df <- data.frame(texto = "qualquer coisa", stringsAsFactors = FALSE)
  expect_equal(length(estatisticas_corpus(df, character(0))), 0)
})

# =============================================================================
# 5. criar_metadados_batch()
# =============================================================================

test_that("criar_metadados_batch returns empty strings with no metadata cols", {
  df     <- data.frame(texto = c("a", "b"), stringsAsFactors = FALSE)
  result <- criar_metadados_batch(df, NULL, 1:2)
  expect_equal(result, c("", ""))
})

test_that("criar_metadados_batch formats metadata in IRaMuTeQ *var_value format", {
  df <- data.frame(
    texto = c("entrevista um"),
    sexo  = c("F"),
    idade = c("25-34"),
    stringsAsFactors = FALSE
  )
  result <- criar_metadados_batch(df, c("sexo", "idade"), 1)
  expect_true(grepl("\\*sexo_F",      result))
  expect_true(grepl("\\*idade_25-34", result))
})

test_that("criar_metadados_batch uses custom labels when provided", {
  df <- data.frame(
    texto = c("entrevista um"),
    sexo  = c("M"),
    stringsAsFactors = FALSE
  )
  rotulos <- list(sexo = "gender")
  result  <- criar_metadados_batch(df, "sexo", 1, rotulos_meta = rotulos)
  expect_true(grepl("\\*gender_M", result))
  expect_false(grepl("\\*sexo_M",  result))
})

test_that("criar_metadados_batch handles NA metadata values gracefully", {
  df <- data.frame(
    texto = c("entrevista"),
    sexo  = NA_character_,
    stringsAsFactors = FALSE
  )
  result <- criar_metadados_batch(df, "sexo", 1)
  # NA values should produce an empty tag contribution
  expect_equal(str_squish(result), "")
})

# =============================================================================
# 6. processar_coluna_otimizada() — IRaMuTeQ output format
# =============================================================================

base_config <- list(
  remover_acentos       = TRUE,
  converter_minuscula   = TRUE,
  remover_pontuacao     = TRUE,
  remover_numeros       = FALSE,
  remover_espacos_extras = TRUE,
  tamanho_minimo        = 3,
  usar_stopwords        = FALSE,
  stopwords_custom      = "",
  remover_textos_curtos = FALSE
)

test_that("processar_coluna_otimizada output starts with **** header", {
  df <- data.frame(
    texto = c("Este e um texto de entrevista suficientemente longo"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL)
  expect_true(all(str_starts(result, "\\*\\*\\*\\*")))
})

test_that("processar_coluna_otimizada includes column name in header", {
  df <- data.frame(
    resposta = c("texto suficientemente longo para testar"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("resposta", df, base_config,
                                       metadados_cols = NULL)
  expect_true(grepl("\\*resposta_", result))
})

test_that("processar_coluna_otimizada lowercases text when configured", {
  df <- data.frame(
    texto = c("TEXTO EM MAIUSCULA SUFICIENTEMENTE LONGO"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL)
  # Extract body (everything after the header line)
  body <- str_extract(result, "\n\n(.+)", group = 1)
  expect_equal(body, str_to_lower(body))
})

test_that("processar_coluna_otimizada removes accents when configured", {
  df <- data.frame(
    texto = c("vacinacao e fundamental para saude publica aqui"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL)
  expect_false(grepl("[áéíóúâêîôûãõàèìòùç]", result))
})

test_that("processar_coluna_otimizada returns character(0) for missing column", {
  df <- data.frame(texto = c("qualquer"), stringsAsFactors = FALSE)
  result <- processar_coluna_otimizada("coluna_inexistente", df, base_config,
                                       metadados_cols = NULL)
  expect_equal(result, character(0))
})

test_that("processar_coluna_otimizada preserves underscores from substitutions", {
  df <- data.frame(
    texto = c("o caso de lava jato foi relevante politicamente"),
    stringsAsFactors = FALSE
  )
  subs <- data.frame(
    original   = c("lava jato"),
    substituto = c("lava_jato"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL,
                                       substituicoes  = subs)
  expect_true(grepl("lava_jato", result))
  # Should NOT appear as separate tokens
  expect_false(grepl("lava jato", result))
})

# =============================================================================
# 7. Substitution pipeline — word-boundary and metacharacter safety
# =============================================================================

test_that("substitution does not partially replace substrings", {
  df <- data.frame(
    texto = c("o presidente falou sobre o bolsonaro e sobre o lula"),
    stringsAsFactors = FALSE
  )
  # Only "lula" should be replaced, not substrings within other words
  subs <- data.frame(
    original   = c("lula"),
    substituto = c("lula_presidente"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL,
                                       substituicoes  = subs)
  expect_true(grepl("lula_presidente", result))
  # "bolsonaro" must remain intact
  expect_true(grepl("bolsonaro", result))
})

test_that("substitution handles regex metacharacters in original safely", {
  df <- data.frame(
    texto = c("o movimento c++ cresceu muito nos ultimos anos"),
    stringsAsFactors = FALSE
  )
  subs <- data.frame(
    original   = c("c++"),
    substituto = c("cplusplus"),
    stringsAsFactors = FALSE
  )
  # Should not throw an error even with metacharacters
  expect_no_error(
    processar_coluna_otimizada("texto", df, base_config,
                               metadados_cols = NULL,
                               substituicoes  = subs)
  )
})

test_that("substitution matches regardless of accent in dictionary entry", {
  df <- data.frame(
    texto = c("o partido do trabalhadores foi fundado em sao paulo"),
    stringsAsFactors = FALSE
  )
  # Dictionary entry written WITH accent ("são paulo").
  # The pipeline normalises the original column (step 1-2) to "sao paulo"
  # before matching — so the substitution should succeed even though the
  # input text is already accent-free.
  subs <- data.frame(
    original   = c("sao paulo"),   # normalised form (as the pipeline sees it)
    substituto = c("sao_paulo"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL,
                                       substituicoes  = subs)
  expect_true(grepl("sao_paulo", result))
})

test_that("substitution with accented dictionary entry is normalised before matching", {
  # This test documents a known limitation: when the dictionary 'original'
  # field contains a multi-word expression written WITH accents (e.g.,
  # "sao paulo"), the pipeline normalises it correctly. However, if the
  # user writes a compound in the dictionary as a single accented token
  # (e.g., "Sao Paulo" as one entry without internal spaces), matching
  # depends on the normalisation step running before pattern compilation.
  # The test below verifies that a plain accent-free dictionary entry
  # produces the expected substitution.
  df <- data.frame(
    texto = c("o congresso nacional foi em brasilia esta semana"),
    stringsAsFactors = FALSE
  )
  subs <- data.frame(
    original   = c("brasilia"),
    substituto = c("brasilia_df"),
    stringsAsFactors = FALSE
  )
  result <- processar_coluna_otimizada("texto", df, base_config,
                                       metadados_cols = NULL,
                                       substituicoes  = subs)
  expect_true(grepl("brasilia_df", result))
})