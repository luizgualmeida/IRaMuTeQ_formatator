# Changelog

All notable changes to IRaMuTeQ Formatator are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

- Automated tests for core text processing functions
- GitHub Actions workflow for JOSS paper compilation

---

## [0.3.0] - 2026-06-14

### Added
- Full English-language interface (bilingual release: PT + EN)
- Downloadable template file for the substitution dictionary
- Visual feedback panel showing loaded dictionary rules with an example substitution

### Changed
- Processing pipeline reordered for correctness: accent removal and lowercasing now always run **before** custom substitutions, ensuring dictionary entries match regardless of input encoding or capitalisation
- `furrr` and `future` dependencies removed; parallel processing replaced with a simpler sequential pipeline that is more portable and easier to install

### Fixed
- Critical bug in custom substitutions: partial-word matches (e.g., replacing `lava` when the target was `lava jato`) now correctly use word-boundary regex (`\\b`), preventing spurious replacements
- Cascading substitution bug: each rule is now applied to the original (normalised) text rather than to the output of the previous rule, eliminating unintended chain replacements
- Dictionary normalisation: the `original` column is now automatically lowercased and stripped of accents before matching, so entries written as `"Lava Jato"` or `"lava jato"` both work correctly

---

## [0.2.0] - 2025-11-01

### Added
- Custom substitution dictionary: users can upload a `.csv` or `.xlsx` file with `original` and `substituto` columns to merge multi-word expressions (e.g., `lava jato` → `lava_jato`) or standardise spelling variants
- Support for `.ods` (OpenDocument Spreadsheet) input files in addition to `.xlsx` and `.csv`
- Editable column labels: users can rename columns for the IRaMuTeQ header without modifying the source file
- Metadata distribution chart (interactive bar chart via `plotly`) in the Validation tab
- Full processing report available for download as `.txt`
- `remover_textos_curtos` toggle to filter documents shorter than 10 characters or 5 words

### Changed
- Text processing refactored to use vectorised `stringr` operations throughout, replacing earlier character-by-character loops
- Stopword removal now uses a single compiled regex pattern for efficiency
- Validation messages now include counts of empty cells and short texts per column

---

## [0.1.0] - 2025-07-27

### Added
- Initial Shiny application with `bs4Dash` dashboard layout
- File upload supporting `.xlsx` and `.csv` formats
- Column selector for text columns and metadata columns
- Text preprocessing pipeline: lowercase conversion, accent removal, punctuation removal, number removal, extra-space cleanup, minimum word length filter
- Built-in Portuguese stopword list covering articles, pronouns, prepositions, conjunctions, common verbs, colloquialisms, and internet abbreviations
- Additional custom stopwords via comma-separated text input
- IRaMuTeQ-format corpus output: `****` document headers with `*variable_value` metadata tags
- Interactive word frequency chart (Top 20 most frequent words)
- Corpus statistics: total documents, total words, unique vocabulary, average words per document
- Download button for the formatted corpus (`.txt`) ready for IRaMuTeQ import
- Example dataset (`materias_com_textos.csv`): news articles covering the Dark Horse documentary funding case, for testing and demonstration purposes

---

[Unreleased]: https://github.com/luizgualmeida/IRaMuTeQ_formatator/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/luizgualmeida/IRaMuTeQ_formatator/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/luizgualmeida/IRaMuTeQ_formatator/compare/v0.1.0...v0.2.0
# [0.1.0]: https://github.com/luizgualmeida/IRaMuTeQ_formatator/releases/tag/v0.1.0