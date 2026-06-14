# Contributing to IRaMuTeQ Formatator

Thank you for your interest in contributing to IRaMuTeQ Formatator! This is a solo-maintained project, but external contributions, bug reports, and suggestions are very welcome.

---

## 📬 How to get in touch

**Maintainer:** Luiz Gustavo de Almeida
**E-mail:** luizgualmeida@gmail.com
**GitHub:** [@luizgualmeida](https://github.com/luizgualmeida)

For questions, suggestions, or general discussion, please open a [GitHub Issue](https://github.com/luizgualmeida/IRaMuTeQ_formatator/issues).

---

## 🐛 Reporting bugs

If you found a bug, please open an issue and include:

- A clear and descriptive title
- Steps to reproduce the problem
- What you expected to happen vs. what actually happened
- Your R version (`R.version`) and operating system
- If possible, a minimal example file (`.csv` or `.xlsx`) that triggers the issue

---

## 💡 Suggesting features or improvements

Feature requests are welcome! Open an issue with:

- A clear description of the proposed feature
- Why it would be useful for IRaMuTeQ users
- Any examples or references to similar tools, if applicable

---

## 🔧 Contributing code

If you'd like to fix a bug or implement a feature:

1. **Fork** the repository on GitHub
2. **Create a new branch** with a descriptive name:
   ```
   git checkout -b fix/stopword-encoding-issue
   ```
3. **Make your changes** in `formatator.R` (or other relevant files)
4. **Test** the app locally by running `shiny::runApp("formatator.R")`
5. **Open a Pull Request** against the `main` branch with a clear description of what you changed and why

Please keep pull requests focused — one fix or feature per PR is ideal.

---

## 📋 Code style guidelines

- Follow base R and tidyverse conventions where possible
- Use meaningful variable names (English preferred for new code)
- Add inline comments for non-obvious logic
- Avoid adding new package dependencies unless strictly necessary — if you do, update the `install.packages()` block in `README.md`

---

## 🧪 Testing your changes

Before submitting a pull request, please verify that:

- [ ] The app launches without errors (`shiny::runApp("formatator.R")`)
- [ ] File upload works for `.csv`, `.xlsx`, and `.ods` formats
- [ ] The corpus output follows the IRaMuTeQ `****` header format correctly
- [ ] The download buttons produce valid `.txt` files
- [ ] You tested with the included `materias_com_textos.csv` example file

---

## 📝 Documentation contributions

Improvements to the README (typos, clearer instructions, additional examples) are also very welcome. Just open a Pull Request with your proposed changes.

---

## 🤝 Code of Conduct

This project follows a simple standard: be respectful and constructive. Harassment or dismissive behavior will not be tolerated. If you experience any issues, contact the maintainer directly at luizgualmeida@gmail.com.

---

*Thank you for helping make IRaMuTeQ Formatator better for the research community!*