# Emacs config

Emacs config based on
- Emacs Bedrock: https://codeberg.org/ashton314/emacs-bedrock
- Magnar Sveen's config: https://github.com/magnars/emacsd.reboot/

## Installation (for macos)

Install Emacs from emacsformacos.com.

- ripgrep - used by the `rg` package

  ```sh
  brew install ripgrep
  ```

- git-when-merged

  Used by magit's `magit-log-merged`, which is useful for looking up a
  commit in the log and seeing it with context (commits before and
  after).

  ```sh
  brew install git-when-merged
  ```

- language servers for html/css

  ```sh
  npm i -g vscode-langservers-extracted
  ```
  
  https://github.com/hrsh7th/vscode-langservers-extracted

- language server for Clojure

  ```sh
  brew install clojure-lsp/brew/clojure-lsp-native
  ```

  https://clojure-lsp.io/installation/#homebrew-macos-and-linux

## Config

Base components:
- [Vertico](https://github.com/minad/vertico): minibuffer completion, instead of ido
- [Consult](https://github.com/minad/consult): search and navigation
- [Avy](https://github.com/abo-abo/avy): jump in buffer
- [Embark](https://github.com/oantolin/embark): invoke commands on whatever's at point)
- [Marginalia](https://github.com/minad/marginalia): minibuffer decorations/docs
- [Corfu](https://github.com/minad/corfu): completion popups, instead of company
- [Cape](https://github.com/minad/cape): completion extensions

## Treesitter

Treesitter provide ASTs and syntax highlighting for a wide variety of
langs, and can be utilized along with LSP server for providing editing
capabilities (navigation, refactoring).

Treesitter grammars must be installed on a per-lang basis, and are
setup in [here](./settings/setup-treesitter), or can be installed manually using:

```
M-x treesitter-install-language-grammar
```

See https://tree-sitter.github.io/tree-sitter/#language-bindings

## Web development with eglot and lsp

- [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html):
  using the builtin language server integration instead of
  [lsp-mode](https://emacs-lsp.github.io/lsp-mode/), as the former is a
  more focused tool and seems to be better integrated with the existing
  ecosystem of Emacs (e.g. xref). It does, however, lack the ability to
  have multiple language servers for a single workspace, meaning that
  you have to forgo [tailwindcss](https://tailwindcss.com/) completions
  along with whatever language you're editing. There are efforts to get
  server multiplexing in place, though.
- [breadcrumb-mode](https://github.com/joaotavora/breadcrumb): providing
  a simple breadcrumb menu at the top of the buffer, and providing a
  really nice symbol outline (`M-x breadcrumb-jump`) that lets you
  quickly navigate to any symbol in the file (as opposed to lsp-mode's
  imenu integration which provides a hierarchical menu which in turn
  prevents you from navigating to top-level symbols - the only thing you
  really want in a js/ts file)
- [Combobulate](https://github.com/mickeynp/combobulate): structural
  editing with good support for Typescript/TSX
- [jtsx](https://github.com/llemaitre19/jtsx): a jsx/tsx editing mode
  that overlaps with Combobulate, but it has one great feature that
  Combobulate's missing: auto-syncing element tags

  > As soon as the name of an opening or a closing tag is edited, its paired tag is immediately synchronized.
  
  https://github.com/llemaitre19/jtsx?tab=readme-ov-file#renaming-elements

