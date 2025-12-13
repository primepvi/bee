;; Função auxiliar para regex de palavras inteiras
(defun bee-regexp-words (words)
  "Return a regex matching whole words in WORDS."
  (regexp-opt words 'words))

;; Palavras reservadas
(defvar bee-reserved '("var" "const" "begin" "end"))

;; Tipos
(defvar bee-types
  '("int8" "int16" "int32" "int64"
    "uint8" "uint16" "uint32" "uint64"
    "float32" "float64"
    "string" "char" "bool"))

;; Literais
(defvar bee-literals '("undefined" "true" "false" "null"))

;; Builtins e lifetimes
(defvar bee-builtins '("@static" "@alloc" "@printf" "@typeof"))

;; Operadores
(defvar bee-operators '("*" "&" "?" "/" "+" "-" "%" "c*"))

;; Font-lock keywords
(defvar bee-font-lock
  `(
    ;; comentários de linha
    ("//.*$" . font-lock-comment-face)
    ;; strings com escape
    ("\"\\(?:\\\\.\\|[^\"\\\r\n]\\)*\"" . font-lock-string-face)
    ;; floats
    ("\\b[+-]?[0-9]+\\.[0-9]+\\(?:[eE][+-]?[0-9]+\\)?\\b" . font-lock-constant-face)
    ;; inteiros
    ("\\b[+-]?[0-9]+\\b" . font-lock-constant-face)
    ;; operadores
    (,(regexp-opt bee-operators) . font-lock-builtin-face)
    ;; keywords
    (,(bee-regexp-words bee-reserved) . font-lock-keyword-face)
    ;; types
    (,(bee-regexp-words bee-types) . font-lock-type-face)
    ;; literals
    (,(bee-regexp-words bee-literals) . font-lock-constant-face)
    ;; builtins / lifetimes
    (,(bee-regexp-words bee-builtins) . font-lock-builtin-face)
    ;; identifiers (qualquer coisa que sobra)
    ("\\b[A-Za-z_][A-Za-z0-9_]*\\b" . font-lock-variable-name-face)
    ;; símbolos
    ("=" . font-lock-builtin-face)
    (":" . font-lock-builtin-face)
    (";" . font-lock-builtin-face)
    ))

;; Syntax table
(defvar bee-syntax-table
  (let ((st (make-syntax-table)))
    ;; _ e - são parte de palavras
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    ;; @ é parte de palavra
    (modify-syntax-entry ?@ "w" st)
    ;; apóstrofo como string quote
    (modify-syntax-entry ?' "\"" st)
    st)
  "Syntax table for `bee-mode`.")

;; Modo derivado
(define-derived-mode bee-mode prog-mode "Bee"
  "Major mode for editing Bee language files."
  :syntax-table bee-syntax-table
  (setq font-lock-defaults '(bee-font-lock)))
