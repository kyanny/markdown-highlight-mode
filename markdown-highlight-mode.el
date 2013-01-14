(setq myKeywords
 '(
   ;; Emphasis
   ("\\*.+?\\*" . font-lock-function-name-face)
   ("_.+?_" . font-lock-function-name-face)
   ("\\*\\*.+?\\*\\*" . font-lock-function-name-face)
   ("__.+?__" . font-lock-function-name-face)

   ;; Code
   ("^    .*" . font-lock-constant-face)
   ("`.+?`" . font-lock-constant-face)

   ;; List
   ("^* " . font-lock-constant-face)

   ;; Headers
   ("^# .*" . font-lock-function-name-face)
   ("^## .*" . font-lock-function-name-face)
   ("^### .*" . font-lock-function-name-face)
   ("^#### .*" . font-lock-function-name-face)
   ("^##### .*" . font-lock-function-name-face)
   ("^###### .*" . font-lock-function-name-face)

   ;; Blockquote
   ("^> .*" . font-lock-constant-face)

   ;; Link
   ("\\[.*\\]\(.*\)" . font-lock-constant-face)
   ("\\[.*\\]\\[.*\\]" . font-lock-constant-face)
   ;; ("\\[.*\\]: \S+ \\".*?\\"" . font-lock-constant-face)

   ;; Horizontal Line
   ("^---+$" . font-lock-constant-face)
   ("^***+$" . font-lock-constant-face)
   ("^___+$" . font-lock-constant-face)
  )
)

(define-derived-mode my-mode fundamental-mode
  (setq font-lock-defaults '(myKeywords))
  (setq mode-name "My mode")
)
