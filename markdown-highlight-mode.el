(setq markdown-highlight-indent-offset 2)
(setq markdown-highlight-blank-line-re "^ *$")

(defun markdown-highlight-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (while (and (looking-at markdown-highlight-blank-line-re)
                (> (point) (point-min)))
      (forward-line -1))
    (+ (current-indentation)
       (if (looking-at " *\\*") markdown-highlight-indent-offset 0))))

(defun markdown-highlight-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `markdown-highlight-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (markdown-highlight-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) markdown-highlight-indent-offset) markdown-highlight-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(setq markdown-highlight-keywords
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

(define-derived-mode markdown-highlight-mode fundamental-mode
  (setq font-lock-defaults '(markdown-highlight-keywords))
  (setq mode-name "My mode")
  (set (make-local-variable 'indent-line-function) 'markdown-highlight-indent-line)
)
