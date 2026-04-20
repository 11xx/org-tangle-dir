;;; org-tangle-dir.el --- Org Babel `:tangle' helpers based on property value -*- lexical-binding: t; -*-

;; Author: Lucas G <g@11xx.org>
;; URL: https://codeberg.org/useless-utils/org-tangle-dir
;; Version: 2026.4.20
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:
;; This package defines helper string expansion functions that generates a
;; filepath based on the `tangle-dir' property for Org Babel Tangle, its value
;; must be a file path string or a allowed string function evaluated by the
;; helper at runtime.

;; This works because functions can be used in the `:tangle' header argument of
;; a source code block, and by defining a helper function like `tdir' that
;; retrieves the value of a inherited property, dynamic strings can be used as
;; tangle target.

;;; Code:
(defvar org-tangle-dir--whitelist-functions
  '(org-tangle-dir-base
    tdir-base
    expand-file-name
    concat
    xdg-config-home
    xdg-data-home
    xdg-cache-home
    xdg-bin-home
    xdg-config-dirs
    xdg-data-dirs
    getenv
    xdg-runtime-dir)
  "Side-effect-free functions permitted inside :tangle-dir: sexp values.
All must return strings or path components.")

(defun org-tangle-dir--safe-form-p (form)
  "Return t if FORM is safe to evaluate as a :tangle-dir: expression.
Safe means: a self-evaluating atom, or a list whose car is in
`org-tangle-dir--whitelist-functions' and whose every argument is also safe."
  (cond
   ((stringp form)          t)
   ((numberp form)          t)
   ((memq form '(t nil))    t)
   ((keywordp form)         t)
   ((and (consp form)
         (symbolp (car form))
         (memq (car form) org-tangle-dir--whitelist-functions)
         (cl-every #'org-tangle-dir--safe-form-p (cdr form)))
    t)
   (t nil)))

(defun org-tangle-dir--eval-sexp (string)
  "Safely evaluate STRING as a :tangle-dir: property value.
Returns STRING as-is if it does not start with '('."
  (let ((s (string-trim string)))
    (if (not (string-prefix-p "(" s))
        s
      (let ((form (condition-case err
                      (read s)
                    (error (user-error
                            "tdir: malformed sexp in :tangle-dir: %s — %s" s err)))))
        (unless (org-tangle-dir--safe-form-p form)
          (user-error
           "tdir: unsafe form in :tangle-dir: %s\n  Only %s with literal/whitelisted args are permitted"
           form org-tangle-dir--whitelist-functions))
        (let ((result (eval form t)))
          (unless (stringp result)
            (user-error "tdir: :tangle-dir: sexp must return a string, got: %S" result))
          result)))))

(defvar org-tangle-dir--resolving nil
  "Stack of heading positions currently being resolved.
Used to detect circular :tangle-dir: references.")

(defun org-tangle-dir--effective-dir ()
  "Resolve :tangle-dir: for the current heading without Org's built-in
property inheritance, to retain full control over sexp evaluation.

- Plain string → returned directly.
- Sexp         → validated and evaluated; circular refs cause a user-error.
- Missing      → walks up the outline tree recursively."
  (let* ((raw (org-entry-get nil "tangle-dir" nil))
         (pos (save-excursion (org-back-to-heading t) (point))))
    (cond
     ((and raw (not (string-prefix-p "(" (string-trim raw))))
      (string-trim raw nil "/"))

     (raw
      (when (memq pos org-tangle-dir--resolving)
        (user-error
         "tdir: circular :tangle-dir: at '%s' — use `org-tangle-dir-base' in properties, not `tdir'"
         (org-entry-get nil "ITEM")))
      (let ((org-tangle-dir--resolving (cons pos org-tangle-dir--resolving)))
        (string-trim (org-tangle-dir--eval-sexp (string-trim raw)) nil "/")))

     (t
      (save-excursion
        (unless (org-up-heading-safe)
          (user-error "tdir: no :tangle-dir: property found in heading hierarchy"))
        (org-tangle-dir--effective-dir))))))

(defun org-tangle-dir-base (&optional subdir)
  "Return the parent heading's effective tangle-dir, joined with SUBDIR.
Use in :tangle-dir: property values for hierarchy-relative paths:

  :tangle-dir: (org-tangle-dir-base \"tasks\")

For externally-rooted paths, use expand-file-name directly:

  :tangle-dir: (expand-file-name \"nnn/plugins\" (xdg-config-home))"
  (save-excursion
    (unless (org-up-heading-safe)
      (user-error "org-tangle-dir-base: no parent heading"))
    (let ((parent-dir (org-tangle-dir--effective-dir)))
      (if subdir
          (expand-file-name (string-trim subdir "/" nil) parent-dir)
        parent-dir))))

(defun org-tangle-dir (&optional path)
  "Return the effective tangle directory for the current Org entry,
optionally joined with PATH via `expand-file-name'.

Use in :tangle src block headers:
  :tangle (tdir \"filename.yml\")

For :tangle-dir: property values, use `org-tangle-dir-base' or `expand-file-name'."
  (let ((dir (org-tangle-dir--effective-dir)))
    (if path
        (expand-file-name (string-trim path "/" nil) dir)
      dir)))

;; shorter aliases, ignore package-lint!
(defalias 'tdir #'org-tangle-dir)
(defalias 'tdir-base #'org-tangle-dir-base)

(provide 'org-tangle-dir)
;;; org-tangle-dir.el ends here
