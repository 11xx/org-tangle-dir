;; -*- lexical-binding: t; -*-
(defvar 11xx--tdir-allowed-functions
  '(tdir-base
    expand-file-name
    concat
    xdg-config-home
    xdg-data-home
    xdg-cache-home
    xdg-bin-home
    xdg-config-dirs
    xdg-data-dirs
    getenv
    xdg-runtime-dir
    )
  "Side-effect-free functions permitted inside :tangle-dir: sexp values.
All must return strings or path components.")

(defun f/tdir-safe-form-p (form)
  "Return t if FORM is safe to evaluate as a :tangle-dir: expression.
Safe means: a self-evaluating atom, or a list whose car is in
`11xx--tdir-allowed-functions' and whose every argument is also safe."
  (cond
   ((stringp form)          t)
   ((numberp form)          t)
   ((memq form '(t nil))    t)
   ((keywordp form)         t)
   ((and (consp form)
         (symbolp (car form))
         (memq (car form) 11xx--tdir-allowed-functions)
         (cl-every #'f/tdir-safe-form-p (cdr form)))
    t)
   (t nil)))

(defun f/eval-tdir-sexp (string)
  "Safely evaluate STRING as a :tangle-dir: property value.
Returns STRING as-is if it does not start with '('."
  (let ((s (string-trim string)))
    (if (not (string-prefix-p "(" s))
        s
      (let ((form (condition-case err
                      (read s)
                    (error (user-error
                            "tdir: malformed sexp in :tangle-dir: %s — %s" s err)))))
        (unless (f/tdir-safe-form-p form)
          (user-error
           "tdir: unsafe form in :tangle-dir: %s\n  Only %s with literal/whitelisted args are permitted"
           form 11xx--tdir-allowed-functions))
        (let ((result (eval form t)))
          (unless (stringp result)
            (user-error "tdir: :tangle-dir: sexp must return a string, got: %S" result))
          result)))))

(defvar tdir--resolving nil
  "Stack of heading positions currently being resolved.
Used to detect circular :tangle-dir: references.")

(defun tdir--effective-dir ()
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
      (when (memq pos tdir--resolving)
        (user-error
         "tdir: circular :tangle-dir: at '%s' — use `tdir-base' in properties, not `tdir'"
         (org-entry-get nil "ITEM")))
      (let ((tdir--resolving (cons pos tdir--resolving)))
        (string-trim (f/eval-tdir-sexp (string-trim raw)) nil "/")))

     (t
      (save-excursion
        (unless (org-up-heading-safe)
          (user-error "tdir: no :tangle-dir: property found in heading hierarchy"))
        (tdir--effective-dir))))))

(defun tdir-base (&optional subdir)
  "Return the parent heading's effective tangle-dir, joined with SUBDIR.
Use in :tangle-dir: property values for hierarchy-relative paths:

  :tangle-dir: (tdir-base \"tasks\")

For externally-rooted paths, use expand-file-name directly:

  :tangle-dir: (expand-file-name \"nnn/plugins\" (xdg-config-home))"
  (save-excursion
    (unless (org-up-heading-safe)
      (user-error "tdir-base: no parent heading"))
    (let ((parent-dir (tdir--effective-dir)))
      (if subdir
          (expand-file-name (string-trim subdir "/" nil) parent-dir)
        parent-dir))))

(defun tdir (&optional path)
  "Return the effective tangle directory for the current Org entry,
optionally joined with PATH via `expand-file-name'.

Use in :tangle src block headers:
  :tangle (tdir \"filename.yml\")

For :tangle-dir: property values, use `tdir-base' or `expand-file-name'."
  (let ((dir (tdir--effective-dir)))
    (if path
        (expand-file-name (string-trim path "/" nil) dir)
      dir)))

(defun f/tdir-set-heading-property ()
  "Set :tangle-dir: for the current heading to (tdir-base \"SLUG\").
SLUG is derived from the heading title and confirmed in the minibuffer."
  (interactive)
  (let* ((title (substring-no-properties (org-entry-get nil "ITEM")))
         (slug  (thread-last title
                  (downcase)
                  (replace-regexp-in-string "[[:space:]]+" "-")
                  (replace-regexp-in-string "[^a-z0-9_-]" "")
                  (replace-regexp-in-string "-+" "-")
                  (string-trim "-")))
         (slug  (read-string "Slug for tdir-base: " slug))
         (value (format "(tdir-base \"%s\")" slug)))
    (org-set-property "tangle-dir" value)
    (message "Set :tangle-dir: %s" value)))

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c C-x T" #'f/tdir-set-heading-property))
