;;; org-tangle-dir-tests.el --- tests for org-tangle-dir -*- lexical-binding: t; -*-

;; Run tests:
;; λ emacs -Q --batch --eval "(progn (require 'org) (require 'cl-lib) (load-file \"org-tangle-dir.el\") (load-file \"org-tangle-dir-tests.el\") (ert-run-tests-batch-and-exit))"
;; Or from the repo dir
;; λ emacs -Q --batch -l org-tangle-dir.el -l org-tangle-dir-tests.el --eval "(ert-run-tests-batch-and-exit)"

(require 'org-tangle-dir)
(require 'ert)

(defmacro org-tangle-dir-test-with-temp-text (text &rest body)
  "Create temp buffer with TEXT in org-mode, then evaluate BODY at heading."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-max))
     (when (re-search-backward "^\\*+" nil t)
       ,@body)))

(ert-deftest org-tangle-dir-test/file-level-plain ()
  "File-level #+PROPERTY with plain path."
  (org-tangle-dir-test-with-temp-text "#+PROPERTY: tangle-dir /tmp\n* H\n"
    (should (string= (org-tangle-dir) "/tmp"))))

(ert-deftest org-tangle-dir-test/file-level-sexp ()
  "File-level #+PROPERTY with sexp."
  (org-tangle-dir-test-with-temp-text "#+PROPERTY: tangle-dir (expand-file-name \"foo\" \"~/tmp\")\n* H\n"
    (should (string= (org-tangle-dir) (expand-file-name "foo" "~/tmp")))))

(ert-deftest org-tangle-dir-test/drawer-plain ()
  "Heading drawer property with plain path."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /var/tmp\n:END:\n"
    (should (string= (org-tangle-dir) "/var/tmp"))))

(ert-deftest org-tangle-dir-test/drawer-sexp ()
  "Heading drawer property with sexp."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: (expand-file-name \"bar\" \"/opt\")\n:END:\n"
    (should (string= (org-tangle-dir) (expand-file-name "bar" "/opt")))))

(ert-deftest org-tangle-dir-test/child-inherits-parent ()
  "Child heading inherits parent's tangle-dir."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /var/tmp\n:END:\n** C\n"
    (should (string= (org-tangle-dir) "/var/tmp"))))

(ert-deftest org-tangle-dir-test/child-overrides-parent ()
  "Child heading overrides parent's tangle-dir."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /var/tmp\n:END:\n** C\n:PROPERTIES:\n:tangle-dir: /other\n:END:\n"
    (should (string= (org-tangle-dir) "/other"))))

(ert-deftest org-tangle-dir-test/child-overrides-file-level ()
  "Child heading overrides file-level property."
  (org-tangle-dir-test-with-temp-text "#+PROPERTY: tangle-dir /tmp\n* P\n** C\n:PROPERTIES:\n:tangle-dir: /other\n:END:\n"
    (should (string= (org-tangle-dir) "/other"))))

(ert-deftest org-tangle-dir-test/deep-inheritance ()
  "Three-level inheritance chain."
  (org-tangle-dir-test-with-temp-text "* A\n:PROPERTIES:\n:tangle-dir: /a\n:END:\n** B\n*** C\n"
    (should (string= (org-tangle-dir) "/a"))))

(ert-deftest org-tangle-dir-test/with-path-arg ()
  "org-tangle-dir with path argument."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /var/tmp\n:END:\n"
    (should (string= (org-tangle-dir "config.yml")
                    (expand-file-name "config.yml" "/var/tmp")))))

(ert-deftest org-tangle-dir-test/tdir-base-plain ()
  "tdir-base returns parent's effective tangle-dir."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /var/tmp\n:END:\n** C\n:PROPERTIES:\n:tangle-dir: (tdir-base \"sub\")\n:END:\n"
    (should (string= (org-tangle-dir) (expand-file-name "sub" "/var/tmp")))))

(ert-deftest org-tangle-dir-test/missing-property-error ()
  "Missing property signals user-error."
  (org-tangle-dir-test-with-temp-text "* No Prop\n"
    (should-error (org-tangle-dir) :type 'user-error)))

(ert-deftest org-tangle-dir-test/trailing-slash-trimmed ()
  "Trailing slashes are trimmed from plain strings."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /tmp/foo///\n:END:\n"
    (should (string= (org-tangle-dir) "/tmp/foo"))))

(ert-deftest org-tangle-dir-test/root-path-preserved ()
  "Root path stays root path."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: /\n:END:\n"
    (should (string= (org-tangle-dir) "/"))))

(ert-deftest org-tangle-dir-test/whitelist-functions ()
  "Sexp with whitelisted functions evaluates correctly."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: (expand-file-name \"conf\" (xdg-config-home))\n:END:\n"
    (should (string= (org-tangle-dir)
                    (expand-file-name "conf" (xdg-config-home))))))

(ert-deftest org-tangle-dir-test/concat-whitelisted ()
  "concat is whitelisted and works."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: (concat \"/tmp\" \"/\" \"foo\")\n:END:\n"
    (should (string= (org-tangle-dir) "/tmp/foo"))))

(ert-deftest org-tangle-dir-test/unsafe-form-rejected ()
  "Non-whitelisted function is rejected."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: (shell-command \"echo pwned\")\n:END:\n"
    (should-error (org-tangle-dir) :type 'user-error)))

(ert-deftest org-tangle-dir-test/must-return-string ()
  "Sexp that doesn't return string signals error."
  (org-tangle-dir-test-with-temp-text "* P\n:PROPERTIES:\n:tangle-dir: (list 1 2 3)\n:END:\n"
    (should-error (org-tangle-dir) :type 'user-error)))

(ert-deftest org-tangle-dir-test/deep-narrow-inheritance ()
  "tdir-base works when narrowed to a subtree below the tangle-dir root.
Regression test for max-lisp-eval-depth with deep recursion + narrowing."
  (org-tangle-dir-test-with-temp-text
   "* Roles
:PROPERTIES:
:tangle-dir: roles
:END:
** File Systems & Mounts
:PROPERTIES:
:tangle-dir: (tdir-base \"file_system\")
:END:
*** Tasks
:PROPERTIES:
:tangle-dir: (tdir-base \"tasks\")
:END:\n"
   (let ((expected (expand-file-name "roles/file_system/tasks")))
     (should (string= (org-tangle-dir) expected)))))

(defconst org-tangle-dir-test--roles-text
  "* Roles
:PROPERTIES:
:tangle-dir: roles
:END:
** File Systems & Mounts
:PROPERTIES:
:tangle-dir: (tdir-base \"file_system\")
:END:
*** Tasks
:PROPERTIES:
:tangle-dir: (tdir-base \"tasks\")
:END:
*** Handlers
#+begin_src yaml
x: 1
#+end_src
"
  "Three-level hierarchy whose deepest headings inherit through sexps.")

(ert-deftest org-tangle-dir-test/narrowed-sexp-reaches-hidden-parent ()
  "A `tdir-base' property resolves while narrowed to its own subtree."
  (org-tangle-dir-test-with-temp-text org-tangle-dir-test--roles-text
    (re-search-backward "^\\*\\*\\* Tasks")
    (org-narrow-to-subtree)
    (should (string= (org-tangle-dir)
                     (expand-file-name "roles/file_system/tasks")))
    (should (buffer-narrowed-p))))

(ert-deftest org-tangle-dir-test/narrowed-inherits-hidden-parent ()
  "A heading without the property, narrowed below the ancestor that sets
a `tdir-base' value, resolves that value from the ancestor."
  (org-tangle-dir-test-with-temp-text
      (concat "#+PROPERTY: tangle-dir /file-level\n"
              org-tangle-dir-test--roles-text)
    (re-search-forward "^x: 1")
    (org-narrow-to-subtree)
    (should (string= (org-tangle-dir "main.yml")
                     (expand-file-name "roles/file_system/main.yml")))))

(provide 'org-tangle-dir-tests)
;;; org-tangle-dir-tests.el ends here
