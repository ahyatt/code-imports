;;; Commentary:
;; 

;;; code-imports.el --- A module for organizing and adding to code imports.
;; Copyright (C) 2011 Andrew Hyatt
;;
;;     This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; code-imports-test.el --- Tests for code-imports.el

(provide 'code-imports-test)

(ert-deftest code-imports--curent-line-test ()
    (should (equal "Line2"
                (with-temp-buffer
                  (insert "Line1\nLine2\nLine3")
                  (previous-line)
                  (code-imports--current-line)))))

(ert-deftest code-imports--blank-line-p ()
  (should-not (code-imports--blank-line-p "foo"))
  (should (code-imports--blank-line-p " "))
  (should (code-imports--blank-line-p "\n"))
  (should (code-imports--blank-line-p "")))

(ert-deftest code-imports--valid-import-block-line-p ()
  (should-not (code-imports--valid-import-block-line-p "random line"))
  (should (code-imports--valid-import-block-line-p ""))
  (should (code-imports--valid-import-block-line-p "#include \"foo.h\";"))
  (should (code-imports--valid-import-block-line-p "import foo.Bar;")))

(ert-deftest code-imports--add-import-to-clipboard ()
  (should (equal '("foo.h")
                 (progn
                   (let ((code-imports-clipboard))
                     (code-imports--add-import-to-clipboard "foo.h" 'c-mode)
                     (assoc-default 'c-mode code-imports-clipboard)))))
  (should (equal '("bar.h" "foo.h")
                 (progn
                   (let ((code-imports-clipboard))
                     (code-imports--add-import-to-clipboard "foo.h" 'c-mode)
                     (code-imports--add-import-to-clipboard "bar.h" 'c-mode)
                     (assoc-default 'c-mode code-imports-clipboard))))))

(ert-deftest code-imports--make-relative ()
  (should (equal "a.h"
                 (let ((code-imports-project-directory "/test"))
                   (code-imports--make-relative "/test/a.h"))))
  (should (equal "a.h"
                 (let ((code-imports-project-directory "~/src/test"))
                   (flet ((expand-file-name (file)
                                            (replace-regexp-in-string
                                             "~" "/home/ahyatt" file)))
                     (code-imports--make-relative
                      "/home/ahyatt/src/test/a.h"))))))

(ert-deftest code-imports-grab-import ()
  (should (equal '("foo.h")
                 (progn
                   (let ((code-imports-clipboard)
                         (code-imports-project-directory "/test"))
                     (with-temp-buffer
                       (let ((major-mode 'c-mode)
                             (buffer-file-name "/test/foo.h"))
                         (code-imports-grab-import)
                         (assoc-default 'c-mode code-imports-clipboard)))))))
  (should (equal '("a/b/C.java")
                 (progn
                   (let ((code-imports-clipboard))
                     (with-temp-buffer
                       (let ((major-mode 'java-mode)
                             (buffer-file-name "C.java"))
                         (insert "package a.b;\n")
                         (code-imports-grab-import)
                         (assoc-default 'java-mode code-imports-clipboard)))))))
  (should (equal '("C.java")
                 (progn
                   (let ((code-imports-clipboard))
                     (with-temp-buffer
                       (let ((major-mode 'java-mode)
                             (buffer-file-name "C.java"))
                         (code-imports-grab-import)
                         (assoc-default 'java-mode code-imports-clipboard)))))))
  (should-error (with-temp-buffer
                  (let ((major-mode 'non-cc-mode))
                    (code-imports-grab-import)))))

(ert-deftest code-imports-add-grabbed-imports ()
  (should-error (with-temp-buffer
                  (code-imports-add-grabbed-imports)))
  ;; Should error when no clipboard data found.
  (should-error (with-temp-buffer
                  (let ((code-imports-project-directory "foo")
                        (code-imports-clipboard))
                    (c-mode)
                    (code-imports-add-grabbed-imports))))
  (should-error (with-temp-buffer
                  (let ((major-mode 'c-mode)
                        (code-imports-project-directory)
                        (code-imports-clipboard '((c-mode "foo.h"))))
                    (code-imports-add-grabbed-imports))))
  (should-error (with-temp-buffer
                  (let ((major-mode 'c-mode)
                        (code-imports-clipboard '((c-mode "foo.h")))
                        (code-imports-project-directory "")
                        (arg))
                    (c-mode)
                    (insert "#ifdef FOO\n#include \"bar.h\"\n#endif\n")
                    (code-imports-add-grabbed-imports))))
  (should (equal '(("foo.h")  . ())
              (with-temp-buffer
                (let ((major-mode 'c-mode)
                      (code-imports-clipboard '((c-mode "foo.h")))
                      (code-imports-project-directory "")
                      (arg))
                  (flet ((code-imports--add-imports (imports)
                                                    (setq arg imports)))
                    (c-mode)
                    (code-imports-add-grabbed-imports)
                    (cons arg code-imports-clipboard)))))))

(ert-deftest code-imports--cut-imports ()
  (should (equal (cons
                  (cons 11 '("#include \"foo.h\"" "#include \"bar.h\""))
                  "Copyright\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "test.cc"))
                     (insert
                      "Copyright\n#include \"foo.h\"\n#include \"bar.h\"\n")
                     (cons (code-imports--cut-imports) (buffer-string))))))
  (should (equal (cons
                  (cons 42 '("#include \"bar.h\"" "#include \"foo.h\""))
                  "Copyright\n#ifndef BAZ_H\n#define BAZ_H 1\n\n#endif\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "baz.h"))
                     (insert
                      "Copyright\n#ifndef BAZ_H\n#define BAZ_H 1\n\n")
                     (insert "#include \"bar.h\"\n#include \"foo.h\"\n#endif\n")
                     (cons (code-imports--cut-imports) (buffer-string))))))
  (should (equal (cons
                  (cons 42 '())
                  "Copyright\n#ifndef BAZ_H\n#define BAZ_H 1\n\n#endif\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "baz.h"))
                     (insert
                      "Copyright\n#ifndef BAZ_H\n#define BAZ_H 1\n\n#endif\n")
                     (cons (code-imports--cut-imports) (buffer-string))))))
  (should (equal (cons
                  (cons 12 '("import foo.Bar;" "import bar.Baz;"))
                  "Copyright\n\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "Test.java"))
                     (insert
                      "Copyright\n\nimport foo.Bar;\nimport bar.Baz;\n")
                     (cons (code-imports--cut-imports) (buffer-string))))))
  (should (equal (cons
                  (cons 11 '("import foo.Bar;" "import bar.Baz;"))
                  "Copyright\n\nclass\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "Test.java"))
                     (insert
                      "Copyright\nimport foo.Bar;\nimport bar.Baz;\n\nclass\n")
                     (cons (code-imports--cut-imports) (buffer-string)))))))

(ert-deftest code-imports--file-to-line ()
  (should (equal "import foo.bar.Baz;"
                 (code-imports--file-to-line "foo/bar/Baz.java")))
  (should (equal "#include \"foo.h\""
                 (code-imports--file-to-line "foo.h"))))

(ert-deftest code-imports--paste-imports ()
  (should (equal "Copyright\n#import \"foo.h\"\n\n#import \"bar.h\"\n"
                 (with-temp-buffer
                   (insert "Copyright\n")
                   (code-imports--paste-imports 11
                                                '("#import \"foo.h\""
                                                  blank
                                                  "#import \"bar.h\""))
                   (buffer-string)))))

(ert-deftest code-imports--import-in-group-p ()
  (should-not (code-imports--import-in-group-p "#import \"foo.h\""
                                               t))
  (should (code-imports--import-in-group-p "#import \"foo.h\""
                                           'self "foo.h"))
  (should-not (code-imports--import-in-group-p "#import \"foo.h\""
                                               'self "bar.h"))
  (should (code-imports--import-in-group-p "#import \"foo.h\""
                                           "\\.h" "bar.h"))
  (should-not (code-imports--import-in-group-p "#import \"foo.h\""
                                               "<.*[^.].>" "bar.h")))

(ert-deftest code-imports--sort-imports ()
  (should (equal '("#import \"foo.h\"" blank "#import <string>" blank
                   "#import \"a.h\"" "#import \"b.h\""
                   "#import \"bar.h\"" "#import \"c.h\"")
                 (code-imports--sort-imports '("#import \"bar.h\""
                                        "#import \"foo.h\""
                                        "#import \"c.h\""
                                        "#import \"a.h\""
                                        "#import \"b.h\""
                                        "#import <string>")
                                      code-imports-c++-ordering
                                      "foo.h"))))

(ert-deftest code-imports--sort-predicate ()
  (should (code-imports--sort-predicate "#include \"a.h\""
                                        "#include \"b.h\""))
  (should-not (code-imports--sort-predicate "#include \"b.h\""
                                        "#include \"a.h\""))
  (should-not (code-imports--sort-predicate "import a.a.A.A;"
                                            "import a.a.A;")))

(ert-deftest code-imports--add-imports ()
  (should (equal "Copyright\n#include \"bar.h\"\n#include \"foo.h\"\n"
                 (with-temp-buffer
                   (let ((buffer-file-name "baz.cc")
                         (code-imports-project-directory ""))
                     (insert "Copyright\n#include \"foo.h\"\n")
                     (code-imports--add-imports '("bar.h"))
                     (buffer-string)))))
  (should (equal (concat "Copyright\n#ifndef BAZ_H\n#define BAZ_H\n"
                         "#include \"bar.h\"\n#include \"foo.h\"\n#endif\n")
                 (with-temp-buffer
                   (let ((buffer-file-name "baz.h")
                         (code-imports-project-directory ""))
                     (insert "Copyright\n#ifndef BAZ_H\n#define BAZ_H\n")
                     (insert "#include \"foo.h\"\n#endif\n")
                     (code-imports--add-imports '("bar.h"))
                     (buffer-string)))))
  (should (equal (concat "Copyright\npackage a.b.C\n\nimport a.Bar;\n"
                         "import a.Foo;\n\nclass Baz {\n  Bar b\n  Foo f\n}")
                 (with-temp-buffer
                   (let ((buffer-file-name "Test.java")
                         (code-imports-project-directory ""))
                     (insert "Copyright\npackage a.b.C\n\nimport a.Foo;\n\n")
                     (insert "class Baz {\n  Bar b\n  Foo f\n}")
                     (code-imports--add-imports '("a/Bar.java"))
                     (buffer-string))))))

(ert-deftest code-imports-organize-imports ()
  (should (equal "Copyright\n#include \"bar.h\"\n#include \"foo.h\"\n"
                 (with-temp-buffer
                   (insert
                    "Copyright\n#include \"foo.h\"\n#include \"bar.h\"\n")
                   (c++-mode)
                   (let ((buffer-file-name "/foo/bar/baz.h")
                         (code-imports-project-directory "/foo"))
                     (code-imports-organize-imports)
                     (buffer-string)))))
  (should (equal (concat "Copyright\n#include \"bar.h\"\n"
                         "#include \"foo.h\"  // for Foo\n")
                 (with-temp-buffer
                   (insert
                    "Copyright\n#include \"foo.h\"  // for Foo\n")
                   (insert "#include \"bar.h\"\n")
                   (c++-mode)
                   (let ((buffer-file-name "/foo/bar/baz.h")
                         (code-imports-project-directory "/foo"))
                     (code-imports-organize-imports)
                     (buffer-string)))))
  (should (equal "import a.B;\n\nclass Foo {\nB;\n}\n"
                 (with-temp-buffer
                   (java-mode)
                   (let ((buffer-file-name "/foo/bar/Baz.java")
                         (code-imports-project-directory "/foo"))
                     (insert "import a.B;\nimport a.A\n\nclass Foo {\nB;\n}\n")
                     (code-imports-organize-imports)
                     (buffer-string)))))
  (should-error (with-temp-buffer
                  (let ((code-imports-project-directory)
                        (buffer-file-name "/foo/bar/baz.h"))
                    (c-mode)
                    (code-imports-organize-imports)))))

(ert-deftest code-imports-unused-import-p ()
  (should-not (with-temp-buffer
                (c-mode)
                (code-imports-unused-import-p "#include \"test.h\"")))
  (should-not (with-temp-buffer
                (java-mode)
                (insert "class Foo {\n Bar b;\n}\n")
                (code-imports-unused-import-p "import a.b.c.Bar;")))
  (should (with-temp-buffer
            (java-mode)
            (insert "class Foo {\n Bar b;\n}\n")
            (code-imports-unused-import-p "import a.b.c.Baz;")))
  (should-not (with-temp-buffer
                (java-mode)
                (insert "class Foo {\n Bar b;\n}\n")
                (code-imports-unused-import-p "import Bar;"))))

(ert-deftest code-imports-line-to-relative-file ()
  (should (equal "bar/baz.h" (code-imports-line-to-relative-file
                              "#include \"bar/baz.h\"")))
  (should-not (code-imports-line-to-relative-file "#include <string>")))

(ert-deftest code-import--guess-import-root ()
  (flet ((file-exists-p (file) (equal file "/foo/bar/bar.h")))
    (should (equal "/foo/bar/"
                   (code-imports--guess-import-root "/foo/bar/baz.h"
                                                      '("bar.h"))))
    (should (equal "/foo/"
                   (code-imports--guess-import-root "/foo/foo/baz.h"
                                                    '("bar/bar.h"))))))

;;; code-imports-test.el ends here
