;;; owl.el --- Owl produces HTML and online documentation for Emacs projects

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/owl.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Owl produces HTML and online documentation for Emacs projects

;;; Code:

(let ((current-directory (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name ".." current-directory))
  (add-to-list 'load-path current-directory))

(defvar owl-documentation-packages '()
  "")

(defvar owl-documentation-prefix nil
  "")

(defun owl--add-code-blocks (docstring)
  docstring)

(defun owl--get-function-definition (fn)
  (let ((function-definiton (save-excursion (find-function fn)
                                            (let* ((start-pos (point))
                                                   (end-pos (progn
                                                              (end-of-defun)
                                                              (point))))
                                              (buffer-substring-no-properties start-pos end-pos))
                                            )))
    (format "
<div class=\"view_source\">Source: <a class=\"view_source_link\" href=\"#\">show</a></div>
<div class=\"source\">
  <pre>
    <code class=\"lisp\">%s</code>
</pre>
</div>" (if function-definiton function-definiton ""))))

(defun owl--get-documentation (fn)
  (let* ((docstring (if (documentation fn) (documentation fn) ""))
         (docstring (owl--add-code-blocks docstring))
         (docstring-list (when (not (equal docstring "")) (split-string docstring "\n"))))
    (if docstring-list
        (with-temp-buffer
          (mapc (lambda (p) (insert (format "<p>%s<p>" p))) docstring-list)
          (buffer-string))
      ""
      )))

(defun owl--get-variable-documentation (variable)
  (let* ((docstring (if (get variable 'variable-documentation) (get variable 'variable-documentation) ""))
         (docstring (owl--add-code-blocks docstring))
         (docstring-list (when (not (equal docstring "")) (split-string docstring "\n"))))
    (if docstring-list
        (with-temp-buffer
          (mapc (lambda (p) (insert (format "<p>%s<p>" p))) docstring-list)
          (buffer-string))
      ""
      )))

(defun owl--get-function-list (prefix no-private)
  "Return list of functions matching PREFIX."
  (let ((prefix-sym (symbol-name prefix))
        (res '()))
    (mapatoms
     (lambda (atom)
       (let ((sn (symbol-name atom)))
         (and (fboundp atom)
              (if (string-prefix-p prefix-sym sn)
                  (if no-private
                      (if (not (string-match "[^-]+--.*" sn))
                          (setq res (cons atom res)))
                    (setq res (cons atom res)))))))
     obarray)
    res))

(defun owl--get-variable-list (prefix no-private)
  "Return list of variables matching PREFIX."
  (let ((prefix-sym (symbol-name prefix))
        (res '()))
    (mapatoms
     (lambda (atom)
       (let ((sn (symbol-name atom)))
         (and (boundp atom)
              (if (string-prefix-p prefix-sym sn)
                  (if no-private
                      (if (not (string-match "[^-]+--.*" sn))
                          (setq res (cons atom res)))
                    (setq res (cons atom res)))))))
     obarray)
    res))

(defun owl--get-function-arguments (fn)
  (mapconcat 'symbol-name (help-function-arglist fn) " "))

(defun owl--execution-dir ()
  (file-name-directory load-file-name))

(defun owl--generate (packages prefix-library)
  (interactive)
  (mapc (lambda (l) (require l)) packages)
  (message "Generate documentation for Alchemist...")
  (let* ((functions (sort (owl--get-function-list prefix-library t) 'string<))
         (variables (sort (owl--get-variable-list prefix-library t) 'string<))
         (functions (append functions variables))
         (html (with-temp-buffer
                 (insert-file-contents (format "%s/tmpl/layout.html" (owl--execution-dir)))
                 (buffer-string)))
         (content (with-temp-buffer
                    (mapc (lambda (fn)
                            (insert (format "
<li>
  <div class=\"detail_header\" id=\"%s\">
    <div class=\"detail_header_links\">
      <a href=\"#%s\" class=\"detail_link\" title=\"Link to this function\">
       <span class=\"octicon octicon-link\"></span>
      </a>
      <a class=\"to_top_link\" href=\"#content\" title=\"To the top of the page\">
       <span class=\"octicon octicon-chevron-up\"></span>
      </a>
    </div>
    <span class=\"detail_type\">%s</span>
    <span class=\"signature\">
      %s
    </span>
    <span class=\"arguments\">%s</span>
  </div>
  <div class=\"docstring\">
    %s
  </div>
  %s
</li>
"
                                            fn
                                            fn
                                            (if (fboundp fn)
                                                "Function"
                                              "Variable")
                                            fn
                                            (if (fboundp fn)
                                                (owl--get-function-arguments fn)
                                              "")
                                            (if (fboundp fn)
                                                (owl--get-documentation fn)
                                              (owl--get-variable-documentation fn))
                                            (if (fboundp fn)
                                                (owl--get-function-definition fn)
                                              "")
                                            ))) functions)
                    (buffer-string)))
         (html (replace-regexp-in-string "### CONTENT ###" content html t)))
    (with-temp-file (format "%s/index.html" (owl--execution-dir))
      (insert html))
    (message "Documentation generated for Alchemist")))

(message load-file-name)

(defun owl--load-documentation-config ()
  (let* ((current-directory (file-name-directory load-file-name))
         (filename (expand-file-name (format "%s/%s" current-directory "owl-setup.el"))))
    (load-file filename)))

(owl--load-documentation-config)
(owl--generate owl-documentation-packages owl-documentation-prefix)

(provide 'owl)

;;; owl.el ends here
