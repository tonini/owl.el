;;; owl.el --- Owl produces HTML and online documentation for Emacs projects

;;; Commentary:
;;

;;; Code:

(let ((current-directory (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name ".." current-directory))
  (add-to-list 'load-path current-directory))

(defvar owl-documentation-packages '()
  "")

(defvar owl-documentation-prefix nil
  "")

(defun docs-add-code-blocks (docstring)
  docstring)

(defun docs-get-function-definition (fn)
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

(defun docs-get-documention (fn)
  (let* ((docstring (if (documentation fn) (documentation fn) ""))
         (docstring (docs-add-code-blocks docstring))
         (docstring-list (when (not (equal docstring "")) (split-string docstring "\n"))))
    (if docstring-list
        (with-temp-buffer
          (mapc (lambda (p) (insert (format "<p>%s<p>" p))) docstring-list)
          (buffer-string))
      ""
      )))

(defun docs-get-variable-documention (variable)
  (let* ((docstring (if (get variable 'variable-documentation) (get variable 'variable-documentation) ""))
         (docstring (docs-add-code-blocks docstring))
         (docstring-list (when (not (equal docstring "")) (split-string docstring "\n"))))
    (if docstring-list
        (with-temp-buffer
          (mapc (lambda (p) (insert (format "<p>%s<p>" p))) docstring-list)
          (buffer-string))
      ""
      )))

(defun docs-function-list (prefix no-private)
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

(defun docs-variable-list (prefix no-private)
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

(defun docs-get-function-arguments (fn)
  (mapconcat 'symbol-name (help-function-arglist fn) " "))

(defun docs-generate (packages prefix-library)
  (interactive)
  (mapc (lambda (l) (require l)) packages)
  (message "Generate documentation for Alchemist...")
  (let* ((functions (sort (docs-function-list prefix-library t) 'string<))
         (variables (sort (docs-variable-list prefix-library t) 'string<))
         (functions (append functions variables))
         (html (with-temp-buffer
                 (insert-file-contents "docs/tmpl/layout.html")
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
                                                (docs-get-function-arguments fn)
                                              "")
                                            (if (fboundp fn)
                                                (docs-get-documention fn)
                                              (docs-get-variable-documention fn))
                                            (if (fboundp fn)
                                                (docs-get-function-definition fn)
                                              "")
                                            ))) functions)
                    (buffer-string)))
         (html (replace-regexp-in-string "### CONTENT ###" content html t)))
    (with-temp-file "docs/index.html"
      (insert html))
    (message "Documentation generated for Alchemist")))

(message load-file-name)

(defun owl-load-documentation-config ()
  (let* ((current-directory (file-name-directory load-file-name))
         (filename (expand-file-name (format "%s/%s" current-directory "owl-setup.el"))))
    (load-file filename)))

(owl-load-documentation-config)
(docs-generate owl-documentation-packages owl-documentation-prefix)

(provide 'owl)

;;; owl.el ends here
