(defvar doxygen-date-format "%Y-%m-%d"
  "The format used to display dates when using the \\date command.")

(defun doxygen-insert-comment()
  "Insert a generic Doxygen comment block at point, including brief
and long sections."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (point)))
        (insert (concat "/** \n"
			" ** \n"
                        " **/\n"))
        (let ((end (point)))
          (indent-region start end nil)))))
  (end-of-line))

(defun doxygen-insert-file-comment ()
  "Insert a Doxygen file comment at point."
  (interactive "*")
  (let ((file-name (if (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name))
                     "untitled")))
	(insert (format (concat
		     "/**********************************************************************/\n"
		     "/** <project name> - \n"
		     " ** <description>\n"
		     " **\n"
		     " **  @file   %s\n"
		     " **\n"
		     " **  @author %s\n"
		     " **\n"
		     " **\n"
		     " **\n"
		     "*************************************************************************/\n")
		     file-name user-full-name))))


(defun doxygen-insert-function-comment ()
  "Insert a Doxygen comment for the function at point."
  (interactive "*")
  (beginning-of-line)
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (point)))
        (let ((args (find-arg-list)))
          (insert (concat "/** \n"
                          (format " * \n" (cdr (assoc 'function args)))
                          " *\n"))
          (when (cdr (assoc 'args args))
            (dump-arguments (cdr (assoc 'args args))))
          (unless (string= "void" (cdr (assoc 'return args)))
            (insert " * @return \n"))
          (insert " */\n"))
        (let ((end (point)))
          (indent-region start end nil)
          (untabify start end)))))
  (end-of-line))

(defun doxygen-insert-member-group-region (start end)
  "Make the current region a member group."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    ; indent-according-to-mode doesn't work well here...
    (insert "//@{\n")
    (goto-char end)
    (end-of-line)
    (insert "\n//@}\n")))

(defun doxygen-insert-compound-comment ()
  "Insert a compound comment."
  (interactive "*")
  (let ((comment-start "/*!< ")
        (comment-end "*/"))
    (indent-for-comment)))


;;; internal utility functions

(defun dump-arguments (arglist)
  "Insert a comment with the Doxygen comments for a function."
  (mapcar (function (lambda (x)
                      (insert (format " * @param %s\t\n"
                                      (extract-argument-name x)))))
          arglist))

(defun extract-argument-name (arg)
  "Get the argument name from the argument string 'arg'."
  ; this does *not* work for function pointer arguments
  (if (string-match "\\([a-zA-Z0-9_]+\\)\\s-*\\(=\\s-*.+\\s-*\\)?$" arg)
      (substring arg (match-beginning 1) (match-end 1))
    arg))

(defun find-return-type ()
  "Extract the return type of a function.
   If the function is a constructor, it returns void."
  (interactive "*")
  (save-excursion
    (let ((start (point)))
      (search-forward "(")
      (let ((bound (point)))
        (goto-char start)
        (if (re-search-forward
             (concat
              "\\(virtual \|static \|const \\)*" ; opt. modifiers
              "\\([a-zA-Z0-9_:*]+\\)\\s-+[a-zA-Z0-9_:*]+\\s-*(") ; return type
             bound t)
            (buffer-substring (match-beginning 2)(match-end 2))
          "void")
        ))))

(defun find-arg-list ()
  "Extract various bits of information from a C or C++ function declaration"
  (interactive "*")
  (let ((return-type (find-return-type)))
  (save-excursion
    (if (re-search-forward (concat
                              "\\([a-zA-Z0-9_:]+\\)\\s-*("    ; function name
                              "\\([^)]*\\))")                 ; argument list
                           nil t)
          (list (cons 'return   return-type)
                (cons 'function (buffer-substring (match-beginning 1)
                                                  (match-end 1)))
                (cons 'args     (split-string
                                 (buffer-substring (match-beginning 2)
                                                   (match-end 2)) ",")))
      nil))))

(provide 'doxygen)

;;; doxygen.el ends here
