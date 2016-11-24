;; WIP
(require 'json)

(defun js-package-deps ()
  (let* ((project-dir (locate-dominating-file "." "package.json"))
	 (package-file (concat project-dir "package.json"))
	 (package-props (json-read-file package-file))
	 (dependencies (assoc 'dependencies package-props))
	 (package-deps (mapcar 'car (cdr dependencies)))
	 )
    package-deps
    )
  )
