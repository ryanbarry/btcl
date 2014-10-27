(in-package :com.gigamonkeys.portable-pathnames)

;;; a portable pathname library
;;;
;;; helps smooth over some differences between common lisp implementations
;;;

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; is the given pathname a directory?
;; returns the pathname if true, nil if false
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

;; converts given pathname to directory form
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

;; converts given pathname to file form
(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil ;clisp needs this to show files w/o extension
   :defaults (pathname-as-directory dirname)))

;; clisp helper to product directory wildcard with nil pathname-type
;; required since clisp won't match extensionless files when wildcard has non-nil type
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;; lists all files and subdirectories in given directory pathname
;; subdirectories are listed in directory form for easy differentiation from files
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+ (or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-subdirectory not implemented")))

;; simple test for whether file or directory represented by pathname exists
;; returns the pathname if true, nil if false
;; always returns pathnames representing directories in directory form
(defun file-exists-p (pathname)
  #+ (or sbcl lispworks openmcl)
  (probe-file pathname)

  #+ (or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #- (or sbcl lispworks openmcl allegro cmu clisp)
  (error "file-exists-p not implemented"))

;; recursively calls given function on pathnames of all files under given directory
;; two keyword args:
;; :directories - when true, will call function on pathnames of directories also
;; :test - specifies fun invoked prior to main fun, skips main fun if ret false
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
  
