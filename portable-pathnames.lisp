(in-package :com.gigamonkeys.portable-pathnames)

;;; a portable pathname library
;;;
;;; helps smooth over some differences between common lisp implementations
;;; 

;;;
;;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.
;;; * Neither the name of Gigamonkeys Consulting nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
  
