(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 pretty-print)
	     (srfi srfi-1)
	     (srfi srfi-26)
	     (guix import utils)
	     (guix build git)
	     (guix scripts hash)
	     (guix git))

(define (j-manifest where what)
  (define cmd
    (format #f "jconsole -js \"load '~a'\" \"echo ~a\" \"exit 0\""
	    where
	    (string-upcase what)))
  (define out (open-input-pipe cmd))
  (let lp ((x (read-line out)) (xs '()))
    (if (eof-object? x)
	(reverse xs)
	(lp (read-line out) `(,x ,@xs)))))

;; most J addons don't have tags or releases, thence this
(define (j-import-package url)
  (define name
    (snake-case (car (last-pair (string-split url (cut char=? <> #\/))))))
  (define url.git (string-append url ".git"))
  (define-values (cache-dir commit rel)
    (update-cached-checkout url.git))
  (define manifest.ijs
    (string-append cache-dir "/manifest.ijs"))
  (define (jdep->gdep dep)
    (string-join (cons "j" (string-split dep (cut char=? <> #\/))) "-"))
  (pretty-print
   `(define-public ,(string->symbol
		     (string-append "j-" name))
      (package
	(name ,(string-append "j-" name))
	(version ,@(j-manifest manifest.ijs "VERSION"))
	(source
	 (origin
	   (method git-fetch)
	   (uri
	    (git-reference
	     (url ,url.git)
	     (commit ,commit)))
	   (sha256
	    (base32 ,(let ((hash (with-output-to-string
				   (lambda ()
				     (guix-hash "-x" "-r" cache-dir)))))
		       (substring hash 0 52))))))
	(propagated-inputs
	 (,@(map (lambda (dep)
		   (let ((dep (jdep->gdep dep)))
		     `(,dep ,(string->symbol dep))))
		 (j-manifest manifest.ijs "DEPENDS"))))
	(outputs '("out"))
	(build-system gnu-build-system)
	(arguments
	 `(#:modules ((guix build gnu-build-system) (guix build utils))
	   #:phases
	   (modify-phases %standard-phases
	     (delete 'configure) (delete 'check) (delete 'build)
	     (replace 'install
	       (lambda _
		 (let ((out (string-append (assoc-ref %outputs "out")
					   ,,(apply
					     string-append
					     "/share/j/addons/"
					     (j-manifest manifest.ijs "FOLDER")))))
		   (copy-recursively "." out)
		   #t))))))
	(home-page ,url)
	(synopsis ,(fold-right (lambda (x s)
				 (string-append x "\n" s))
			       ""
			       (j-manifest manifest.ijs "CAPTION")))
	(description ,(fold-right (lambda (x s)
				 (string-append x "\n" s))
			       ""
			       (j-manifest manifest.ijs "DESCRIPTION")))
	(license)))))

(define (import-to-file url file)
  (with-output-to-file file
    (lambda ()
      (j-import-package url))))

(define (j-package-latest-commit+hash url)
  (define name
    (snake-case (car (last-pair (string-split url (cut char=? <> #\/))))))
  (define url.git (string-append url ".git"))
  (define-values (cache-dir commit rel)
    (update-cached-checkout url.git))
  (list commit
	(substring (with-output-to-string
		     (lambda ()
		       (guix-hash "-x" "-r" cache-dir)))
		   0 52)))

