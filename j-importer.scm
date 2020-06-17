(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 pretty-print)
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
;; latest-repository-commit store url
;; latest-repository-commit* url

;; most J addons don't have tags or releases, thence this
(define (j-import-package name url)
  (define url.git (string-append url ".git"))
  (define-values (cache-dir commit rel)
    (update-cached-checkout url.git))
  (define manifest.ijs
    (string-append cache-dir "/manifest.ijs"))
  (pretty-print
   `(define-public ,(string->symbol
		     (string-append "j-" name))
      (package
	(name ,name)
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
	 ,@(j-manifest manifest.ijs "DEPENDS"))
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
	(synopsis ,@(j-manifest manifest.ijs "CAPTION"))
	(description ,@(j-manifest manifest.ijs "DESCRIPTION"))
	(license)))))
