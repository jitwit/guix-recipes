(define-module (jrn packages chez)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
   #:select (gpl2+ gpl3+ lgpl2.0+ lgpl2.1+ asl2.0 bsd-3 expat public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module ((gnu packages chez) #:select (chez-scheme))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public chez-srfi
  (package
   (name "chez-srfi")
   (version "1.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/fedeinthemix/chez-srfi.git")
       (commit
	(string-append "v" version))))
     (sha256
      (base32 "1vgn984mj2q4w6r2q66h7qklp2hrh85wwh4k9yisga5fi0ps7myf"))
     (file-name
      (git-file-name name version))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (arguments
    `(#:make-flags
      (let
	  ((out
	    (assoc-ref %outputs "out")))
	`(,(string-append "PREFIX=" out)))
      #:test-target "test"
      #:phases
      (modify-phases %standard-phases
		     (delete 'configure))))
   (home-page "https://github.com/fedeinthemix/chez-srfi")
   (synopsis "SRFI libraries for Chez Scheme")
   (description
    "This package provides a collection of SRFI libraries for Chez Scheme.")
   (license expat)))

(define-public chez-mit
  (package
   (name "chez-mit")
   (version "0.1")
   (home-page "https://github.com/fedeinthemix/chez-mit")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url home-page)
       (commit
	(string-append "v" version))))
     (sha256
      (base32 "0c7i3b6i90xk96nmxn1pc9272a4yal4v40dm1a4ybdi87x53zkk0"))
     (file-name
      (git-file-name name version))))
   (build-system gnu-build-system)
   (inputs
    `(("chez-srfi" ,chez-srfi)))
					; for tests
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (arguments
    `(#:make-flags
      (let
	  ((out
	    (assoc-ref %outputs "out")))
	`(,(string-append "PREFIX=" out)))
      #:tests? #f ;; fixme
      #:phases
      (modify-phases %standard-phases
		     (delete 'configure))))
   (synopsis "MIT/GNU Scheme compatibility library for Chez Scheme")
   (description "This package provides a set of MIT/GNU Scheme compatibility
libraries for Chez Scheme.  The main goal was to provide the functionality
required to port the program @code{Scmutils} to Chez Scheme.")
   (license gpl3+)))

(define-public chez-scmutils
  (package
   (name "chez-scmutils")
   (version "0.1")
   (home-page "https://github.com/fedeinthemix/chez-scmutils")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url home-page)
       (commit
	(string-append "v" version))))
     (sha256
      (base32 "0lb05wlf8qpgg8y0gdsyaxg1nbfx1qbaqdjvygrp64ndn8fnhq7l"))
     (file-name
      (git-file-name name version))))
   (build-system gnu-build-system)
   (inputs
    `(("chez-srfi" ,chez-srfi)))
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (propagated-inputs
    `(("chez-srfi" ,chez-srfi)
      ("chez-mit" ,chez-mit)))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (arguments
    `(#:make-flags
      (let
	  ((out
	    (assoc-ref %outputs "out")))
	`(,(string-append "PREFIX=" out)))
      #:tests? #f			; no test suite
      #:phases
      (modify-phases %standard-phases
	(delete 'configure)
	;; Since the documentation is lacking, we install the source
	;; code.  For things to work correctly we have to replace
	;; relative paths by absolute ones in 'include' forms.  This
	;; in turn requires us to compile the files in the final
	;; destination.
	(delete 'build)
	(add-after 'install 'install-src
	  (lambda*
	      (#:key
	       (make-flags
		'())
	       #:allow-other-keys)
	    (apply invoke "make" "install-src" make-flags)))
	(add-after 'install-src 'absolute-path-in-scm-files
	  (lambda*
	      (#:key outputs #:allow-other-keys)
	    (let
		((out
		  (assoc-ref outputs "out")))
	      (for-each
	       (lambda
		   (file)
		 (substitute* file
		   (("include +\"\\./scmutils")
		    (string-append "include \""
				   (dirname file)))))
	       (find-files out "\\.sls"))
	      (for-each
	       (lambda
		   (file)
		 (substitute* file
		   (("include +\"\\./scmutils/simplify")
		    (string-append "include \""
				   (dirname file)))))
	       (find-files out "fbe-syntax\\.scm"))
	      #t)))
	(add-after 'absolute-path-in-scm-files 'build
	  (lambda*
	      (#:key outputs
	       (make-flags
		'())
	       #:allow-other-keys)
	    (let*
		((out
		  (assoc-ref outputs "out"))
		 (mk-file
		  (car
		   (find-files out "Makefile"))))
	      (with-directory-excursion
		  (dirname mk-file)
		(apply invoke "make" "build" make-flags)))))
	(add-after 'build 'clean-up
	  (lambda*
	      (#:key outputs #:allow-other-keys)
	    (let*
		((out
		  (assoc-ref outputs "out")))
	      (for-each delete-file
			(find-files out "Makefile|compile-all\\.ss"))
	      #t))))))
   (synopsis "Port of MIT/GNU Scheme Scmutils to Chez Scheme")
   (description "This package provides a port of the MIT/GNU Scheme
Scmutils program to Chez Scheme.  The port consists of a set of
libraries providing most of the functionality of the original.")
   (license gpl3+)))

(define-public chez-matchable
  (package
    (name "chez-matchable")
    (version "20160306")
    (home-page "https://github.com/fedeinthemix/chez-matchable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (sha256
        (base32 "02qn7x348p23z1x5lwhkyj7i8z6mgwpzpnwr8dyina0yzsdkr71s"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("chez-srfi" ,chez-srfi))) ; for tests
    (native-inputs
     `(("chez-scheme" ,chez-scheme)))
    (arguments
     `(#:make-flags (let ((out (assoc-ref %outputs "out")))
		      `(,(string-append "PREFIX=" out)))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
    (synopsis "Portable hygienic pattern matcher for Scheme")
    (description "This package provides a superset of the popular Scheme
@code{match} package by Andrew Wright, written in fully portable
@code{syntax-rules} and thus preserving hygiene.")
    (license public-domain)))

(define-public chez-hemlock
  (package
   (name "chez-hemlock")
   (version "0.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/hemlock.git")
       (commit "d26beffa520c7ae91c1f4d9c1aaa0b0708a2a41b")))
     (sha256
      (base32 "0mylkcvi3cdyw95vhn8is4df0p6a31wyc4v1g8a0lv8lwzpi042y"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (arguments
    `(#:make-flags `(,(string-append "prefix" "=" (assoc-ref %outputs "out")))
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
	(delete 'configure)
	(replace 'install
	  (lambda* (#:key outputs #:allow-other-keys)
	    (let ((out (string-append
			(assoc-ref %outputs "out")
			"/lib/csv-site")))
	      (mkdir-p out)
	      (for-each (lambda (so)
			  (install-file so out))
			(find-files "." "\\.so"))
	      #t))))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/chez-hemlock")
   (synopsis "Datastructures for chez scheme")
   (description "Patricia trees, KD trees, heaps, algebraic graphs, queues")
   (license gpl3+)))

(define-public chez-euler
  (package
   (name "chez-euler")
   (version "0.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/chez-euler.git")
       (commit "cf3a3aa79fb7abb836aa4db4672918e0a499dd26")))
     (sha256
      (base32 "0y6l0c1v729gmpbcv5rlq8r7qzrdlqfr033a3byi68wr682z0yjs"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (propagated-inputs
    `(("chez-srfi" ,chez-srfi)
      ("chez-hemlock" ,chez-hemlock)))
   (arguments
    `(#:make-flags `(,(string-append "prefix" "=" (assoc-ref %outputs "out")))
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
	(delete 'configure)
	(replace 'install
	  (lambda* (#:key outputs #:allow-other-keys)
	    (let ((out (string-append
			(assoc-ref %outputs "out")
			"/lib/csv-site")))
	      (mkdir-p out)
	      (for-each (lambda (so)
			  (install-file so out))
			(find-files "." "\\.so"))
	      #t))))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/chez-euler")
   (synopsis "Numerical Procedures for chez scheme")
   (description "Primes, Permutations, Combinations, and so on")
   (license gpl3+)))
