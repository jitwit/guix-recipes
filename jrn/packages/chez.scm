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
  #:use-module (jrn packages j)
  #:use-module ((gnu packages chez) #:select (chez-scheme chez-srfi chez-matchable))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public cs-hemlock
  (package
   (name "cs-hemlock")
   (version "0.2")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/hemlock.git")
       (commit "06b207b0df02b594cee5b47ae1c15e26bdcffd04")))
     (sha256
      (base32 "01qzfd5pakids1svr6ckfp58maxhvk63pxbmdnq0j9zlj9fnslha"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (arguments
    `(#:make-flags `(,(string-append "out" "=" (assoc-ref %outputs "out")
				     "/lib/csv-site"))
      #:tests? #f
      #:phases (modify-phases %standard-phases (delete 'configure))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/chez-hemlock")
   (synopsis "Datastructures for chez scheme")
   (description "Patricia trees, KD trees, heaps, algebraic graphs, queues")
   (license gpl3+)))

(define-public cs-euler
  (package
   (name "cs-euler")
   (version "0.2")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/chez-euler.git")
       (commit "d79b883641685282afbeba21f0a2f7dd3eee62a0")))
     (sha256
      (base32 "1779fk8459jjn1cgs69rbsrz9q3255w9bpv1igdprxp6npvilak0"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (propagated-inputs
    `(("cs-hemlock" ,cs-hemlock)))
   (arguments
    `(#:make-flags `(,(string-append "prefix" "=" (assoc-ref %outputs "out")))
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

(define-public cs-cobble
  (package
   (name "cs-cobble")
   (version "0.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/cobble.git")
       (commit "4b144e11c7e81dcc0c3a5e43ffd72f5e6bf12778")))
     (sha256
      (base32 "1h8jvkra9ndh310qgjsh7n3k28ain9rl4z2v69ljsi910djrbm45"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (propagated-inputs
    `(("cs-hemlock" ,cs-hemlock)
      ("chez-srfi" ,chez-srfi)
      ("cs-euler" ,cs-euler)
      ("chez-matchable" ,chez-matchable)))
   (arguments
    `(#:make-flags `(,(string-append "out" "=" (assoc-ref %outputs "out")))
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
       (replace 'configure
	 (lambda* (#:key outputs #:allow-other-keys)
		  (let ((gobbler.so (string-append (assoc-ref %outputs "out")
						   "/bin/gobbler.so")))
		    (substitute* `("gobbler")
				 (("gobbler.so") gobbler.so))
		    #t))))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/cobble")
   (synopsis "boggle command line solver and scheme library")
   (description "boggle command line solver and scheme library")
   (license gpl3+)))

(define-public cs-intcode
  (package
   (name "cs-intcode")
   (version "0.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/intcode.git")
       (commit "cc76d14475e46235f6e4f1c4e766ae52ec86770e")))
     (sha256
      (base32 "0s4xm5hhkm3qq9h1xznrv8v90gg4kjcd7v5k4m2xq3l6mbpkw9bv"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("chez-scheme" ,chez-scheme)))
   (arguments
    `(#:make-flags `(,(string-append "out" "=" (assoc-ref %outputs "out")
				     "/lib/csv-site"))
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
	(delete 'configure) (delete 'check))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/intcode")
   (synopsis "intcode interpreter for advent of code puzzles")
   (description "intcode interpreter for advent of code puzzles")
   (license asl2.0)))

(define-public cs-juniper
  (package
   (name "cs-juniper")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/jitwit/juniper.git")
       (commit "e00d5d2bbf0b2de61c8f05134b554fb22286a3e7")))
     (sha256
      (base32 "0wm1d05rrhms65wmnpg7pq32xkxaqnrn90s54cg9mdij60kxvrlc"))))
   (build-system gnu-build-system)
   (native-inputs `(("chez-scheme" ,chez-scheme)))
   (inputs `(("j" ,j)))
   (arguments
    `(#:make-flags `(,(string-append "out" "=" (assoc-ref %outputs "out")
				     "/lib/csv-site")
		     ,(string-append "j-bin" "=" (assoc-ref %build-inputs "j") "/bin"))
      #:tests? #f
      #:phases
      (modify-phases
       %standard-phases
       (replace 'configure
	 (lambda* (#:key outputs #:allow-other-keys)
		  (let* ((j-path (assoc-ref %build-inputs "j"))
			 (libj.so (string-append j-path "/bin/libj.so")))
		    (substitute* `("juniper.sls")
				 (("libj.so") libj.so)
				 (("\"profile.ijs\"")
				  (string-append "\"" j-path "/bin/profile.ijs\""))
				 (("^;;")
				  (string-append "(load-shared-object \""
						 libj.so "\") ;;")))
		    #t))))))
   (native-search-paths
    `(,(search-path-specification
	(variable "CHEZSCHEMELIBDIRS")
	(files
	 `(,(string-append "lib/csv-site"))))))
   (home-page "https://github.com/jitwit/juniper")
   (synopsis "interface with J from scheme")
   (description "interface with J from scheme")
   (license gpl3+)))
