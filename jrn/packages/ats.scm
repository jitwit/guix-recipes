
(define-module (jrn packages ats)
  #:use-module (ice-9 popen)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix licenses)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (jrn packages j)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ncurses))

(define-public ats-anairiats
  (package
    (name "ats-anairiats")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "mirror://sourceforge/ats-lang/ats-lang-anairiats-"
	     version
	     ".tgz"))
       (sha256
	(base32
	 "0l2kj1fzhxwsklwmn5yj2vp9rmw4jg0b18bzwqz72bfi8i39736k"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("lapack" ,lapack)))
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
	 (add-before 'install 'bad-lib
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     ;; fails at strip due to libblas
	     (delete-file "contrib/cblas/TEST/test_lu")
	     #t)))))
    (home-page "http://www.ats-lang.org")
    (synopsis
     "Functional programming language with dependent types")
    (description
     "Functional programming language with dependent types")
    (license gpl3+)))

(define-public ats-postiats
  (package
    (name "ats-postiats")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://sourceforge.net/projects/ats2-lang/files/ats2-lang/ats2-postiats-"
	     version
	     "/ATS2-Postiats-"
	     version
	     ".tgz/download"))
       (sha256
	(base32
	 "0g5a7823739pbkicv8dmzliwxnkd6h5vz91j9ii34rzp0mrbzx03"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("ats-anairiats" ,ats-anairiats)))
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
	 (add-before 'configure 'setup-env
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (setenv "PATSHOME" (getenv "PWD"))
	     (setenv "PATH"
		     (string-append (getenv "PATH")
				    ":"
				    (getenv "PWD")
				    "/bin"))
	     #t))
	 (delete 'check)
	 (replace 'build
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (invoke "make" "-f" "Makefile_dist" "all")
	     #t))
	 (add-after 'install 'ats-emacs
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let ((emacs-ats2 (string-append (assoc-ref %outputs "out")
					      "/share/emacs/site-lisp")))
	       (mkdir-p emacs-ats2)
	       (copy-recursively "utils/emacs" emacs-ats2)
	       #t)))
	 ;; fixme: missing file raises exception
	 (delete 'validate-runpath))))
    (home-page "http://www.ats-lang.org")
    (synopsis
     "Functional programming language with dependent types")
    (description
     "Functional programming language with dependent types")
    (license gpl3+)))
