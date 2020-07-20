
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
    (version "0.4.0")
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
	 "1h39113rfxwxhdwvadrdfz23h9hs7rsg2xa063n4bnly88nvcjd7"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases)))
    (home-page "http://www.ats-lang.org")
    (synopsis
     "Functional programming language with dependent types")
    (description
     "Functional programming language with dependent types")
    (license gpl3+)))
