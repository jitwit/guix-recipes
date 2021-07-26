(define-module (jrn packages games)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system meson))

(define-public lc0
  (package
   (name "lc0")
   (version "0.27.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/LeelaChessZero/lc0")
           (commit (string-append "v" version))
	   (recursive? #t)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "00r8bahyqq8n26h3ih8n0spq9lm44pgvmmss29n1mq45g997vbgb"))))
   (build-system meson-build-system)
   (native-inputs
    `(("ninja" ,ninja)
      ("gcc" ,gcc-9)
      ("git" ,git)
      ("cmake" ,cmake)
      ("pkg-config" ,pkg-config)
      ("gtest" ,googletest)
      ("zlib" ,zlib)
      ))
   (inputs
    `(("boost" ,boost)
      ("eigen" ,eigen)
      ("openblas" ,openblas)))
   (arguments
    '(;; #:configure-flags '("-DUSE_BLAS=YES")
      #:phases (modify-phases %standard-phases
		 (replace 'build
		   (lambda* (#:key inputs outputs #:allow-other-keys)
		     (invoke "sh" "../source/build.sh"))))))
   (home-page "https://lczero.org/")
   (synopsis "Leela Zero Chess engine.")
   (description
    "Lc0 is a UCI-compliant chess engine designed to play chess via
neural network, specifically those of the LeelaChessZero project.")
   (license license:gpl3+)))
