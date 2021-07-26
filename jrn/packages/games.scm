(define-module (jrn packages games)
  #:use-module (ice-9 match)
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson))

(define-public lc0
  (package
   (name "lc0")
   (version "0.28.0-rc1")
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
       "11n8xpd9zv7fas2r6bar344ypnfg8j3liig8cq77820rg9caiqqc"))))
   (build-system meson-build-system)
   (native-inputs
    `(("ninja" ,ninja)
      ("gcc" ,gcc-9)
      ("git" ,git)
      ("cmake" ,cmake)
      ("pkg-config" ,pkg-config)
      ("gtest" ,googletest)
      ("zlib" ,zlib)))
   (inputs
    `(("boost" ,boost)
      ("eigen" ,eigen)
      ("openblas" ,openblas)))
   (arguments
    '(#:phases (modify-phases %standard-phases
		 (replace 'build
		   (lambda* (#:key inputs outputs #:allow-other-keys)
		     (invoke "sh" "../source/build.sh"))))))
   (home-page "https://lczero.org/")
   (synopsis "Leela Zero Chess engine.")
   (description
    "Lc0 is a UCI-compliant chess engine designed to play chess via
neural network, specifically those of the LeelaChessZero project.")
   (license license:gpl3+)))

(define-public stockfish
  (let ((neural-network-revision "26abeed38351"))
    (package
      (name "stockfish")
      (version "14")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/official-stockfish/Stockfish")
               (commit (string-append "sf_" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "046b3rq9w8lzgk07q5zazzkl93ai99ab18hr9d8n73mabjpi6zbx"))))
      (build-system gnu-build-system)
      (inputs
       `(("neural-network"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://tests.stockfishchess.org/api/nn/nn-"
                                 neural-network-revision ".nnue"))
             (sha256
              (base32
               "0l1h1nb7bh0sppxw2sx6bldgnz513qzwmgj78h6kl7sihg9yxar6"))))))
      (arguments
       `(#:tests? #f
         #:make-flags (list "-C" "src"
                            "build"
			    (string-append "nnuenet=nn-"
					   ,neural-network-revision
					   ".nnue")
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out"))
                            (string-append "ARCH="
                                           ,(match (%current-system)
                                              ("x86_64-linux" "x86-64")
                                              ("i686-linux" "x86-32")
                                              ("aarch64-linux" "general-64")
                                              ("armhf-linux" "armv7")
                                              ("mips64el-linux" "general-64")
                                              (_ "general-32"))))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; The official neural network file is needed for building
                    ;; and is embedded in the resulting binary.
                    (add-after 'unpack 'copy-net
                      (lambda* (#:key inputs #:allow-other-keys)
                        (copy-file (assoc-ref inputs "neural-network")
                                   (format #f "src/nn-~a.nnue"
                                           ,neural-network-revision))
			(substitute* `("src/evaluate.h")
			  (("nn-.{12}.nnue")
			   (format #f "nn-~a.nnue" ,neural-network-revision))))))))
      (synopsis "Strong chess engine")
      (description
       "Stockfish is a very strong chess engine.  It is much stronger than the
best human chess grandmasters.  It can be used with UCI-compatible GUIs like
ChessX.")
      (home-page "https://stockfishchess.org/")
      (license license:gpl3+))))
