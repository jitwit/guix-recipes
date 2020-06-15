(define-module (jrn packages fonts)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression))

(define-public font-apl2741
  (package
    (name "font-apl2741")
    (version "0.0")
    (source (origin
              (method url-fetch)
              (uri "http://www.apl385.com/fonts/apl2741.zip")
              (sha256
               (base32
                "0cq9vng0mdx9g38s9dmxrlxmis02ba57sjjp579i05gacyf2axl2"))))
    (native-inputs `(("unzip" ,unzip)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       (let ((destination "/share/fonts/truetype/")
	     (font.tff "Apl2741.ttf"))
	 `((,font.tff ,(string-append destination font.tff))))
       #:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda*  (#:key inputs #:allow-other-keys)
	     (invoke "unzip" (assoc-ref inputs "source"))
	     #t)))))
    (home-page "http://www.apl385.com/")
    (synopsis "Unicode monospaced terminal-style APL font")
    (description "Historic interest only. This was hand digitised as a
PostScript Type-3 font from an original 2741 golf-ball salvaged from
an IBM selectric typewriter. Looks good on paper, but poor on screen
as it is lightweight and italic. The hinting is rudimentary and even
with ClearType it is hard to read!")
    (license license:public-domain)))

(define-public font-apl385
  (package
    (name "font-apl385")
    (version "0.0")
    (source (origin
              (method url-fetch)
              (uri "http://www.apl385.com/fonts/apl385.zip")
              (sha256
               (base32
                "132qfsnx0v6qf8x8iy3flivv449nz42nnpkwjysmz65w6wqxpk1g"))))
    (native-inputs `(("unzip" ,unzip)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       (let ((destination "/share/fonts/truetype/")
	     (font.tff "Apl385.ttf"))
	 `((,font.tff ,(string-append destination font.tff))))
       #:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda*  (#:key inputs #:allow-other-keys)
	     (invoke "unzip" (assoc-ref inputs "source"))
	     #t)))))
    (home-page "http://www.apl385.com/")
    (synopsis "Unicode monospaced APL font")
    (description "Developed with all APL software vendors in the late
1980s and enhanced as required with new characters to meet interpreter
updates. Thanks to Dave Liebtag (IBM), Richard Nabavi (APL.68000) and
all at Dyalog APL for their support. Original artwork of Adrian
Smith.")
    (license license:public-domain)))

(define-public font-apl333
  (package
    (name "font-apl333")
    (version "0.0")
    (source (origin
              (method url-fetch)
              (uri "http://www.apl385.com/fonts/apl333.zip")
              (sha256
               (base32
                "0yn0ha7d14vp4ma3lxbc9kpyrn20m7brjisr6w55c9mi24w9v3a5"))))
    (native-inputs `(("unzip" ,unzip)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       (let ((destination "/share/fonts/truetype/")
	     (font.tff "APL333.ttf"))
	 `((,font.tff ,(string-append destination font.tff))))
       #:phases
       (modify-phases %standard-phases
	 (replace 'unpack
	   (lambda*  (#:key inputs #:allow-other-keys)
	     (invoke "unzip" (assoc-ref inputs "source"))
	     #t)))))
    (home-page "http://www.apl385.com/")
    (synopsis "Unicode proportionally spaced APL font")
    (description "The same artwork as above, but proportionally
spaced, as there are many of us who see no need to mono-space code
these days! Looks splendid in Visual Studio as a C# font.")
    (license license:public-domain)))
