;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Joseph Novakovich <josephnovakovich@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

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
