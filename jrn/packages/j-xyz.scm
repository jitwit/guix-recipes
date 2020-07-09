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

(define-module (jrn packages j-xyz)
  #:use-module (ice-9 popen)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) :select (expat gpl2+ asl2.0))
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (jrn packages j)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ncurses))

;;;; Graphics Addons
(define-public j-media-imagekit
  (package
    (name "j-media-imagekit")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/media_imagekit.git")
             (commit "6de6b5eb1305427a99e0eb59285d421959b9c1bb")))
       (sha256
        (base32 "17i2dfdmx6bsa826a1xfw8xg9r855cw8yq8asgfiv3vcvdzqpqa8"))))
    (propagated-inputs
     `(("j-graphics-viewmat" ,j-graphics-viewmat)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/media/imagekit")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/media_imagekit")
    (synopsis
     "Utilities for accessing 24-bit jpeg, png, bmp image files in J")
    (description
     "The image kit package provides utilities for accessing 24-bit jpeg, png image files in J. The core functions allow reading and writing image files as 3-dimensional J arrays.\n\nThe addon includes several scripts. The main script, imagekit.ijs, provides J functions for the basic image reading, writing, and viewing images through other J addons. Another script, html_gallery.ijs, provides J functions that create thumbnails and image...\n\n")
    (license expat)))

(define-public j-graphics-jpeg
  (package
    (name "j-graphics-jpeg")
    (version "1.0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/graphics_jpeg.git")
             (commit
              "1234416aff49164a14a73cc73cf47d4cde634b52")))
       (sha256
        (base32
         "0s2xmg6fv1kxhzkkjxvb3w424zk71iij2i9qizr1w81l76nk945j"))))
    (propagated-inputs `(("j-graphics-pplatimg" ,j-graphics-pplatimg)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/graphics/jpeg")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/graphics_jpeg")
    (synopsis "jpeg utilities")
    (description "Utilities for *.jpg files\n\n")
    (license expat)))

(define-public j-graphics-bmp
  (package
    (name "j-graphics-bmp")
    (version "1.0.14")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_bmp.git")
         (commit "edcffe8d7847e9941d14f6838110a068e0a89102")))
       (sha256
        (base32 "00k417ia5fvszmb4pnd4japrn8i15ync8n4fk1l7i7fbafinxrnr"))))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/graphics/bmp")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_bmp")
    (synopsis "Utilities for *.bmp files")
    (description "Utilities for *.bmp files")
    (license expat)))

(define-public j-graphics-color
  (package
    (name "j-graphics-color")
    (version "1.0.19")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_color.git")
         (commit "2bb8578c370fd2f25b118ae8cba11153c4687eab")))
       (sha256
        (base32 "155m56f0d268jc8g9yc6fw5l8mnx2sm408if865h0a7682mpgqrb"))))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/graphics/color")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_color")
    (synopsis "Color tables and related scripts")
    (description "Color tables and related scripts")
    (license expat)))

(define-public j-graphics-pplatimg
  (package
    (name "j-graphics-pplatimg")
    (version "1.0.03")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_pplatimg.git")
         (commit "a635da57097649ad434680e6006679e65f2711eb")))
       (sha256
        (base32 "034px2d9m1p49jz75469dqx2z1m1zds2jg7rq9c5qjmz7q5kbli3"))))
    (inputs `(("glibc" ,glibc)
	      ("gdk-pixbuf" ,gdk-pixbuf)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("pplatimg.ijs")
               (("LIBGLIB=: 'libgobject-2.0.so.0 '")
                (string-append "LIBGLIB=: '"
                               (assoc-ref %build-inputs "glib")
                               "/lib/libgobject-2.0.so.0 '"))
               (("LIBGDKPIX=: 'libgdk_pixbuf-2.0.so.0 '")
                (string-append "LIBGDKPIX=: '"
                               (assoc-ref %build-inputs "gdk-pixbuf")
                               "/lib/libgdk_pixbuf-2.0.so.0 '")))))
         (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/graphics/pplatimg")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_pplatimg")
    (synopsis "Platform neutral image I/O utilities")
    (description "Implementations for Windows, Linux and Mac OS X.
Supports BMP, GIF, JPEG, PNG, TIFF, Exif, ICO, WMF,
and EMF formats where available. Returns pixel matrix
in ARGB (Alpha most significant) integer format.
Expects ARGB, or triples of RGB in any axis of rank 3 array.
Good for glpixels. Uses GDI+, Core Graphics (Quartz),
The gdk-pixbuf Library from GTK+.
Ported to 64-bit platforms by Bill Lam
Based on media/platimg developed by Oleg Kobchenko")
    (license expat)))

(define-public j-graphics-png
  (package
    (name "j-graphics-png")
    (version "1.0.28")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_png.git")
         (commit "2767c9b8efea71c38b0d8433bd58aba360ea464a")))
       (sha256
        (base32 "1i5i9x7am36dr58bvlhydyp3bhmhbgg355k9jfddjylbrsnb7rc9"))))
    (propagated-inputs `(("j-arc-zlib" ,j-arc-zlib)))
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
                                       "/share/j/addons/graphics/png")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_png")
    (synopsis "")
    (description "This J addon provides an interface to zlib.")
    (license expat)))

(define-public j-graphics-cairo
  (package
    (name "j-graphics-cairo")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_cairo.git")
         (commit "e23236060339cdf4b4bec1da9dbca911c2230ad8")))
       (sha256
        (base32 "15f22zxyfj96a2ak6pdclg0jzc81a9x96hwcv5b2s7kbgpnj6cw8"))))
    (inputs `(("cairo" ,cairo)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("cairo.ijs")
               (("libcairo=: 'libcairo.so.2'")
                (string-append "libcairo=: '"
                               (assoc-ref %build-inputs "cairo")
                               "/lib/libcairo.so.2'")))))
         (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/graphics/cairo")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_cairo")
    (synopsis "")
    (description "This J addon provides an interface to zlib.")
    (license expat)))

(define-public j-graphics-afm
  (package
    (name "j-graphics-afm")
    (version "1.0.15")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_afm.git")
         (commit "a5dd48482aa0f37a1e917d3651148203312a8107")))
       (sha256
        (base32 "0kzl810n43cpibygvwvyahk99sywmpcg317wisd841adnyqfbfkx"))))
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
                                       "/share/j/addons/graphics/afm")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_afm")
    (synopsis "AFM")
    (description "Adobe Font Metrics.")
    (license expat)))

(define-public j-graphics-plot
  (package
    (name "j-graphics-plot")
    (version "1.0.193")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_plot.git")
         (commit "900efc7c70923f8b2592e6d0ecefa77b00b8ece2")))
       (sha256
        (base32 "04n07988gp5a424sgpasivs2mb3yiqfaxnkp4gw0xf5gqx8v3ams"))))
    (propagated-inputs `(("j-graphics-afm" ,j-graphics-afm)
			 ("j-graphics-bmp" ,j-graphics-bmp)
			 ("j-graphics-color" ,j-graphics-color)
			 ("j-graphics-png" ,j-graphics-png)
			 ("j-math-misc" ,j-math-misc)
			 ("j-general-misc" ,j-general-misc)))
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
                                       "/share/j/addons/graphics/plot")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_plot")
    (synopsis "Plot")
    (description "Plot Package.")
    (license expat)))

(define-public j-graphics-viewmat
  (package
    (name "j-graphics-viewmat")
    (version "1.0.83")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/graphics_viewmat.git")
         (commit "ed86115ed60a43a506f4ec9c827963866b738217")))
       (sha256
        (base32 "1jv52s3fi6wrqjdi2p45v7mjy2jr5zmkq213axjgbgm2f4b3m458"))))
    (propagated-inputs `(("j-graphics-bmp" ,j-graphics-bmp)
			 ("j-graphics-color" ,j-graphics-color)
			 ("j-graphics-png" ,j-graphics-png)))
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
                                       "/share/j/addons/graphics/viewmat")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/graphics_viewmat")
    (synopsis "Displays a viewmat")
    (description "Viewmat displays tables of data graphically.")
    (license expat)))

;; todo figure out how gnuplot binary is set to hard link... or just run...
;;(define-public j-graphics-gnuplot
;;  (package
;;    (name "j-graphics-gnuplot")
;;    (version "1.0.7")
;;    (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;               (url "https://github.com/jsoftware/graphics_gnuplot.git")
;;               (commit
;;                 "1b2d3f3e9bc206d13921f16982d5b52bca44c4f7")))
;;        (sha256
;;          (base32
;;            "0kz3gjykyrpkvn1agp7hc4rgzyb3kvnrs3k1jwrk20agjn2nzqn0"))))
;;    (propagated-inputs '())
;;    (outputs '("out"))
;;    (build-system gnu-build-system)
;;    (arguments
;;      `(#:modules
;;        ((guix build gnu-build-system)
;;         (guix build utils))
;;        #:phases
;;        (modify-phases
;;          %standard-phases
;;          (delete 'configure)
;;          (delete 'check)
;;          (delete 'build)
;;          (replace
;;            'install
;;            (lambda _
;;              (let ((out (string-append
;;                           (assoc-ref %outputs "out")
;;                           "/share/j/addons/graphics/gnuplot")))
;;                (copy-recursively "." out)
;;                #t))))))
;;    (home-page
;;      "https://github.com/jsoftware/graphics_gnuplot")
;;    (synopsis "Create gnuplot graphics\n")
;;    (description
;;      "The gnuplot addon creates gnuplot graphics from data in J.\n\nThis works with gnuplot V4.0 or later, and may work with earlier versions.\n\nThe original script was contributed by L. Fraser Jackson.\n\n")
;;    (license expat)))

(define-public j-api-gles
  (package
    (name "j-api-gles")
    (version "1.0.31")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/api_gles.git")
         (commit "a3709bb1a087c8439cc2afedd2cc749799422327")))
       (sha256
        (base32 "0fwp8kl27vdix4nz5z0i0b80jwgbk58ck4hg93gcvzn6l40sgsir"))))
    (inputs `(("mesa" ,mesa)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("gles.ijs")
               (("libGLESv2.so.2")
                (string-append (assoc-ref %build-inputs "mesa")
                               "/lib/libGLESv2.so.2"))
               (("libEGL.so.1")
                (string-append (assoc-ref %build-inputs "mesa")
                               "/lib/libEGL.so.1"))
               (("libGL.so.1")
                (string-append (assoc-ref %build-inputs "mesa")
                               "/lib/libGL.so.1")))))
         (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/api/gles")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/api_gles")
    (synopsis "Modern OpenGL API")
    (description "OpenGL 4.2 and OpenGL ES 3.1 API definitions
see jwiki http://code.jsoftware.com/wiki/OpenGL%20ES")
    (license expat)))

;;;; Mathematical Addons
(define-public j-math-calculus
  (package
    (name "j-math-calculus")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/jsoftware/math_calculus.git")
	     (commit
	      "9a5cc851423b8c25e09c3be8e3066483d1b895f2")))
       (sha256
	(base32
	 "0jayryq57cxbmwh9r490q8d2dsk2400fvk8fvymbhbjhk305ryrh"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
	(guix build utils))
       #:phases
       (modify-phases
	   %standard-phases
	 (delete 'configure)
	 (delete 'check)
	 (delete 'build)
	 (replace
	     'install
	   (lambda _
	     (let ((out (string-append (assoc-ref %outputs "out")
				       "/share/j/addons/math/calculus")))
	       (copy-recursively "." out)
	       #t))))))
    (home-page
     "https://github.com/jsoftware/math_calculus")
    (synopsis "symbolic differentiation and integration")
    (description
     "Conjunctions to perform differentiation and integration of J verbs, and secant-slope approximation for verbs that cannot be handled symbolically\n\n")
    (license expat)))

(define-public j-math-fftw
  (package
    (name "j-math-fftw")
    (version "1.1.17")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/math_fftw.git")
         (commit "7e7e2bbe90cfe479235aebc5335eb0e09a875654")))
       (sha256
        (base32 "0fk43crxrfy2z694lkpw5sgwf7i5ykm3c6x9k52vnljg0kgi5dgp"))))
    (inputs `(("fftw" ,fftw)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("fftw.ijs")
               (("DLL=: 'libfftw3.so.3'")
                (string-append "DLL=: '"
                               (assoc-ref %build-inputs "fftw")
                               "/lib/libfftw3.so'")))
             #t))
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/math/fftw")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/math_fftw")
    (synopsis "FFTW")
    (description "FFTW (Fastest Fourier Transform in the West) is a
collection of fast C routines for computing the Discrete Fourier Transform in
one or more dimensions. It includes complex, real, and parallel transforms,
and can handle arbitrary array sizes efficiently. The FFTW Add-On consists of
a DLL incorporating the FFTW routines, plus supporting J scripts and
labs. FFTW and the FFTW package are distributed under the terms of the GNU
General Public License. For more information on GNU, see the GNU web page.

FFTW is available under Windows, Mac and Linux.
")
    (license gpl2+)))

(define-public j-stats-base
  (package
    (name "j-stats-base")
    (version "1.0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/stats_base.git")
             (commit "ad7ffc494cd5a51d715dffa32e01dc1974adc992")))
       (sha256
        (base32 "11zyj473fb7k5708hybkvz7mnlr1zln8rhaqhzkznqcq5l6hhqjn"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/stats/base")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/stats_base")
    (synopsis "Basic statistics package")
    (description
     "Basic statistics package.\n\nMigrated from system/packages/stats.\n\n")
    (license expat)))

(define-public j-stats-distribs
  (package
    (name "j-stats-distribs")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/stats_distribs.git")
             (commit
              "fc3bf537748fa424a580326c5789e1248ea64d58")))
       (sha256
        (base32
         "0xjlnj9q0vpigp0zyhvmy9i59k6n4fzb4wwj0yw8shnpbaj9lzwc"))))
    (propagated-inputs `(("j-stats-base" ,j-stats-base)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/stats/distribs")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/stats_distribs")
    (synopsis
     "Verbs for working with distributions")
    (description
     "Verbs for working with statistical distributions.\nCurrently covers normal and uniform distributions.\n\nAddon compiled by Ric Sherlock & Fraser Jackson from many contributions by the J community.\n\n")
    (license expat)))

;; does some mutation to start
;; (define-public j-math-uu
;;   (package
;;     (name "j-math-uu")
;;     (version "2.1.26")
;;     (source
;;       (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;                (url "https://github.com/earthspot/math_uu.git")
;;                (commit
;;                  "a2f27a49aa8041ead449723749285dca1a2d818f")))
;;         (sha256
;;           (base32
;;             "1hk156ncw45j4akxim6kvgmfk19digs0vwb0n9pr5a9yfqvqrf0n"))))
;;     (propagated-inputs `(("j-format-zulu" ,j-format-zulu)))
;;     (outputs '("out"))
;;     (build-system gnu-build-system)
;;     (arguments
;;       `(#:modules
;;         ((guix build gnu-build-system)
;;          (guix build utils))
;;         #:phases
;;         (modify-phases
;;           %standard-phases
;;           (delete 'configure)
;;           (delete 'check)
;;           (delete 'build)
;;           (replace
;;             'install
;;             (lambda _
;;               (let ((out (string-append
;; 			  (assoc-ref %outputs "out")
;; 			  "/share/j/addons/math/uu")))
;;                 (copy-recursively "." out)
;;                 #t))))))
;;     (home-page
;;       "https://github.com/earthspot/math_uu")
;;     (synopsis
;;       "Scientific units conversion package\n")
;;     (description
;;       "UU (units-to-units) is a scientific units conversion package\nbased on the SI system of units.\n\n")
;;     (license expat)))

;; https://github.com/jsoftware/math_lapack2.git
(define-public j-math-lapack2
  (package
    (name "j-math-lapack2")
    (version "1.0.08")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/math_lapack2.git")
         (commit "1eaec0d8a39dd687195dcf5b925ccd73ddab1b4c")))
       (sha256
        (base32 "0gjkbk9n9gc5fc063wxqmwdw37fb4m8cdsalqwjbdq8yijsqrv6k"))))
    (inputs `(("lapack" ,lapack)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("lapack2.ijs")
               (("liblapack.so.3")
                (string-append (assoc-ref %build-inputs "lapack")
                               "/lib/liblapack.so.3")))
             #t))
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/math/lapack2")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/math_lapack2")
    (synopsis "Linear Algebra Package through J")
    (description "LAPACK (Linear Algebra Package) is a set of routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems. The associated matrix factorizations (LU, Cholesky, QR, SVD, Schur, generalized Schur) are also provided, as are related computations such as reordering of the Schur factorizations and estimating condition numbers.
This addon is a leaner version of another math/lapack addon which is no longer maintained.
Binary for Mac/iOS is provided by the veclib framework.
Binary for Linux, install liblapack3 (or similar) from your distro repository. If available, install libopenblas-base or libatlas3-base which provides an optimized version of BLAS.
For Windows, run getbin_jlapack2_'' to install the shared library.
Both Windows and Android binary provided here use reference BLAS.
Reference BLAS implementation may be orders of magnitude slower than optimized implementations. Build your own optimized BLAS if speed performance is critical.
See wiki page: code.jsoftware.com/wiki/Vocabulary/LAPACK")
    (license gpl2+)))

(define-public j-math-misc
  (package
    (name "j-math-misc")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/math_misc.git")
         (commit "ee67a58fd84d6fe11ad636d703457cb3d7bcd4fc")))
       (sha256
        (base32 "0j2m90nz9r66dccivzzr215fy7k5pk4brkpwl2skqjgkqxd163ln"))))
    (inputs '())
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
                                       "/share/j/addons/math/misc")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/math_misc")
    (synopsis "misc math scripts")
    (description "amoeba.ijs Nelder-Mead multi-dimentional minimization, aka the amoeba method
bigpi.ijs Calculate several digits of pi
brent.ijs Brent's method in J
build.ijs
cheby.ijs Chebyshev approximation
contfrac.ijs Continued fraction utilities
det.ijs Definitions for determinants
fermat.ijs Fermat factorization
gamesolver.ijs Find optimal mixed strategies for 2-person games
gcd.ijs Calculate GCD
integer.ijs Verbs to generate various integer sequences
integrat.ijs Various methods for numeric integration
jacobi.ijs Jacobi's method for eigenvalues and vectors
legendre.ijs Legendre symbol and quadratic residues
linear.ijs Solve linear equations
makemat.ijs Make various standard matrices
matfacto.ijs Matrix factorization
mathutil.ijs Math utilities
matutil.ijs Matrix utilities
mean.ijs Various means
numbers.ijs Various number definitions (Stirling, Euler ...)
odeint.ijs Solve initial value ordinary differential equations
pollard.ijs Pollard factorizations
poly.ijs Polynomial functions
primutil.ijs Primes - prime testing programs
quatern.ijs Definitions for quaternions
rsa.ijs Examples of RSA encryption
simplex.ijs Simplex method
simplexnr.ijs Simplex method (after Numerical Recipes in C)
spline.ijs Spline utilities
svd.ijs Singular value decomposition
trig.ijs Trigonometric functions")
    (license expat)))

;;;; Compression Addons
(define-public j-arc-zlib
  (package
    (name "j-arc-zlib")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/arc_zlib.git")
         (commit "be37111ffaabd9189f295f35714ffcdf7cf34de3")))
       (sha256
        (base32 "1was81f9dwfh63kh18k1gdc9xmj16vh1cgzfcipyjjdrx93jg68x"))))
    (inputs `(("zlib" ,zlib)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* '("zlib.ijs")
               (("zlib=: .+$")
                (string-append "zlib=: '"
                               (assoc-ref %build-inputs "zlib")
                               "/lib/libz.so'\n")))
             #t))
         (delete 'check) (delete 'build)
	 (replace 'install
	   (lambda _
	     (let ((out (string-append (assoc-ref %outputs "out")
				       "/share/j/addons/arc/zlib")))
	       (copy-recursively "." out)
	       #t))))))
    (home-page "https://github.com/jsoftware/arc_zlib")
    (synopsis "Interface with zlib")
    (description "This J addon provides an interface to zlib.")
    (license expat)))

;; depends on arc/zip, todo 
;; (define-public j-arc-ziptrees
;;   (package
;;     (name "j-arc-ziptrees")
;;     (version "1.0.13")
;;     (source
;;       (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;                (url "https://github.com/jsoftware/arc_ziptrees.git")
;;                (commit
;;                  "be0206c115b13073f72f5f551e68ca8842fa8494")))
;;         (sha256
;;           (base32
;;             "1n5sr21yf17c0ycf2857qnkl598j77c5c86yx2yqaz9iq4zgrgs2"))))
;;     (propagated-inputs
;;      `(("j-arc-zip" ,j-arc-zip)
;;        ("j-general-dirutils" ,j-general-dirutils)))
;;     (outputs '("out"))
;;     (build-system gnu-build-system)
;;     (arguments
;;       `(#:modules
;;         ((guix build gnu-build-system)
;;          (guix build utils))
;;         #:phases
;;         (modify-phases
;;           %standard-phases
;;           (delete 'configure)
;;           (delete 'check)
;;           (delete 'build)
;;           (replace
;;             'install
;;             (lambda _
;;               (let ((out (string-append
;;                            (assoc-ref %outputs "out")
;;                            "/share/j/addons/arc/ziptrees")))
;;                 (copy-recursively "." out)
;;                 #t))))))
;;     (home-page
;;       "https://github.com/jsoftware/arc_ziptrees")
;;     (synopsis "Zips and Unzips directory trees")
;;     (description
;;       "Zips and unzips directory trees to and from zip files.\nUses the the arc/zip addon.\n\nContributed by Ric Sherlock\n\n")
;;     (license expat)))

;;;; Data Addons
(define-public j-data-ddmysql
  (package
    (name "j-data-ddmysql")
    (version "1.0.15")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/data_ddmysql.git")
               (commit
                 "94ee53df3d0f3f7df06c4798446a71fb5dd90a39")))
        (sha256
          (base32
            "09as7zry3kwlh9n10xgxp5xbqdqbvdgl9fdnjl3icsc6fmkd186a"))))
    (inputs `(("mysql" ,mysql)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (replace 'configure
	    (lambda _
	      (substitute* '("ddmysql.ijs")
		(("libmysqlclient.so.18")
		 (string-append (assoc-ref %build-inputs "mysql")
				"/lib/libmysqlclient.so")))
	      #t))
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/data/ddmysql")))
                (copy-recursively "." out)
                #t))))))
    (home-page
      "https://github.com/jsoftware/data_ddmysql")
    (synopsis "Data driver for MySQL")
    (description
      "Accessing MySQL server with native C API using the data driver (JDD) interface such ddcon and ddfet.  Requires MySQL client library or Connector/C.\n\nFor Linux, install libmysqlclient (or similar) from your distro repository.  Or download from http://www.mysql.com/downloads.html\n\nSee wiki page http://code.jsoftware.com/wiki/JDD for help.\n\n")
    (license expat)))

(define-public j-data-ddsqlite
  (package
    (name "j-data-ddsqlite")
    (version "1.0.42")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/data_ddsqlite.git")
               (commit
                 "245e32800356f9d4a365c971d9ece1c534a978ce")))
        (sha256
          (base32
            "18gqm8s7fva0l3wv3bqcfl0axmjyap1ixwf1jgwlnd1wppvk8v5w"))))
    (inputs `(("sqlite" ,sqlite)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (replace 'configure
	    (lambda _
	      (substitute* '("ddsqlite.ijs")
               (("libsqlite=: ")
                (string-append "libsqlite=: '"
                               (assoc-ref %build-inputs "sqlite")
                               "/lib/libsqlite3.so' NB. ")))
	      #t))
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/data/ddsqlite")))
                (copy-recursively "." out)
                #t))))))
    (home-page
     "https://github.com/jsoftware/data_ddsqlite")
    (synopsis "Data driver for SQLite")
    (description
      "Accessing SQLite database with C API using the data driver (JDD) interface such as ddcon and ddfet.  Requires sqlite 3.6.0 or newer.\n\nSee readme.txt for shared library installation.\n\nSee wiki page http://code.jsoftware.com/wiki/JDD for help.\n\n")
    (license expat)))

;; wants to use libjsqlite3.so which is downloaded from
;; jsoftware.so. unclear what difference is
;; (define-public j-data-sqlite
;;   (package
;;     (name "j-data-sqlite")
;;     (version "1.0.35")
;;     (source
;;       (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;                (url "https://github.com/cdburke/data_sqlite.git")
;;                (commit
;;                  "331e04b7a357d47284adb71cc38bcbc563572f43")))
;;         (sha256
;;           (base32
;;             "0kahwwvrd1aksbr5wk7jqq2zsfr20brzflka24iqqmqqf2995p77"))))
;;     (inputs `(("sqlite" ,sqlite)))
;;     (outputs '("out"))
;;     (build-system gnu-build-system)
;;     (arguments
;;       `(#:modules
;;         ((guix build gnu-build-system)
;;          (guix build utils))
;;         #:phases
;;         (modify-phases
;;           %standard-phases
;;           (replace 'configure
;; 	    (lambda _
;; 	      (substitute* '("sqlite.ijs")
;;                (("libsqlite=: ")
;;                 (string-append "libsqlite=: '"
;;                                (assoc-ref %build-inputs "sqlite")
;;                                "/lib/libsqlite3.so' NB. "))
;;                (("binreq=: 108") "binreq=: 008"))
;; 	      #t))
;;           (delete 'check)
;;           (delete 'build)
;;           (replace 'install
;;             (lambda _
;;               (let ((out (string-append
;;                            (assoc-ref %outputs "out")
;;                            "/share/j/addons/data/sqlite")))
;;                 (copy-recursively "." out)
;;                 #t))))))
;;     (home-page
;;       "https://github.com/cdburke/data_sqlite")
;;     (synopsis "Sqlite enhanced API for J")
;;     (description
;;       "Sqlite allows direct access to sqlite databases.\n\nSee wiki page: code.jsoftware.com/wiki/Addons/data/sqlite/Overview\n\n")
;;     (license expat)))

(define-public j-data-jmf
  (package
    (name "j-data-jmf")
    (version "1.0.33")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/data_jmf.git")
         (commit "6ca036cab3808b352a7b08777dc47b9d76a0bc9d")))
       (sha256
        (base32 "1pjkm520427l1wwh4mkxpnkn4v6j9n5ki2gaiaax2ziky7v3zmvw"))))
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
                                       "/share/j/addons/data/jmf")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/data_jmf")
    (synopsis "J Memory Mapped File")
    (description "J Memory Mapped File")
    (license expat)))

(define-public j-data-jfiles
  (package
    (name "j-data-jfiles")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/data_jfiles.git")
             (commit "58249adde967f6c23b580625f41be8aca0ce1c40")))
       (sha256
        (base32 "12f1552z1v4f4mvs39qcn0dlxsrp5c3h5wc3b8msyhd4vx9pic7r"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/data/jfiles")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/data_jfiles")
    (synopsis "J component file and keyed file")
    (description
     "This script contains definitions for the J component file and\nkeyed file system.\n\nA keyed file is a J component file in which data is accessed\nusing keywords.\n\n")
    (license expat)))

(define-public j-data-jd
  (package
    (name "j-data-jd")
    (version "4.4.83")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/data_jd.git")
             (commit
              "4f73704ab64e9c9cb323292340c87db936f0fa14")))
       (sha256
        (base32
         "0w8s1h0ji7shwqfnv3yfg8bh586h2f199l3r4dbaa58hp3h4gkwi"))))
    (propagated-inputs
     `(("j-convert-pjson" ,j-convert-pjson)
       ("j-data-jfiles" ,j-data-jfiles)
       ("j-data-jmf" ,j-data-jmf)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/data/jd")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/data_jd")
    (synopsis "Jd")
    (description
     "Jd is a commercial database product from Jsoftware.\nAlthough similar in terminology and features to\nMySQL, Oracle, DB2, SQL Server, and others, it is closer\nin spirit and design to Kx's kdb, Jsoftware's free JDB,\nand old APL financial systems on mainframes in 70s and 80s.\n\nThe key difference between Jd and most other systems \nis that Jd comes with a fully integrated and mature\nprogramming language. Jd is implemented in J and lives\nopenly and dynamically in the J execution and development\nenvironment. Jd is a natural extension of J and the full power\nof J is available to the Jd database application developer.\nThe integration is not just available to you,\nit is unabashedly pushed to you for exploitation.\n\nJd is a columnar (column oriented) RDBMS.\n\nJd is particularly suited to analytics.\nIt works well with large tables (100s of millions of rows),\nmultiple tables connected by complex joins, structured data,\nnumerical data, and complex queries and aggregations.\n\n")
    (license 'commercial)))

(define-public j-convert-pjson
  (package
    (name "j-convert-pjson")
    (version "1.0.23")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/cdburke/convert_pjson.git")
         (commit "a64defe9adb24a0350517ab99121e8c75259983e")))
       (sha256
        (base32 "1km259hnvc1qwxhvb6nz7pk79jz5rn62g43yhn6ma5bvfz5hj35r"))))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/convert/pjson")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/cdburke/convert_pjson")
    (synopsis "json library for J")
    (description "This J addon provides json serialization from within J.")
    (license expat)))

(define-public j-convert-json
  (package
    (name "j-convert-json")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/convert_json.git")
         (commit "7fdff1f8898b958c14c1035f6b42e494bd98fd0d")))
       (sha256
        (base32 "1896hjd43lzmrrags4srgm73r0lf36b89x1z2vdikdwwrksrr9ms"))))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/convert/json")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/convert_json")
    (synopsis "json")
    (description "json encoder/decoder.")
    (license expat)))

(define-public j-tables-dsv
  (package
    (name "j-tables-dsv")
    (version "1.0.14")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/tables_dsv.git")
         (commit "6c3565993a95f977d93a219c95eff202d0b1845b")))
       (sha256
        (base32 "1znp1qqmb4060qh0xjfxrx6016128w6zkpb51imja2gsqvg0lnnw"))))
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
                                       "/share/j/addons/tables/dsv")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/tables_dsv")
    (synopsis "Read/write delimiter-separated files and strings")
    (description "Reads/writes/appends Delimiter-separated value (DSV) files and strings.
Supports user-defined field and string delimiters.
Contributed by Ric Sherlock.")
    (license expat)))

(define-public j-tables-csv
  (package
    (name "j-tables-csv")
    (version "1.0.16")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/tables_csv.git")
         (commit "3f0d75d1cd4822398d2185c77c0fd1d0ab2af140")))
       (sha256
        (base32 "0qp0d1xivpsmkbrnd0lk7q4dryim48gvwjbrn5r10l07xqqvya8d"))))
    (outputs '("out"))
    (inputs `(("j-tables-dsv" ,j-tables-dsv)))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) (delete 'check) (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/j/addons/tables/csv")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/tables_csv")
    (synopsis "Read and write CSV files and strings")
    (description "Reads/writes/appends Comma-separated value (CSV) files and strings.
Generally better performance than the base library CSV script.
Note: requires that the tables/dsv addon also be installed.
Contributed by Ric Sherlock.")
    (license expat)))

;;;; General Addons
(define-public j-general-jod
  (package
    (name "j-general-jod")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/general_jod.git")
               (commit
                 "28954f977e22acfd0d1470a889f0d0442d198563")))
        (sha256
          (base32
            "1zjn757y0p7av6hsxy856q24zpg3nrr04hvm5xjikmz8gybzvzcr"))))
    (propagated-inputs `(("j-data-jfiles" ,j-data-jfiles)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/general/jod")))
                (copy-recursively "." out)
                #t))))))
    (home-page "https://github.com/jsoftware/general_jod")
    (synopsis "JOD J Object Dictionary")
    (description
      "JOD (J Object Dictionary) is a J programming and refactoring \ntool. JOD supports linked dictionary databases and provides \ntools for building, documenting and testing J systems. \n\nJOD requires the (data/jfiles) addon. \n\nIf you install JOD it is highly recommended that you\ninstall the related addon JODDOCUMENT. JODDOCUMENT \ninstalls detailed PDF documentation for JOD. \n\nFor more information see the JOD wiki pages (Info Button),\nrun the lab \"JOD lab JOD Introduction\" - JOD labs are under \nthe General lab category. If you do not see JOD labs listed\non the Lab menus use the Lab browse button to access:\n\n   ~addons/general/jod/jodlabs\n\nMore information is available on the JOD Page and GitHub at:\n  \n   https://analyzethedatanotthedrivel.org/the-jod-page/\n   https://github.com/bakerjd99/jod  \n\n")
    (license expat)))

(define-public j-general-joddocument
  (package
    (name "j-general-joddocument")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/general_joddocument.git")
               (commit
                 "49ce6c6adb2e560b63b034ac675af96b0fc5bd75")))
        (sha256
          (base32
	   "0ck04m508rgv3nxb6czaxa62s3vn5a4skny3mhwlyczmnwgj3gi2"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
			  (assoc-ref %outputs "out")
			  "/share/j/addons/general/joddocument")))
                (copy-recursively "." out)
                #t))))))
    (home-page "https://github.com/jsoftware/general_joddocument")
    (synopsis "JOD Documentation")
    (description
      "JOD Document contains (jod.pdf) which documents the JOD system.\n\nJOD Document also contains pdf versions of Jupyter\nnotebooks that show how to use various JOD facilities.\nSee the directory:\n\njpath '~addons/general/joddocument/pdfdoc'\n\nFor more information see the JOD wiki pages\n(Info Button) or visit The JOD Page at:\n\nhttps://analyzethedatanotthedrivel.org/the-jod-page/\n\nLaTeX document source is available on GitHub at:\n\nhttps://github.com/bakerjd99/joddoc\n\nJohn Baker\nbakerjd99@gmail.com\n\n")
    (license expat)))

(define-public j-general-jodsource
  (package
    (name "j-general-jodsource")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/general_jodsource.git")
               (commit
                 "921ee10dfb4e681fc759e5040e95d4dbf9921310")))
        (sha256
          (base32
            "0sxikk60w4xxih9299wnfqbkdjdn965vk00cld2zpfk9wk5d586c"))))
    (propagated-inputs `(("j-general-jod" ,j-general-jod)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace 'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/general/jodsource")))
                (copy-recursively "." out)
                #t))))))
    (home-page
      "https://github.com/jsoftware/general_jodsource")
    (synopsis "JOD Object Dictionary Source\n")
    (description
      "(jodsource) contains serialized dictionary dump scripts \n(joddev,jod,utils). These dump scripts contain all the\ncode, test cases, build scripts, and documentation used\nto develop and maintain JOD and related systems.\n\nDump scripts are large J scripts that load dictionary\nobjects into JOD dictionaries when run.\n\nJOD system dictionaries are good examples of how JOD \ncan be used to build complex J systems.\n\nJOD requires the (data/jfiles) addon. \n\nJOD source dictionaries can be set up in two ways:\n\n1) Run the J script \n\n   ~addons/general/jodsource/jodsourcesetup.ijs\n\n   This is the recommended method. (jodsourcesetup.ijs) \n   can take a minute or so to run.\n \n2) Run the JOD lab \"JOD Source Code Dump Scripts.\" \n   JOD labs are in the (General) lab category. If you\n   do not see JOD labs listed on the Lab menus use the Lab\n   browse button to access:\n\n   ~addons/general/jod/jodlabs\n\nMore information is available on the JOD Page at:\n  \n   https://analyzethedatanotthedrivel.org/the-jod-page/\n\n")
    (license expat)))

(define-public j-misc-miscutils
  (package
    (name "j-misc-miscutils")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/misc_miscutils.git")
             (commit
              "3fea111992b16f8e5d476caa5dc4567afe885c48")))
       (sha256
        (base32
         "0smd6gq1agmy0pll2vp64vbihilzl3s1g7761mnfq2sipb3ykm22"))))
    (propagated-inputs `(("j-format-printf" ,j-format-printf)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/misc/miscutils")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/misc_miscutils")
    (synopsis "Miscellaneous utility definitions")
    (description
     "langexten contains basic extensions to J.\nklutils contains function for 'keyed lists', which are tables where specified columns are 'key' and the rest are 'data'\nutils contains a grab-bag of definitions\n\n")
    (license expat)))

(define-public j-api-ncurses
  (package
    (name "j-api-ncurses")
    (version "1.0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/api_ncurses.git")
               (commit
                 "88d3543be292a3cb79d22a02af9fb972bc014636")))
        (sha256
          (base32
            "0zcr3knh1hlvr0ydslhmlb5z8jpr91w7rzyaxq4zrbk65ydsbzl2"))))
    (propagated-inputs `(("ncurses" ,ncurses)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (replace 'configure
	    (lambda _
	      (substitute* '("ncurses.ijs")
		(("libncurses.so.5")
		 (string-append (assoc-ref %build-inputs "ncurses")
				"/lib/libncurses.so"))
		(("libncursesw.so.5")
		 (string-append (assoc-ref %build-inputs "ncurses")
				"/lib/libncursesw.so"))
		(("libpanel.so.5")
		 (string-append (assoc-ref %build-inputs "ncurses")
				"/lib/libpanel.so"))
		(("libpanelw.so.5")
		 (string-append (assoc-ref %build-inputs "ncurses")
				"/lib/libpanelw.so")))
	      #t))
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
			   "/share/j/addons/api/ncurses")))
                (copy-recursively "." out)
                #t))))))
    (home-page
      "https://github.com/jsoftware/api_ncurses")
    (synopsis "NCURSES/PDCURSES API\n")
    (description
      "CRT screen handling and optimization package\n\nTo install shared library/dll,\nlinux: (debian) sudo aptitude install libncurses5 (or libncursesw5 for unicode)\nwin32: download pre-compiled binary from http://pdcurses.sourceforge.net/\n\n")
    (license expat)))

(define-public j-format-zulu
  (package
    (name "j-format-zulu")
    (version "0.0.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/format_zulu.git")
               (commit
                 "16273e825aae5064ffd72047cc1373c61add6fbe")))
        (sha256
          (base32
            "10vf46ynrgn634i494mvyjcirmlvhl6f6f9vjn4fk9m7h24lhk8s"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
			  (assoc-ref %outputs "out")
			  "/share/j/addons/format/zulu")))
                (copy-recursively "." out)
                #t))))))
    (home-page
      "https://github.com/jsoftware/format_zulu")
    (synopsis "Strings conversion package\n")
    (description
      "Addon: zulu - is a set of stringlist conversion verbs\nplus a sandbox of sample data.\nConverts consistently between 4 supported forms of stringlist,\n  boxed        eg: 'alpha';'bravo';'charlie'\n  LF-separated eg: 'alpha',LF,'bravo',LF,'charlie'\n  open         eg: 'alpha bravo charlie'\n  matrix       eg: 3 7$'alpha  bravo  charlie'\n\n")
    (license expat)))

(define-public j-format-printf
  (package
    (name "j-format-printf")
    (version "1.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/format_printf.git")
             (commit "5fca8b8961100f3eecfa3ff1aa7eae43f433fae1")))
       (sha256
        (base32
         "07yz6cr6v7lrfpv9gk80phcy6mqdibzghy4bz5scd2167kxqvi13"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/format/printf")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/format_printf")
    (synopsis "C-style printf formatting")
    (description
     "Printf provides verbs and adverbs for formatted printing in the manner of C's printf and sprintf.\n\n")
    (license expat)))

(define-public j-general-dirutils
  (package
    (name "j-general-dirutils")
    (version "1.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/general_dirutils.git")
             (commit
              "8221d58878d2926dde42a7a1330ae5e994410c53")))
       (sha256
        (base32
         "12jl7mi6wm5s3116gkv8j1x04dzxfhws7824jhpcdv97siafymdy"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/general/dirutils")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/general_dirutils")
    (synopsis "Additional directory utilities")
    (description
     "Directory utilities in addition to those in dir.ijs.\nIncluding test for existence of directory, create all non-existing\ndirectories in a path.\nContributed by Ric Sherlock\n\n")
    (license expat)))

(define-public j-general-unittest
  (package
    (name "j-general-unittest")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/general_unittest.git")
         (commit "c0916768bba3832fbb9bf2305d34575a5adc0ad0")))
       (sha256
        (base32 "0j78h07jm1b1q79vz091kv54cqx7g1jralj1dy5vyvswj3gkqrwj"))))
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
                                       "/share/j/addons/general/unittest")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/general_unittest")
    (synopsis "Unit Test Framework")
    (description "Implements a unit test framework.")
    (license expat)))

(define-public j-general-misc
  (package
    (name "j-general-misc")
    (version "2.5.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/general_misc.git")
         (commit "bbfc957fc4ddf90231c4a06239cbc85c67cc2769")))
       (sha256
        (base32 "1ky88dq8skdik0lbrr9892bmbdqbj8fn6w83gxg0jc1lkqsmqb6d"))))
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
                                       "/share/j/addons/general/misc")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/general_misc")
    (synopsis "misc general scripts")
    (description "trace.ijs Execution trace utilities
fndisplay.ijs Display hooks, forks and other syntax elements
font.ijs
format.ijs
parts.ijs Partition functions
validate.ijs Data validation functions
jdll.ijs
guid.ijs Create guids in various formats
pack.ijs Package utilities
ieee64.ijs
test_clippaste.ijs Test script for clippaste.ijs
inverted.ijs Current URL http://code.jsoftware.com/wiki/Essays/Inverted%20Table
prompt.ijs
bigfiles.ijs
numeric.ijs Various numeric utilities
evolute.ijs
clippaste.ijs
test_bigfiles.ijs 	data=.bixread f;start0,start1[,len[,dirflag]]			indexed read
fndef.ijs Using a more literate style")
    (license expat)))

(define-public j-debug-jig
  (package
    (name "j-debug-jig")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/debug_jig.git")
             (commit
              "c2413bbe58f1746573c37f1799f6fdc756328132")))
       (sha256
        (base32
         "1hyamk0m10yn76240fqn9c90z50mbyzl80jlv7ig2d9v2diprq5n"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/debug/jig")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/debug_jig")
    (synopsis "Augmented Display of J results")
    (description
     "Jig displays an interactive SVG window that allows users to easily see the shape, type and other properties of the results of J sentences. Jig runs in the jqt environment and requires the full version of the QT ide\n\n")
    (license expat)))

;;;; Web Addons
(define-public j-sockets-socklib
  (package
    (name "j-sockets-socklib")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/sockets_socklib.git")
             (commit
              "53dd3d9459ec6d790d33b39da9b24e915e9a837a")))
       (sha256
        (base32
         "10ah2wyf42vm2amm4w42i1pzxyvflfrbnmgcgxmhark14kxa19m5"))))
    (propagated-inputs
     `(("j-misc-miscutils" ,j-misc-miscutils)
       ("j-format-printf" ,j-format-printf)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/sockets/socklib")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/sockets_socklib")
    (synopsis
     "Routines for multiple asynchronous sockets")
    (description
     "sockmux creates a socket in its own locale and manages multiple such sockets.\nsockconnxactn calls sockmux and mediates transfer of data to an application, using callbacks to tell the application when data has been received.  Suitable for things like email or HTTP transactions.\nThere is also a file-server. \n\n")
    (license expat)))

(define-public j-sockets-sockutils
  (package
    (name "j-sockets-sockutils")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/sockets_sockutils.git")
             (commit
              "3cd63f5de3124eeafe47de379a12a16cf89631eb")))
       (sha256
        (base32
         "1mwrc92dn4zmdqzkb30agd1y1zcam9sdg2bbk44zp9p3g9c88yv8"))))
    (propagated-inputs
     `(("j-misc-miscutils" ,j-misc-miscutils)
       ("j-sockets-socklib" ,j-sockets-socklib)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/sockets/sockutils")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/sockets_sockutils")
    (synopsis "Routines for web transactions")
    (description "Routines to process transactions using the socket system\n")
    (license expat)))

(define-public j-web-gethttp
  (package
    (name "j-web-gethttp")
    (version "1.0.21")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/web_gethttp.git")
         (commit "d4881d9f4df9d6f2fda0929cba98fdcdeb228f7a")))
       (sha256
        (base32 "0k5zrydb2hjzygpywdq4z4j8mqfdw4l43b7qmcqxrzk2p8fff947"))))
    (propagated-inputs `(("curl" ,curl))) ;; calls at run time
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
                                       "/share/j/addons/web/gethttp")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/web_gethttp")
    (synopsis "Retrieve files from web")
    (description "J interface to Wget/cURL for retrieving files using http, https or ftp protocols.")
    (license expat)))

;;;; IDE addons
(define-public j-ide-qt
  (package
    (name "j-ide-qt")
    (version "1.1.139")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/ide_qt.git")
         (commit "e306a3898e89a3215c94075d70d360a7deb477d2")))
       (sha256
        (base32 "0vnx2dhi9wjimnk8s3l17fz6ywxx7rkcd905a0swhgbr32w5ajxs"))))
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
                                       "/share/j/addons/ide/qt")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/ide_qt/")
    (synopsis "Qt IDE")
    (description "Qt development")
    (license expat)))

(define-public j-debug-lint
  (package
    (name "j-debug-lint")
    (version "1.18.16")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/debug_lint.git")
         (commit "e312d707b31a8c7c92e63c62f9e0b13b553a0d68")))
       (sha256
        (base32 "1askbwxsrq2jv7bvb0087jdlcgc6kss2c7lz38z36gx3kvcz7lc8"))))
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
                                       "/share/j/addons/debug/lint")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/debug_lint/")
    (synopsis "Load a script and check its syntax")
    (description "lint tries to find errors before a script is run.  The idea is for 'lint' to replace 'load'
during debugging.  The errors it looks for are the following:
 explicit definitions lacking trailing )
 undefined names, including names not defined in all paths
 verbs used with invalid valences
 non-noun results at the end of condition blocks and verbs
 syntax errors
 sentences with no effect on execution (eg verb verb)
See the program header for description and directives.")
    (license expat)))

(define-public j-debug-dissect
  (package
    (name "j-debug-dissect")
    (version "4.6.39")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/debug_dissect.git")
             (commit
              "113260c9e3bbedf8ed3707af38e2343247278b61")))
       (sha256
        (base32
         "1w659x1jxwskaikqm082012s2wisn0lzxygazpbjgcsqj35zvslj"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/debug/dissect")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/debug_dissect")
    (synopsis
     "Run a sentence and produce a 2D display of results")
    (description
     "dissect runs a sentence after inserting instrumentation at the execution of each primitive.\nThen it creates a 2D display showing each word and the results of executing it.\nThe user can click on the display to probe execution.\n\n")
    (license expat)))

;;;; Demos/Labs
(define-public j-demos-qtdemo
  (package
    (name "j-demos-qtdemo")
    (version "1.0.21")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/demos_qtdemo.git")
         (commit "a888bce12028d2a91fa1c190662c9c61a157ab97")))
       (sha256
        (base32 "1y870wf21zyv9ib56pl4clhlb8kfn01nsadlldygzxpn8ap3i03n"))))
    (propagated-inputs `(("j-api-gles" ,j-api-gles)
			 ("j-graphics-bmp" ,j-graphics-bmp)
			 ("j-graphics-plot" ,j-graphics-plot)
			 ("j-graphics-viewmat" ,j-graphics-viewmat)
			 ("j-ide-qt" ,j-ide-qt)))
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
                                       "/share/j/addons/demos/qtdemo")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/demos_qtdemo/")
    (synopsis "qt demo")
    (description "simple demos for qt")
    (license expat)))

(define-public j-demos-isigraph
  (package
    (name "j-demos-isigraph")
    (version "1.0.71")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/demos_isigraph.git")
         (commit "301d390b932f8751613d149333cdc91e409e4461")))
       (sha256
        (base32 "0vhkz89f09fkdrxi5ilb59n3dqvjm2w1xvpjm1fwrfpyilwdgzhi"))))
    (propagated-inputs `(("j-graphics-viewmat" ,j-graphics-viewmat)))
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
                                       "/share/j/addons/demos/isigraph")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/demos_isigraph/")
    (synopsis "qt demo")
    (description "simple demos for qt")
    (license expat)))

(define-public j-demos-wdplot
  (package
    (name "j-demos-wdplot")
    (version "1.0.50")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/demos_wdplot.git")
             (commit
              "4ae390bb83bb66868929b5bbdaa4e2285ac245ba")))
       (sha256
        (base32 "0s9gk6kpyf7rv9sj1q0hsyh8br8nziif0lva4n36al2cqib751zq"))))
    (propagated-inputs
     `(("j-graphics-plot" ,j-graphics-plot)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/demos/wdplot")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/demos_wdplot")
    (synopsis "Plot demos wd emulation")
    (description "Plot demos using wd emulation\n")
    (license expat)))

;; many depends:
;; (define-public j-demos-wd
;;   (package
;;     (name "j-demos-wd")
;;     (version "1.0.71")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri
;;         (git-reference
;;          (url "https://github.com/jsoftware/demos_wd.git")
;;          (commit "301d390b932f8751613d149333cdc91e409e4461")))
;;        (sha256
;;         (base32 "0vhkz89f09fkdrxi5ilb59n3dqvjm2w1xvpjm1fwrfpyilwdgzhi"))))
;;     (propagated-inputs `(("j-graphics-viewmat" ,j-graphics-viewmat)))
;;     (outputs '("out"))
;;     (build-system gnu-build-system)
;;     (arguments
;;      `(#:modules ((guix build gnu-build-system) (guix build utils))
;;        #:phases
;;        (modify-phases %standard-phases
;;          (delete 'configure) (delete 'check) (delete 'build)
;;          (replace 'install
;;            (lambda _
;;              (let ((out (string-append (assoc-ref %outputs "out")
;;                                        "/share/j/addons/demos/isigraph")))
;;                (copy-recursively "." out)
;;                #t))))))
;;     (home-page "https://github.com/jsoftware/demos_wd/")
;;     (synopsis "qt demo")
;;     (description "simple demos for qt")
;;     (license expat)))

;;;; Games
(define-public j-games-minesweeper
  (package
    (name "j-games-minesweeper")
    (version "1.0.52")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/games_minesweeper.git")
         (commit "6578a63b9ee47d6c00dc5aacb0c61ff22de9c1d3")))
       (sha256
        (base32 "15jbwpsxr1hyqyzj9r8gh2ib14gwawqcahwn40cg68k6avaf0bj9"))))
    (propagated-inputs `(("j-graphics-viewmat" ,j-graphics-viewmat)))
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
                                       "/share/j/addons/games/minesweeper")))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/games_minesweeper")
    (synopsis "Classic Minesweeper game")
    (description "Implentation of classic Minesweeper game.
Designed as an example of how to implement equivalent user interfaces for different environments.
User interfaces available for various J environments currently include:
  * jconsole, jQt
Authors: Ric Sherlock, Bill Lam and Raul Miller.")
    (license expat)))

(define-public j-games-2048
  (package
    (name "j-games-2048")
    (version "1.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/games_2048.git")
             (commit
              "810f50208fd2f42ba81ca504f458d6cbf2c9c641")))
       (sha256
        (base32
         "1hlbwg8ki0wpsajzw9jzd087xv8fbg34szwh9dcv6hlhz8mk2cnh"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/games/2048")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/games_2048")
    (synopsis "2048 game")
    (description
     "Implentation of the 2048 game.\n\nAn example of how to implement equivalent user interfaces for different environments.\nUser interfaces available for various J environments currently include:\n  * jconsole, jQt\n\nAuthors: Ric Sherlock\n\n")
    (license expat)))

(define-public j-games-bagofnouns
  (package
    (name "j-games-bagofnouns")
    (version "1.0.92")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/games_bagofnouns.git")
             (commit
              "bc7f8a700f7fe8cf010f7f70db269998f739c48a")))
       (sha256
        (base32
         "0s24c5dzyhh8dnjqbb70g389841h20wgpj22k77r7ki121xy9jxm"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/games/bagofnouns")))
               (copy-recursively "." out)
               #t))))))
    (home-page  "https://github.com/jsoftware/games_bagofnouns")
    (synopsis "Distributed game controller")
    (description
     "Lets people play a game over Zoom\n\n")
    (license expat)))

(define-public j-games-pousse
  (package
    (name "j-games-pousse")
    (version "1.0.37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/games_pousse.git")
             (commit
              "3a8810111a0a029a2401a39b40774b7253abb2bb")))
       (sha256
        (base32
         "0g53k8hjmd6rgsdd858kwvqa7i0gmrxkardnn7nw2j73mnyf90kr"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/games/pousse")))
               (copy-recursively "." out)
               #t))))))
    (home-page
     "https://github.com/jsoftware/games_pousse")
    (synopsis "pousse")
    (description "pousse\n")
    (license expat)))

(define-public j-games-solitaire
  (package
    (name "j-games-solitaire")
    (version "1.0.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/games_solitaire.git")
             (commit
              "7ef609f69c891779af8c7ac3ac36d70e3bf033dc")))
       (sha256
        (base32
         "1c4xm6325v5ii9hmggh5xx5rj8vr325976crbjx3mannin3i6q3y"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace
             'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/games/solitaire")))
               (copy-recursively "." out)
               #t))))))
    (home-page  "https://github.com/jsoftware/games_solitaire")
    (synopsis "solitaire")
    (description "solitaire\n")
    (license expat)))

;;;; Labs
(define-public j-labs-labs
  (package
    (name "j-labs-labs")
    (version "1.0.198")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/labs_labs.git")
             (commit "cfa4b1df62ed137df2dc2ab2661f260484880ea3")))
       (sha256 (base32 "1cgfd5j8j1r1yfy71dcjcldh8yb3l9s120ln5c253liszwii0a6w"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
	(guix build utils))
       #:phases
       (modify-phases %standard-phases
	 (delete 'configure) (delete 'check) (delete 'build)
	 (replace 'install
	   (lambda _
	     (let ((out (string-append (assoc-ref %outputs "out")
				       "/share/j/addons/labs/labs")))
	       (copy-recursively "." out)
	       #t))))))
    (home-page  "https://github.com/jsoftware/labs_labs")
    (synopsis "LABS")
    (description "LABS")
    (license expat)))

(define-public j-docs-help
  (package
    (name "j-docs-help")
    (version "1.0.75")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jsoftware/docs_help.git")
             (commit "a33ca7b508877a958371aa8a2b9f6447d4569944")))
       (sha256
        (base32
         "0v98j3h9w5ggfh5xz70yin764afrgfqjkm7i3y49ybv557c7kq07"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        (guix build utils))
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append
                         (assoc-ref %outputs "out")
                         "/share/j/addons/docs/help"))
		   (man (string-append
			 (assoc-ref %outputs "out")
			 "/share/man")))
	       (mkdir-p (string-append man "/man1"))
	       (mkdir-p (string-append man "/man5"))
	       (install-file "man/jconsole.1" (string-append man "/man1"))
	       (install-file "man/editrc.5" (string-append man "/man5"))
               (copy-recursively "." out)
               #t))))))
    (home-page "https://github.com/jsoftware/docs_help")
    (synopsis "J Help Files")
    (description "J Help Files\n\n")
    (license expat)))

(define-public j-dev-fold
  (package
    (name "j-dev-fold")
    (version "1.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jsoftware/dev_fold.git")
               (commit "97bda88fbc383e3c097d9928d7e3feea005bdd88")))
        (sha256
	 (base32
	  "01fx8ypals0r29w5xkc31zx20rhm8v3ixmjl9klhm70b1w2bnjjj"))))
    (propagated-inputs '())
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace 'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/dev/fold")))
                (copy-recursively "." out)
                #t))))))
    (home-page "https://github.com/jsoftware/dev_fold")
    (synopsis "Fold implementation in J")
    (description "J emulations of the Fold primitives\n\n")
    (license expat)))

(define-public j-stats-bonsai
  (package
    (name "j-stats-bonsai")
    (version "1.0.0")
    (source
      (origin
       (method git-fetch)
       
        (uri (git-reference
	      (url "https://github.com/jitwit/bonsai.git")
	      (commit
	       "d84d1a21438b6e5ad64b325fb1ee23ca3b1caba4")))
        (sha256
          (base32
	   "1gd6b2cx50z8m9nfzj2dyphryj68ibiqivmv5gq6wv1p4asbhap0"))))
    (propagated-inputs
     `(("j-stats-base" ,j-stats-base)
       ("j-stats-distribs" ,j-stats-distribs)
       ("j-graphics-plot" ,j-graphics-plot)))
    (outputs '("out"))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((guix build gnu-build-system)
         (guix build utils))
        #:phases
        (modify-phases
          %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (replace
            'install
            (lambda _
              (let ((out (string-append
                           (assoc-ref %outputs "out")
                           "/share/j/addons/stats/bonsai")))
                (copy-recursively "." out)
                #t))))))
    (home-page "https://github.com/jitwit/bonsai")
    (synopsis
      "statistical benchmarks with confidence\n")
    (description
     "bootstrapping benchmarks in J for confidence\n\ninspired by https://hackage.haskell.org/package/criterion\n\ninformed by https://projecteuclid.org/download/pdf_1/euclid.ss/1177013815 and https://web.stanford.edu/~hastie/CASI/\n\n")
    (license asl2.0)))
