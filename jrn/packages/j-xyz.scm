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
  #:use-module ((guix licenses) :select (expat gpl2+))
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (jrn packages j)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages curl))

;;;; Graphics Addons
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
    (native-inputs `(("j" ,j)))
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
    (native-inputs `(("j" ,j)))
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
    (inputs `(("j" ,j)
              ("glibc" ,glibc)
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
    (inputs `(("j" ,j)
              ("j-arc-zlib" ,j-arc-zlib)))
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
    (inputs `(("j" ,j)
              ("cairo" ,cairo)))
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
    (inputs `(("j" ,j)))
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
    (inputs `(("j" ,j) ;; todo: general-misc & graphics/gl2
              ("j-graphics-afm" ,j-graphics-afm)
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
    (inputs `(("j" ,j) ;; todo: graphics/gl2
              ("j-graphics-bmp" ,j-graphics-bmp)
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
    (inputs `(("j" ,j)
              ("mesa" ,mesa)))
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
         (commit "289c02d2a8781a4c810ccf783e6447508e5f5ddd")))
       (sha256
        (base32 "0rpdazacplyg8ipra19h1zksclkkgqc33q6pc45fga6a8zh3d3a4"))))
    (native-inputs `(("j" ,j)))
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
         (url "https://github.com/jitwit/arc_zlib.git")
         (commit "08d0738fedf1e0592347748a8b7466ea95466062")))
       (sha256
        (base32 "0wwl4sf1pxwp9p3cg7pzqsnfsvvybgpsm85p1zlcfgz9c3gs1glz"))))
    (inputs `(("zlib" ,zlib)))
    (native-inputs `(("j" ,j)))
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
         (delete 'check) (delete 'build) (replace 'install
                                           (lambda _
                                             (let ((out (string-append (assoc-ref %outputs "out")
                                                                       "/share/j/addons/arc/zlib")))
                                               (copy-recursively "." out)
                                               #t))))))
    (home-page "https://github.com/jsoftware/arc_zlib")
    (synopsis "Interface with zlib")
    (description "This J addon provides an interface to zlib.")
    (license expat)))

;;;; Data Addons
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
         (commit "af56a2ec846ef3a31720a6459de568d32bebd5fc")))
       (sha256
        (base32 "0q4npxs0x5agywq6x5rmjhnhzlyvvh1cmd71rqxvg8696jgm7qmv"))))
    (native-inputs `(("j" ,j)))
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
    (native-inputs `(("j" ,j)))
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
    (native-inputs `(("j" ,j)))
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
    (native-inputs `(("j" ,j)))
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
    (native-inputs `(("j" ,j)))
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

;;;; Web Addons
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
