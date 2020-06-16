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

(define-module (jrn packages j)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) :select (gpl3 lgpl2.1))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages qt))

;; fixme, maybe build "standard library" including profile/addons and
;; such separately. jconsole and jqt both can be told where libraries
;; and such are with cmdline args -lib and -jprofile so what's
;; currenlty here is actually good enough for getting the shared
;; object files built. this will also enable getting rid of that silly
;; JPATH env var addition...
(define-public j
  (package
    (name "j")
    (version "902")
    (source
     (origin
       (method git-fetch)
       (uri
	(git-reference
	 (url "https://github.com/jsoftware/jsource.git")
	 (commit "7faecfaf9c5e1f3bf4410aa61fee28142a6a44ce")))
       (sha256
	(base32 "0bpp5zm03z6hpdz6l2bxvk1dw5khvhh05lzv56583np8jw4ml2mg"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("readline" ,readline)
       ("bc" ,bc)
       ("libedit" ,libedit)
       ("pcre2" ,pcre2)
       ("zlib" ,zlib)))
    (outputs
     '("out"))
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
	(guix build utils)
	(ice-9 match))
       #:phases
       (modify-phases %standard-phases
	 (replace 'configure
	   (lambda _
	     ;; the environment variables J build scripts expect
	     (setenv "HOME"
		     (getenv "TEMP"))
	     (let*
		 ((jgit
		   (getcwd))
		  (jbld
		   (string-append
		    (getenv "HOME")
		    "/jbld"))
		  (jplatform ,(if
			       (target-arm?)
			       "raspberry" "linux"))
		  (jbits ,(if
			   (target-64bit?)
			   "j64" "j32"))
		  (jsuffix "so")
		  (CC "gcc")
		  (tsu
		   (string-append jgit "/test/tsu.ijs"))
		  (j32
		   (string-append jbld "/j32/bin/jconsole " tsu))
		  (j64
		   (string-append jbld "/j64/bin/jconsole " tsu))
		  (j64avx
		   (string-append jbld "/j64/bin/jconsole -lib libjavx."
                                  jsuffix " " tsu))
		  (j64avx2
		   (string-append jbld "/j64/bin/jconsole -lib libjavx2."
                                  jsuffix " " tsu))
		  (jmake
		   (string-append jgit "/make"))
		  (out
		   (assoc-ref %outputs "out"))
		  ;; this is likely not a good hack to help profile.ijs
		  ;; point flexibly to where it ought. used when running
		  ;; tests as well as upon installation to make installed
		  ;; addons visible. probably best to do full patches on
		  ;; profile or to just provide one.
		  (j-pre-install
		   (string-append jbld "/" jbits "/"))
		  (guix-profile-j-share "'share/j',~2!:5'JPATH'")
		  (usr-j-share
		   (string-append "'/usr/share/j/"
				  (substring ,version 0 1)
				  "."
				  (substring ,version 1)
				  "'")))
	       ;; make/make.txt asks us to copy make/jvars.sh to ~ and
	       ;; change appropriate vars. that file is used to set
	       ;; environment variables before calling J's build scripts. We
	       ;; set those explicitly here instead.
	       (setenv "jgit" jgit)
	       (setenv "jbld" jbld)
	       (setenv "CC" CC)
	       (setenv "jplatform" jplatform)
	       (setenv "jsuffix" jsuffix)
	       (setenv "tsu" tsu)
	       (setenv "j32" j32)
	       (setenv "j64" j64)
	       (setenv "j64avx" j64avx)
	       (setenv "j64avx2" j64avx2)
	       (setenv "jbits" jbits)
	       (setenv "jmake" jmake)
	       (setenv "JPATH" j-pre-install)
	       ;; make/make.txt asks us to copy jsrc/jversion-x.h to
	       ;; jsrc/jversion.h and change the appropriate
	       ;; variables. Instead of using substitute*, just print
	       ;; directly to target file.
	       (with-output-to-file "jsrc/jversion.h"
		 (lambda ()
		   (display "#define jversion  ") (write ,version)  (newline)
		   (display "#define jplatform ") (write jplatform) (newline)
		   (display "#define jtype     ") (write "beta")    (newline)
		   (display "#define jlicense  ") (write "GPL3")    (newline)
		   (display "#define jbuilder  ") (write "guix")    (newline)))
	       ;; be able to see added addons
	       (substitute* `(,(string-append jgit "/jlibrary/bin/profile.ijs"))
		 ((usr-j-share)
		  guix-profile-j-share))
	       ;; be able to use j's pcre2 regexes
	       (substitute* `(,(string-append jgit "/jlibrary/system/main/regex.ijs"))
		 (("pcre2dll=: f")
		  (string-append "pcre2dll=: '"
				 (assoc-ref %build-inputs "pcre2")
				 "/lib/libpcre2-8.so.0'")))
	       ;; be able to use tar in built in addons
	       (substitute* `(,(string-append jgit "/jlibrary/system/util/tar.ijs"))
		 (("libz=: .+$")
		  (string-append "zlib=: '"
				 (assoc-ref %build-inputs "zlib")
				 "/lib/libz.so'\n")))
	       ;; now, we copy over files which will be included with
	       ;; installation. todo: should delete extraneous txt and bat
	       ;; files.
	       (mkdir-p
		(string-append j-pre-install "/bin"))
	       (copy-recursively
		(string-append jgit "/jlibrary")
		(string-append j-pre-install "/share/j"))
	       (install-file
		(string-append jgit "/jlibrary/bin/profile.ijs")
		(string-append j-pre-install "/bin"))
	       #t)))
	 (replace 'build
	   (lambda _
	     (let ((jbits (getenv "jbits")))
	       (invoke "cat" "jsrc/jversion.h")
	       (invoke "make/build_jconsole.sh" jbits)
	       (invoke "make/build_tsdll.sh" jbits)
	       (invoke "make/build_libj.sh" jbits)
	       (when (string=? jbits "j64")
		 (invoke "make/build_libj.sh" "j64avx")
		 (invoke "make/build_libj.sh" "j64avx2")))
	     #t))
	 (replace 'check
	   (lambda _
	     (if (string=? (getenv "jbits") "j64")
		 (system "echo \"RECHO ddall\" | $j64avx2")
		 (system "echo \"RECHO ddall\" | $j32"))
	     #t))
	 (replace 'install
 	   (lambda _
	     (let ((bin-in  (string-append (getenv "JPATH") "/bin/"))
		   (bin-out (string-append (assoc-ref %outputs "out") "/bin/")))
	       (install-file (string-append bin-in "jconsole") bin-out)
	       (install-file (string-append bin-in "libj.so")  bin-out)
	       (when (target-64bit?)
		 (install-file (string-append bin-in "libjavx.so")   bin-out)
		 (install-file (string-append bin-in "libjavx2.so")  bin-out)))
	     #t)))))
    (synopsis "APL Dialect")
    (description "Terse, interpreted, array language originally developed by
Ken Iverson and Roger Hui.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license gpl3)))

;;;; WIP/doesn't work!
(define-public jqt
  (package
    (name "jqt")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/qtide.git")
         (commit "5638f9d91b32ed4d2083296236e41371b79821e0")))
       (sha256
        (base32 "0qanmh4863mn0aryl3k1yg38a78mc2cv26xxyhlr11xhc3a7ck73"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (inputs `(("bash" ,bash)
              ("mesa" ,mesa)
              ("pulseaudio" ,pulseaudio)
              ("qtbase" ,qtbase)
              ("qtwebsockets" ,qtwebsockets)
              ("qtsvg" ,qtsvg)
              ("qtdeclarative" ,qtdeclarative)
              ("qtwebchannel" ,qtwebchannel)
              ("qtmultimedia" ,qtmultimedia)
              ("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:modules ((guix build gnu-build-system) (guix build utils))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (with-directory-excursion "lib"  (invoke "qmake") (invoke "make"))
             (with-directory-excursion "main" (invoke "qmake") (invoke "make"))
             #t))
         (replace 'install
           (lambda _
             (copy-recursively "bin/linux-x86_64/release"
                               (string-append (assoc-ref %outputs "out")
                                              "/bin"))
	     #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out") "/bin/jqt")
               `("QT_PLUGIN_PATH" ":" prefix
                 ,(map (lambda (label)
                         (string-append (assoc-ref inputs label)
                                        "/lib/qt5/plugins"))
                       '("qtbase"
			 "qtdeclarative"
			 "qtwebsockets"
			 "qtsvg"
			 "qtwebengine"
			 "qtmultimedia"
			 "qtwebchannel"))))
             #t)))))
    (synopsis "QT-based ide for the J Programming Language")
    (description "tbd")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license lgpl2.1)))
