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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) :select (gpl3 lgpl2.1))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt))

(define (profile.ijs install version)
  (string-append "NB. J profile
jpathsep_z_=: '/'&(('\\' I.@:= ])})
bin =. BINPATH_z_ =. '/.guix-profile/bin',~home=. 2!:5'HOME'
install=." install "
'addons system tools'=. install&, &.> '/addons';'/system';'/tools'
user=. home,userx=. '/j" version "-user'
'break config snap temp'=. user&, &.> '/break';'/config';'/snap';'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'
SystemFolders_j_=: ids,.jpathsep@\".&.>ids
md=. 3 : 0
if. -.#1!:0 }:a=.y,'/' do. for_n. I. a='/' do. 1!:5 :: [ <n{.a end. end.
)
md &.> (user,'/projects');break;config;snap;temp
0!:0 <jpathsep (4!:55 (;:'userx ids md'), ids)]system,'/util/boot.ijs'
"))

(define-public j
  (package
    (name "j")
    (version "903")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/jsource")
         (commit "1a339d22f4d97c2058defa21ff2c9d630d5b3adf")))
       (sha256
        (base32 "0hwi403967mn88qgxqf22syrqfgli1zsicchy03gzzyqf4n32jav"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("clang" ,clang)
       ("readline" ,readline)
       ("bc" ,bc)
       ("libedit" ,libedit)
       ("pcre2" ,pcre2)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jplatform ,(if (target-arm?) "raspberry" "linux"))
                   (j64x ,(if (target-64bit?) "j64avx2" "j32"))
                   (out (assoc-ref %outputs "out")))
               (setenv "jplatform" jplatform)
               (setenv "CC" "clang")
               (setenv "j64x" j64x)
               (with-output-to-file "jsrc/jversion.h"
                 (lambda ()
                   (display "#define jversion  ") (write ,version)  (newline)
                   (display "#define jplatform ") (write jplatform) (newline)
                   (display "#define jtype     ") (write "beta")    (newline)
                   (display "#define jlicense  ") (write "GPL3")    (newline)
                   (display "#define jbuilder  ") (write "guix.gnu.org")))
               (invoke "cat" "jsrc/jversion.h")
               (substitute* `("jlibrary/system/main/regex.ijs")
                 (("pcre2dll=: f")
                  (string-append "pcre2dll=: '"
                                 (assoc-ref %build-inputs "pcre2")
                                 "/lib/libpcre2-8.so.0'")))
               (substitute* `("jlibrary/system/util/tar.ijs")
                 (("libz=: .+$")
                  (string-append "zlib=: '"
                                 (assoc-ref %build-inputs "zlib")
                                 "/lib/libz.so'\n")))
               #t)))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (chdir "make2")
             (invoke "./build_all.sh")
             (chdir "..")
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((tsu (string-append (getcwd) "/test/tsu.ijs"))
                   (jbld
                    (canonicalize-path
                     (string-append "bin/"
                                    (getenv "jplatform")
                                    "/"
                                    (getenv "j64x")))))
               ; following instructions from make2/make.txt with temp profile.ijs
               (with-output-to-file "profile.ijs"
                 (lambda ()
                   (display ,(profile.ijs "'jlibrary'" version))))
	       (invoke (string-append jbld "/jconsole")
                       "-lib" (string-append jbld "/libj.so")
                       "-jprofile" "profile.ijs"
                       "./test/tsu.ijs"
                       "-js" "exit 0 [ RECHO ddall")
               #t)))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                    (share (string-append (assoc-ref %outputs "out")
                                          "/share/j"))
                    (jbld
                     (string-append "bin/"
                                    (getenv "jplatform")
                                    "/"
                                    (getenv "j64x")))
                    (jconsole (string-append jbld "/jconsole"))
                    (libj.so  (string-append jbld "/libj.so")))
               (install-file jconsole bin)
               (install-file libj.so bin)
               (copy-recursively "jlibrary/addons"
                                 (string-append share "/addons"))
               (copy-recursively "jlibrary/system"
                                 (string-append share "/system"))
               (with-output-to-file (string-append bin "/profile.ijs")
                 (lambda ()
                   (display
                    ,(profile.ijs "home,'/.guix-profile/share/j'" version))))
               #t))))))
    (synopsis "Dialect of the APL programming language")
    (description "J is a programming language that works with arrays, verbs,
adverbs, and conjunctions.  For example, @code{+/x} sums array @code{x} and
@code{/:~x} sorts it.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license gpl3)))

(define-public jqt
  (package
    (name "jqt")
    (version "1.9.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jsoftware/qtide.git")
         (commit "3e724bd70afd02462613e326a6e2d43cda69f72f")))
       (sha256
        (base32 "028q56wqk71gsvyvxpsn1cw87wxiwk7dri2kifnj0xpl01ll6z22"))))
    (build-system gnu-build-system)
    (inputs `(("bash" ,bash)
              ("j" ,j)
              ("mesa" ,mesa)
              ("pulseaudio" ,pulseaudio)
              ("qtbase" ,qtbase)
              ("qtwebsockets" ,qtwebsockets)
              ("qtsvg" ,qtsvg)
              ("qtdeclarative" ,qtdeclarative)
              ("qtquickcontrols" ,qtquickcontrols)
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
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (with-directory-excursion "lib"  (invoke "qmake") (invoke "make"))
             (with-directory-excursion "main" (invoke "qmake") (invoke "make"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
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
                         "qtquickcontrols"
                         "qtmultimedia"
                         "qtwebchannel")))
               `("QTWEBENGINEPROCESS_PATH" =
                 (,(string-append  (assoc-ref inputs "qtwebengine")
                                   "/lib/qt5/libexec/QtWebEngineProcess"))))
             (substitute* `(,(string-append (assoc-ref outputs "out") "/bin/jqt"))
               (("\\$@")
                (string-append "-lib\" \""
                               (assoc-ref inputs "j") "/bin/libj.so\" "
                               "\"-jprofile\" \""
                               (assoc-ref inputs "j") "/bin/profile.ijs\" "
                               "\"$@")))
             #t)))))
    (synopsis "The jqtide application is an executable jqt, and a
shared object libjqt.so")
    (description "The jqtide application is an executable jqt, and a
shared object libjqt.so.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license lgpl2.1)))
