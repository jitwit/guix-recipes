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
	 (commit "c40a0833f5be9a40676ae70493ed9e2d33dd687e")))
       (sha256
	(base32 "0k21lzx7kjdczvm7xvp01l7l81alcs8zyz6hwkriispzw9lgrqi9"))))
    (build-system gnu-build-system)
    (inputs
     `(("bash" ,bash)
       ("readline" ,readline)
       ("bc" ,bc)
       ("libedit" ,libedit)
       ("pcre2" ,pcre2)
       ("zlib" ,zlib)))
    (outputs '("out"))
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
	(guix build utils))
       #:phases
       (modify-phases %standard-phases
	 (replace 'configure
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((jplatform ,(if (target-arm?) "raspberry" "linux"))
		    (out     (assoc-ref %outputs "out")))
	       (with-output-to-file "jsrc/jversion.h"
		 (lambda ()
		   (display "#define jversion  ") (write ,version)  (newline)
		   (display "#define jplatform ") (write jplatform) (newline)
		   (display "#define jtype     ") (write "beta")    (newline)
		   (display "#define jlicense  ") (write "GPL3")    (newline)
		   (display "#define jbuilder  ") (write "guix.gnu.org")
		   (newline)))
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
	     (let ((jplatform ,(if (target-arm?) "raspberry" "linux"))
		   (j64x ,(if (target-64bit?) "j64avx2" "j32")))
	       (chdir "make2")
	       (system (format #f "jplatform=~a j64x=~a USE_SLEEF=1 ./build_jconsole.sh"
			       jplatform j64x))
	       (system (format #f "jplatform=~a j64x=~a USE_SLEEF=1 ./build_tsdll.sh"
			       jplatform j64x))
	       (system (format #f "jplatform=~a j64x=~a USE_SLEEF=1 ./build_libj.sh"
			       jplatform j64x))
	       (chdir "..")
	       #t)))
	 (replace 'check
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((jplatform ,(if (target-arm?) "raspberry" "linux"))
		    (j64x ,(if (target-64bit?) "j64avx2" "j32"))
		    (tsu (string-append (getcwd) "/test/tsu.ijs"))
		    (jbld (string-append "bin/" jplatform "/" j64x)))
	       ;; following directions in make2/make.txt
	       (copy-recursively jbld "jlibrary/bin")
	       (chdir "jlibrary/bin")
	       (system "echo \"RECHO ddall\" | jconsole ../../test/tsu.ijs")
	       (chdir "../..")
	       #t)))
	 (replace 'install
 	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (let* ((bin-out (string-append (assoc-ref %outputs "out") "/bin"))
		    (share-out (string-append (assoc-ref %outputs "out")
					      "/share/j"))
		    (jconsole "jlibrary/bin/jconsole")
		    (libj.so  "jlibrary/bin/libj.so"))
	       (install-file jconsole bin-out)
	       (install-file libj.so bin-out)
	       (copy-recursively "jlibrary/addons"
				 (string-append share-out "/addons"))
	       (copy-recursively "jlibrary/system"
				 (string-append share-out "/system"))
	       ;; rewrite of J's usual profile.ijs to play nice with
	       ;; guix. going through $HOME/.guix-profile/share/j
	       ;; doesn't feel totally right though...
	       (with-output-to-file (string-append bin-out "/profile.ijs")
		 (lambda ()
		   (display
		    "NB. J profile
NB. JFE sets BINPATH_z_ and ARGV_z_

jpathsep_z_=: '/'&(('\\' I.@:= ])})
home=. 2!:5'HOME'
BINPATH_z_=: home,'/.guix-profile/bin/jconsole'

bin=. BINPATH
install=. home,'/.guix-profile/share/j'
addons=. install,'/addons'
system=. install,'/system'
tools=. install,'/tools'
isroot=. 0
userx=. '/j902-user'
user=. home,userx
break=. user,'/break'
config=. user,'/config'
snap=. user,'/snap'
temp=. user,'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'

SystemFolders_j_=: ids,.jpathsep@\".&.>ids

NB. used to create mutable j user directories for temp
NB. files/configuring jqt/projects and so on
md=. 3 : 0 NB. recursive makedir
a=. jpathsep y,'/'
if. ('root'-:2!:5'USER') +. ('//'-:2{.a)+.('/root/'-:6{.a)+.('/var/root/'-:10{.a)+.('/usr/'-:5{.a)+.('/tmp'-:a) do. return. end. NB. installed under / /root /usr
if. -.#1!:0 }:a do.
  for_n. I. a='/' do. 1!:5 :: [ <n{.a end.
end.
)

NB. try to ensure user folders exist
md user,'/projects'
md break
md config
md snap
md temp

NB. boot up J and load startup.ijs if it exists
0!:0 <jpathsep (4!:55 (;:'isroot userx ids md'), ids)]system,'/util/boot.ijs'\n")))
	       #t))))))
    (synopsis "APL Dialect")
    (description "Terse, interpreted, array language originally developed by
Ken Iverson and Roger Hui.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license gpl3)))

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
	      ("j" ,j)
              ("mesa" ,mesa)
              ("pulseaudio" ,pulseaudio)
	      ;; fixme figure out which of these isn't needed
              ("qtbase" ,qtbase)
              ("qtwebsockets" ,qtwebsockets)
              ("qtsvg" ,qtsvg)
	      ;; ("qtwebkit" ,qtwebkit)
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
			 ;; "qtwebkit"
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
    (synopsis "The jqtide application is an executable, jqt, and a
shared object, libjqt.so")
    (description "The jqtide application is an executable, jqt, and a
shared object, libjqt.so.")
    (home-page "https://code.jsoftware.com/wiki/Main_Page")
    (license lgpl2.1)))
