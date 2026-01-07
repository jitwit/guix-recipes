(define-module (jrn packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)     ; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages lesstif)   ; motif
  #:use-module (gnu packages linux)     ; alsa-lib, gpm
  #:use-module (gnu packages mail)      ; for mailutils
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages web)       ; for jansson
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (emacs->emacs-next))

(define (%emacs-modules build-system)
  (let ((which (build-system-name build-system)))
    `((guix build ,(symbol-append which '-build-system))
      (guix build utils)
      (srfi srfi-1)
      (ice-9 ftw))))

(define-public jrn-emacs
  (package/inherit emacs-no-x
    (name "jrn-emacs")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (build-system glib-or-gtk-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-no-x)
       ((#:modules _) (%emacs-modules build-system))
       ((#:configure-flags flags #~'())
        #~(cons* "--with-cairo" "--without-libgmp" #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Note: due to the changed #:modules, %standard-phases in #$phases
            ;; refers to glib-or-gtk:%standard-phases, so we don't need to add
            ;; them ourselves.
            (add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
              ;; Restore the dump file that Emacs installs somewhere in
              ;; libexec/ to its original state.
              (lambda* (#:key outputs target #:allow-other-keys)
                (let* ((libexec (string-append (assoc-ref outputs "out")
                                               "/libexec"))
                       ;; each of these ought to only match a single file,
                       ;; but even if not (find-files) sorts by string<,
                       ;; so the Nth element in one maps to the Nth element of
                       ;; the other
                       (pdmp (find-files libexec "\\.pdmp$"))
                       (pdmp-real (find-files libexec "\\.pdmp-real$")))
                  (for-each rename-file pdmp-real pdmp))))))))
    (inputs (modify-inputs (package-inputs emacs-no-x)
              (prepend
               cairo
               dbus
               gtk+
               giflib
               harfbuzz
               libjpeg-turbo
               libotf
               libpng
               (librsvg-for-system)
               libtiff
               libx11
               libxft
               libxpm
               libwebp
               pango
               poppler)))))
