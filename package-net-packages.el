;;; package-net-packages.el --- Installation and Maintenance of XEmacs packages

;; Copyright (C) 2000 Andy Piper.

;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02111-1301, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Manipulate packages for the netinstall setup utility

;; The process should be so:

;; 1. The package maintainer or release manager makes a release
;; announcement.
;;
;; 2. For a new package releases the netinstall maintainer simply
;; needs to update `ftp://ftp.xemacs.org/pub/xemacs/setup.ini'. This is
;; harder than it sounds because the file also includes information
;; about the binary releases. At the moment going to the netinstall
;; directory and typing:
;;
;;   `make XEMACS=<current executable location> setup.ini' 
;;
;; will do the right thing provided that:
;; 
;; (a) `package-net-cygwin32-binary-size' and
;; `package-net-win32-binary-size' are set correctly.
;;
;; (b) The binary pointed to by `XEMACS' has a current
;; `package-index.LATEST.pgp' file. If you don't specify the XEMACS=
;; part then you will get whatever is current for your build tree -
;; which is probably not what you want.
;;
;; You can run `package-net-convert-index-to-ini' manually and specify
;; REMOTE but I generally found that to be inconvenient and error-prone.
;;
;; 3. For package releases that's all you need to do. For binary
;; releases you need to build both cygwin and win32 binaries and put
;; them in appropriate tarballs:
;;
;; For cygwin, configure, make and install and then do (this is for
;; 21.1.13):
;;
;;   cd <install dir>
;;   tar cvzf xemacs-i686-pc-cygwin32-21.1.13.tar.gz \
;;      ./bin/i686-pc-cygwin32 ./lib/xemacs-21.1.13 \
;;      ./lib/xemacs/lock ./man/man1/xemacs.1 \
;;      ./man/man1/ctags.1 ./man/man1/gnu*.1'
;;
;;  Note that the naming of the package is important. Don't be tempted
;;  to change the order in any way.
;;
;; For win32 build and install the release and then (again for
;; 21.1.13):
;;
;;   cd <install dir>
;;   tar cvzf xemacs-i386-pc-win32-21.1.13.tar.gz ./XEmacs-21.1.13
;; 
;; The binaries should be uploaded to
;; `ftp://ftp.xemacs.org/pub/xemacs/binaries/cygwin32' and
;; `ftp://ftp.xemacs.org/pub/xemacs/binaries/win32' respectively. Take
;; a note of their sizes and set `package-net-cygwin32-binary-size'
;; and `package-net-win32-binary-size' appropriately in this file and
;; then follow step 2.

(require 'package-admin)
(require 'package-get)

;; What path should we use from the myriad available?
;; For netinstall we just want something simple, and anyway this is only to 
;; bootstrap the process. This will be:
;; <root>/setup/ for native windows
;; <root>/lib/xemacs/setup for cygwin.
;;
;;; To Do:
;;
;; 1. Package update functions should also update the installed
;; database so that running setup.exe again does not reinstall
;; packages.
;;
;; 2. Generating setup.ini should be more automatic.

(defun package-net-packages-convert-index-to-ini (&optional destdir remote category)
  "Convert the package index to ini file format in DESTDIR.
DESTDIR defaults to the value of `data-directory'."
;  (package-get-require-base remote)

  (setq destdir (file-name-as-directory (or destdir data-directory)))
  (let* ((fname "setup-packages.ini")
	 (buf (get-buffer-create (format "*%s*" fname))))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (erase-buffer buf)
          (goto-char (point-min))
          (let ((entries package-get-base) entry plist)
	    (insert "# This file is automatically generated.  If you edit it, your\n")
	    (insert "# edits will be discarded next time the file is generated.\n")
	    (insert "#\n\n")
	    (insert (format "# package index version %s.\n" "unknown"))
	    ;; Standard packages
	    (while entries
	      (setq entry (car entries))
	      (setq plist (car (cdr entry)))
	      ;; ignore mule packages
	      (unless (or (memq 'mule-base (plist-get plist 'requires))
			  (eq 'mule-base (car entry)))
		(insert (format "@ %s\n" (symbol-name (car entry))))
		(insert (format "version: %s\n" (plist-get plist 'version)))
		(insert (format "install: packages/%s %s\n" (plist-get plist 'filename)
				(plist-get plist 'size)))
	      ;; These are not supported as yet
	      ;;
	      ;; (insert (format "source: %s\n" (plist-get plist 'source)))
	      ;; (insert "[prev]\n")
	      ;; (insert (format "version: %s\n" (plist-get plist 'version)))
	      ;; (insert (format "install: %s\n" (plist-get plist 'filename)))
	      ;; (insert (format "source: %s\n" (plist-get plist 'source)))
		(insert "\n"))
	      (setq entries (cdr entries))))
	  (insert (format "# %s file ends here\n" fname))
	  (write-region (point-min) (point-max) (concat destdir fname)))
      (kill-buffer buf))))

(defun package-net-packages-batch-convert-index-to-ini ()
  "Convert the package index to ini file format."
  (unless noninteractive
    (error "`package-net-packages-batch-convert-index-to-ini' is to be used only with -batch"))
  (let ((dir (pop command-line-args-left))
	(category (pop command-line-args-left))
	(package-get-require-signed-base-updates nil))
    (package-net-packages-convert-index-to-ini dir nil category)))

;; package-net-packages.el ends here
