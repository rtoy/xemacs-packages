;;; package-compile.el --- Bytecompiler wrapper for building packages from source

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@xemacs.org>
;; Keywords: internal, lisp

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This is meant to be called -batch and handles setting up the loadpath
;; and minimizing what autoloads are imported.

;; This package cannot be used with an XEmacs binary that was built with
;; backwards compatible lisp directories.

;;; Code:

(setq stack-trace-on-error t)

(when (interactive-p)
  (error "package-compile may only be used with -batch"))

;;; Step 1, set the load-path and load the core autoloads.

(let* ((roots (paths-find-emacs-roots invocation-directory
				      invocation-name))
       (lisp-directory (paths-find-lisp-directory roots))
       (depth (cond
	       ;; #### All hail Mr. Preprocessor!
	       ;; OK, OK, his code is under development; FIXME when it's done.
	       ((boundp 'paths-load-path-depth) ; XEmacs 21.1
		paths-load-path-depth)
	       ((boundp 'paths-core-load-path-depth) ; XEmacs > 21.2.41
		paths-core-load-path-depth)
	       (t (error "Somebody has been messing with paths-find-*!")))))
  (setq load-path (paths-find-recursive-load-path (list lisp-directory)
						  depth)))

(load (expand-file-name "auto-autoloads" (car load-path)))

;;; Step 2, collect the dependencies into load-path and load the autoloads.
(require 'bytecomp)

(defconst package-directory-map
  '(
    ;; xemacs-packages
    ("Sun" . "xemacs-packages")
    ("ada" . "xemacs-packages")
    ("apel" . "xemacs-packages")
    ("auctex" . "xemacs-packages")
    ("bbdb" . "xemacs-packages")
    ("build" . "xemacs-packages")
    ("c-support" . "xemacs-packages")
    ("calc" . "xemacs-packages")
    ("calendar" . "xemacs-packages")
    ("cc-mode" . "xemacs-packages")
    ("clearcase" . "xemacs-packages")
    ("cookie" . "xemacs-packages")
    ("crisp" . "xemacs-packages")
    ("debug" . "xemacs-packages")
    ("dictionary" . "xemacs-packages")
    ("dired" . "xemacs-packages")
    ("edebug" . "xemacs-packages")
    ("ediff" . "xemacs-packages")
    ("edit-utils" . "xemacs-packages")
    ("edt" . "xemacs-packages")
    ("efs" . "xemacs-packages")
    ("eieio" . "xemacs-packages")
    ("elib" . "xemacs-packages")
    ("emerge" . "xemacs-packages")
    ("eshell" . "xemacs-packages")
    ("eterm" . "xemacs-packages")
    ("eudc" . "xemacs-packages")
    ("footnote" . "xemacs-packages")
    ("forms" . "xemacs-packages")
    ("frame-icon" . "xemacs-packages")
    ("fsf-compat" . "xemacs-packages")
    ("games" . "xemacs-packages")
    ("gnats" . "xemacs-packages")
    ("gnus" . "xemacs-packages")
    ("haskell-mode" . "xemacs-packages")
    ("hm--html-menus" . "xemacs-packages")
    ("ibuffer" . "xemacs-packages")
    ("idlwave" . "xemacs-packages")
    ("igrep" . "xemacs-packages")
    ("ilisp" . "xemacs-packages")
    ("ispell" . "xemacs-packages")
    ("jde" . "xemacs-packages")
    ("liece" . "xemacs-packages")
    ("mail-lib" . "xemacs-packages")
    ("mailcrypt" . "xemacs-packages")
    ("mew" . "xemacs-packages")
    ("mh-e" . "xemacs-packages")
    ("mine" . "xemacs-packages")
    ("misc-games" . "xemacs-packages")
    ("mmm-mode" . "xemacs-packages")
    ("net-utils" . "xemacs-packages")
    ("os-utils" . "xemacs-packages")
    ("pc" . "xemacs-packages")
    ("pcl-cvs" . "xemacs-packages")
    ("pcomplete" . "xemacs-packages")
    ("prog-modes" . "xemacs-packages")
    ("ps-print" . "xemacs-packages")
    ("psgml" . "xemacs-packages")
    ("reftex" . "xemacs-packages")
    ("rmail" . "xemacs-packages")
    ("scheme" . "xemacs-packages")
    ("semantic" . "xemacs-packages")
    ("sgml" . "xemacs-packages")
    ("sh-script" . "xemacs-packages")
    ("sieve" . "xemacs-packages")
    ("slider" . "xemacs-packages")
    ("sounds-au" . "xemacs-packages")
    ("sounds-wav" . "xemacs-packages")
    ("speedbar" . "xemacs-packages")
    ("strokes" . "xemacs-packages")
    ("supercite" . "xemacs-packages")
    ("texinfo" . "xemacs-packages")
    ("text-modes" . "xemacs-packages")
    ("textools" . "xemacs-packages")
    ("time" . "xemacs-packages")
    ("tm" . "xemacs-packages")
    ("tooltalk" . "xemacs-packages")
    ("tpu" . "xemacs-packages")
    ("tramp" . "xemacs-packages")
    ("vc" . "xemacs-packages")
    ("vc-cc" . "xemacs-packages")
    ("vhdl" . "xemacs-packages")
    ("view-process" . "xemacs-packages")
    ("viper" . "xemacs-packages")
    ("vm" . "xemacs-packages")
    ("w3" . "xemacs-packages")
    ("xemacs-base" . "xemacs-packages")
    ("xemacs-devel" . "xemacs-packages")
    ("xslide" . "xemacs-packages")
    ("xslt-process" . "xemacs-packages")
    ("zenirc" . "xemacs-packages")
    ;; mule/*
    ("edict" . "mule-packages")
    ("egg-its" . "mule-packages")
    ("latin-unity" . "mule-packages")
    ("leim" . "mule-packages")
    ("locale" . "mule-packages")
    ("lookup" . "mule-packages")
    ("mule-base" . "mule-packages")
    ("mule-ucs" . "mule-packages")
    ("skk" . "mule-packages")))


(defun package-name-to-directory (package)
  "Map `package' to a source directory."
  (let ((dir (expand-file-name
	      package
	      (expand-file-name (cdr (assoc package
					    package-directory-map))
				"../.."))))
    (cond ((equal package "gnus")
	   (expand-file-name "gnus/lisp" (file-name-as-directory dir)))
	  ((or (equal package "w3") 
	       (equal package "bbdb") 
	       (equal package "jde")
	       (equal package "tramp")
	       (equal package "lookup")
	       (equal package "mule-ucs"))
	   (expand-file-name "lisp" (file-name-as-directory dir)))
	  ((equal package "mew")
	   (expand-file-name "mew" (file-name-as-directory dir)))
	  ((equal package "zenirc")
	   (expand-file-name "src" (file-name-as-directory dir)))
	  (t dir))))

(defvar depends nil)
(defvar command-line-args-left)

(while (and command-line-args-left
	    (not (equal "--" (car command-line-args-left))))
  (push (car command-line-args-left) depends)
  (pop command-line-args-left))

(when command-line-args-left
  (pop command-line-args-left))

;; Setup load-path, data-directory-list and load necessary auto-autoloads
(while depends
  (let* ((dir (package-name-to-directory (car depends)))
	 (etc-dir (expand-file-name "etc" dir)))
    (when (null dir)
      (error "%s is not in `package-directory-map'.  See: package-compile.el"
	     dir))
    (push dir load-path)
    ;; This assumes package has layout *-packages/package/etc/package
    ;; This is the case for the only package it matters at the time or writing
    ;; which is ps-print
    (if (file-directory-p (expand-file-name (car depends) etc-dir))
	(push (file-name-as-directory etc-dir) data-directory-list))
    (load (expand-file-name "auto-autoloads" dir))
    (pop depends)))

;; Lastly, add the current directory
(push default-directory load-path)

;; Let it be known we are running under special circomstances
(defvar bootstrap-in-progress t)

;;; Step 3, perform the requested bytecompilation

;; (message "datadirs = %s" data-directory-list)

;; (prin1 "Load path = ")
;; (prin1 load-path)
;; (terpri)

;; Let the caller specify command
;(batch-byte-compile)

(provide 'package-compile)

;;; package-compile.el ends here
