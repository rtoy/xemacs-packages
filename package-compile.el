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

;; (setq stack-trace-on-error t)

(when (interactive-p)
  (error "package-compile may only be used with -batch"))

;;; Step 1, set the load-path and load the core autoloads.

(let* ((roots (paths-find-emacs-roots invocation-directory
				      invocation-name))
       (lisp-directory (paths-find-lisp-directory roots)))
  (setq load-path (paths-find-recursive-load-path (list lisp-directory)
						  paths-load-path-depth)))

(load (expand-file-name "auto-autoloads" (car load-path)))

;;; Step 2, collect the dependencies into load-path and load the autoloads.
(require 'bytecomp)

(defconst package-directory-map
  '(
    ;; libs/*
    ("Sun" . "libs")
    ("apel" . "libs")
    ("dired" . "libs")
    ("edebug" . "libs")
    ("efs" . "libs")
    ("elib" . "libs")
    ("fsf-compat" . "libs")
    ("mail-lib" . "libs")
    ("sounds-au" . "libs")
    ("sounds-wav" . "libs")
    ("tooltalk" . "libs")
    ("xemacs-base" . "libs")
    ("xemacs-devel" . "libs")
    ;; mule/*
    ("edict" . "mule")
    ("egg-its" . "mule")
    ("leim" . "mule")
    ("locale" . "mule")
    ("lookup" . "mule")
    ("mule-base" . "mule")
    ("skk" . "mule")
    ;; prog/*
    ("ada" . "prog")
    ("c-support" . "prog")
    ("cc-mode" . "prog")
    ("debug" . "prog")
    ("ediff" . "prog")
    ("emerge" . "prog")
    ("idlwave" . "prog")
    ("jde" . "prog")
    ("pcl-cvs" . "prog")
    ("prog-modes" . "prog")
    ("scheme" . "prog")
    ("semantic" . "prog")
    ("sh-script" . "prog")
    ("vc" . "prog")
    ("vc-cc" . "prog")
    ("vhdl" . "prog")
    ;; comm/*
    ("bbdb" . "comm")
    ("eicq" . "comm")
    ("eudc" . "comm")
    ("footnote" . "comm")
    ("gnats" . "comm")
    ("gnus" . "comm")
    ("mailcrypt" . "comm")
    ("mew" . "comm")
    ("mh-e" . "comm")
    ("net-utils" . "comm")
    ("rmail" . "comm")
    ("supercite" . "comm")
    ("tm" . "comm")
    ("vm" . "comm")
    ("w3" . "comm")
    ("zenirc" . "comm")
    ;; games/*
    ("cookie" . "games")
    ("games" . "games")
    ("mine" . "games")
    ("misc-games" . "games")
    ;; oa/*
    ("calc" . "oa")
    ("calendar" . "oa")
    ("edit-utils" . "oa")
    ("forms" . "oa")
    ("frame-icon" . "oa")
    ("hm--html-menus" . "oa")
    ("ispell" . "oa")
    ("pc" . "oa")
    ("psgml" . "oa")
    ("sgml" . "oa")
    ("slider" . "oa")
    ("speedbar" . "oa")
    ("strokes" . "oa")
    ("text-modes" . "oa")
    ("time" . "oa")
    ;; os/*
    ("eterm" . "os")
    ("igrep" . "os")
    ("ilisp" . "os")
    ("os-utils" . "os")
    ("ps-print-nomule" . "os")
    ("view-process" . "os")
    ;; wp/*
    ("auctex" . "wp")
    ("crisp" . "wp")
    ("edt" . "wp")
    ("reftex" . "wp")
    ("texinfo" . "wp")
    ("textools" . "wp")
    ("tpu" . "wp")
    ("viper" . "wp")))

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
	       (equal package "lookup"))
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

;; Setup load-path and load necessary auto-autoloads
(while depends
  (let ((dir (package-name-to-directory (car depends))))
    (when (null dir)
      (error "%s is not in `package-directory-map'.  See: package-compile.el"
	     dir))
    (push dir load-path)
    (load (expand-file-name "auto-autoloads" dir))
    (pop depends)))

;; Lastly, add the current directory
(push default-directory load-path)

;;; Step 3, perform the requested bytecompilation

;; (prin1 "Load path = ")
;; (prin1 load-path)
;; (terpri)

(batch-byte-compile)

(provide 'package-compile)

;;; package-compile.el ends here
