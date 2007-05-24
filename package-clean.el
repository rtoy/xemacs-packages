;;; package-clean.el --- Remove bogus .elc files in package tree

;; Copyright (C) 1997 by Free Software Foundation, Inc.
;; Copyright (C) 2000, 2001 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>, based on cleantree.el by
;;         Steven L Baur <steve@xemacs.org>

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

;; This is meant to be called -batch at the beginning of package-tree
;; compilation (i.e. `make' from the top of the package tree) and
;; removes out-of-date and orphaned .elc files.

;;; Code:

(setq stack-trace-on-error t)

(when (interactive-p)
  (error "package-clean may only be used with -batch"))

(defvar package-clean-ignored-dirs
  `("." ".." "CVS" "SCCS" "RCS" ,@(unless (featurep 'mule) '("mule"))))

(defvar package-clean-ignored-files
  ;; note: entries here are regexps
  '())

(defun package-clean-do-it (dir)
  ;; Stage 1.
  ;; Remove out-of-date elcs
  (let ((files (directory-files dir t "\\.el$"))
	file file-c)
    (while (setq file (car files))
      (setq files (cdr files))
      (setq file-c (concat file "c"))
      (when (and (file-exists-p file-c)
		 (file-newer-than-file-p file file-c))
	(message "Removing out-of-date %s" file-c)
	(delete-file file-c))))
  ;; Remove elcs without corresponding el
  (let ((files (directory-files dir t "\\.elc$"))
	file file-c)
    (while (setq file-c (car files))
      (setq files (cdr files))
      (setq file (replace-in-string file-c "c$" ""))
      (when (and (file-exists-p file-c)
		 (not (file-exists-p file)))
	(message "Removing %s; no corresponding .el" file-c)
	(delete-file file-c))))

  ;; We descend recursively
  (let ((dirs (directory-files dir t nil t))
	dir)
    (while (setq dir (pop dirs))
      (when (and (not (member (file-name-nondirectory dir)
			      package-clean-ignored-dirs))
		 (file-directory-p dir))
	(package-clean-do-it dir)))))

(message "Removing old or spurious .elcs in directory tree `%s'..."
	 (expand-file-name "."))
(package-clean-do-it ".")
(message "Removing old or spurious .elcs in directory tree `%s'...done"
	 (expand-file-name "."))

