;;; hack-package-index.el --- Update package-get-base package index file

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@xemacs.org>
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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; usage xemacs -batch -l hack-package-index.el -f batch-hack-package-index \
;;  package-name /path/to/package-info /path/to/package-get-base.el

;;; Code:

(defvar end-of-package-marker ";;;@@@"
  "Marker in between entries.")

(defun locate-package-entry (name)
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^(package-get-update-base-entry (quote\n(" name "$") nil t)
      (let ((from (match-beginning 0)))
	(re-search-forward (concat "^" end-of-package-marker "$"))
	(delete-region from (1+ (point-at-eol))))
    (re-search-forward (concat "^" end-of-package-marker "$"))
    (forward-char)))

(defun convert-crlf ()
  "Convert CRLF to LF."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n" t t))))

(defun batch-hack-package-index ()
  (let* ((package-name (pop command-line-args-left))
	 (package-info (pop command-line-args-left))
	 (package-get-base (pop command-line-args-left))
	 (category (pop command-line-args-left))
	 (unsupinfo (if (equal category "unsupported")
			" (Unsupported Packages)" "")))
    (set-buffer (get-buffer-create "*package index*"))
    (when (file-exists-p package-get-base)
      (insert-file-contents-literally package-get-base))
    (when (eq (point-min) (point-max))
      (insert (format ";; Package Index file%s -- Do not edit manually.\n"
		      unsupinfo))
      (insert end-of-package-marker "\n")
      (insert (format ";; Package Index file%s ends here\n"
		      unsupinfo)))
    (locate-package-entry package-name)
    (insert "))\n")
    (insert end-of-package-marker "\n")
    (forward-line -2)
    (insert "(package-get-update-base-entry (quote\n")
    (let ((length
           (second (insert-file-contents-literally package-info))))
      (save-restriction
        (narrow-to-region (point) (+ (point) length))
        (convert-crlf)))
    (write-file package-get-base)))

(provide 'hack-package-index)

;;; hack-package-index.el ends here
