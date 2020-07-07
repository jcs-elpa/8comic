;;; 8comic.el --- Use 8comic to read manga  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-05 22:43:56

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Use 8comic to read manga.
;; Keyword: comic manga read reader
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;; URL: https://github.com/jcs-elpa/8comic

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use 8comic to read manga.
;;

;;; Code:

(require 'request)

(defgroup 8comic nil
  "Use 8comic to read manga."
  :prefix "8comic-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/8comic"))

(defconst 8comic--url-base "https://comicbus.com"
  "URL 8comic base.")

(defconst 8comic--url-html-format (concat 8comic--url-base "/html/%s.html")
  "URL 8comic front page format.")

(defconst 8comic--url-view-1-format (concat 8comic--url-base "/view/%s.html?ch=%s")
  "URL 8comic view format for first page.")

(defconst 8comic--url-view-a-format (concat 8comic--url-base "/view/%s.html?ch=%s-%s")
  "URL 8comic view format after first page.")

(defvar 8comic--menu-list '()
  "List of comic menu.")

(defun 8comic--insert-image-by-url (url)
  "Insert image by URL."
  (unless url (user-error "[WARNING] Couldn't find URL"))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

(defun 8comic--form-base-url (post-url)
  "Form full URL by POST-URL."
  (concat 8comic--url-base post-url))

(defun 8comic--page-404-p (data)
  "Check if page 404 by html string DATA."
  (string-match-p "404" data))

(defun 8comic--comic-image (index)
  "Get the front page image URL by page INDEX."
  (format (8comic--form-base-url "/pics/0/%s.jpg") index))

(defun 8comic--get-front-page-data (index data)
  "Get all needed front page data by mange INDEX and html DATA."
  (unless (8comic--page-404-p data)
    (message "data: %s" data)))

(defun 8comic--request-page (index)
  "Requet the comic page by INDEX."
  (request
    (format 8comic--url-html-format index)
    :type "GET"
    :parser 'buffer-string
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (8comic--get-front-page-data index data)))))

;;; Table

(defconst 8comic--table-format
  (vector (list "ID" 8 t)
          (list "Name" 10 t)
          (list "Description" 20 t))
  "Format to assign to `tabulated-list-format' variable.")

(defun 8comic--get-menu-entries ()
  "Get all the entries for table."
  (let ((entries '()) (id-count 0))
    ;; TODO: ..
    entries))

(define-derived-mode 8comic-menu-mode tabulated-list-mode
  "8comic-menu-mode"
  "Major mode for 8comic menu mode."
  :group '8comic
  (setq tabulated-list-format 8comic--table-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (8comic--get-menu-entries))
  (tabulated-list-print t))

(provide '8comic)
;;; 8comic.el ends here
