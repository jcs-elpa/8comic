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

(defconst 8comic--chapter-id "id=\"c"
  "Chapter identifier.")

(defconst 8comic--chapter-id-format (concat 8comic--chapter-id "%s\"")
  "Chapter identifier format.")

(defvar 8comic--menu-dic '()
  "Comic menu dictionary.")

(defvar 8comic--missing-episodes 10
  "Allow how many missing gap episodes from one episode to another episode.")

;;; Util

(defun 8comic--set-hash-by-index (index val)
  "Assign VAL to hash by id INDEX."
  (setf (gethash index 8comic--menu-dic) val))

(defun 8comic--get-hash-by-index (index)
  "Return the hash data by key INDEX."
  (gethash index 8comic--menu-dic))

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

;;; Request

(defun 8comic--form-base-url (post-url)
  "Form full URL by POST-URL."
  (concat 8comic--url-base post-url))

(defun 8comic--form-page-data (name img-url episodes)
  "Form the comic data by front page information, NAME, IMG-URL, EPISODES."
  (list :name name :image img-url :episodes episodes))

(defun 8comic--page-404-p (data)
  "Check if page 404 by html string DATA."
  (string-match-p "404" data))

(defun 8comic--comic-page-p (data)
  "Check if page a comic page by html string DATA."
  (string-match-p 8comic--chapter-id data))

(defun 8comic--comic-name (data)
  "Return a list of target comic's name by html string DATA."
  ;; TODO: get the name of the target manga.
  "")

(defun 8comic--comic-image (index)
  "Get the front page image URL by page INDEX."
  (format (8comic--form-base-url "/pics/0/%s.jpg") index))

(defun 8comic--comic-episodes (data)
  "Return a list of target comic's episodes by html string DATA."
  (let ((lst '()) (ep 0) (id-str "") (found nil)
        (max-ep 8comic--missing-episodes))
    (while (< ep max-ep)
      (setq id-str (format 8comic--chapter-id-format ep))
      (setq found (string-match-p id-str data))
      (when found
        (setq max-ep (+ ep 8comic--missing-episodes))  ; Raise max episodes search limit.
        (push ep lst))
      (setq ep (1+ ep)))
    (reverse lst)))

(defun 8comic--get-front-page-data (index data)
  "Get all needed front page data by mange INDEX and html DATA."
  (when (and (not (8comic--page-404-p data))
             (8comic--comic-page-p data))
    (message "data: %s" data)
    (let* ((name (8comic--comic-name data))
           (img-url (8comic--comic-image index))
           (episodes (8comic--comic-episodes data))
           (page-data (8comic--form-page-data name img-url episodes)))
      (8comic--set-hash-by-index index page-data))))

(defun 8comic--request-page (index)
  "Requet the comic page by INDEX."
  (8comic--ensure-hash-table)
  (request
    (format 8comic--url-html-format index)
    :type "GET"
    :parser 'buffer-string
    :encoding 'utf-8
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (8comic--get-front-page-data index data)))))

(defun 8comic--ensure-hash-table (&optional reset)
  "Ensure menu list a hash table object.
If RESET is non-nil, will force to make a new hash table."
  (when (or reset (not (hash-table-p 8comic--menu-dic)))
    (setq 8comic--menu-dic (make-hash-table))))

;;;###autoload
(defun 8comic-refresh-menu-list ()
  "Refresh menu list once."
  (interactive)
  (8comic--ensure-hash-table t)
  (let ((end 1000) (index 2))
    (while (<= index end)
      (8comic--request-page index)
      (setq index (1+ index)))))

;;; Front Page



;;; Menu

(defconst 8comic--menu-table-format
  (vector (list "ID" 8 t)
          (list "Name" 10 t)
          (list "Description" 20 t))
  "Format to assign to `tabulated-list-format' variable.")

(defvar 8comic-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '8comic-menu-enter)
    (define-key map (kbd "<mouse-1>") '8comic-menu-enter)
    map)
  "Keymap for `8comic-menu-mode'.")

(defun 8comic-menu-enter ()
  "Enter the selected item, goto the front page of the selected comic."
  (interactive)
  ;; TODO: Goto front page.
  )

(defun 8comic--get-menu-entries ()
  "Get all the entries for table."
  (let ((entries '()) (id-count 0))
    ;; TODO: ..
    entries))

(define-derived-mode 8comic-menu-mode tabulated-list-mode
  "8comic-menu-mode"
  "Major mode for 8comic menu mode."
  :group '8comic
  (setq tabulated-list-format 8comic--menu-table-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (8comic--get-menu-entries))
  (tabulated-list-print t))

;;;###autoload
(defun 8comic ()
  "Start 8comic menu."
  (interactive)
  (pop-to-buffer "*8comic-menu*" nil)
  (8comic-menu-mode))

(provide '8comic)
;;; 8comic.el ends here
