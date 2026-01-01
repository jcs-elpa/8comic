;;; 8comic.el --- Use 8comic to read manga  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026  Shen, Jen-Chieh
;; Created date 2020-07-05 22:43:56

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/8comic
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (request "0.3.0"))
;; Keywords: multimedia comic manga read reader

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

(require 'cl-lib)
(require 'request)

(defgroup 8comic nil
  "Use 8comic to read manga."
  :prefix "8comic-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/8comic"))

(defcustom 8comic-watch-list '()
  "List of comic you would like to keep track of when first load."
  :type 'list
  :group '8comic)

(defcustom 8comic-max-request-limit 10
  "Maximum request at a time."
  :type 'integer
  :group '8comic)

(defconst 8comic--url-base "https://comicbus.com"
  "URL 8comic base.")

(defconst 8comic--url-html-format (concat 8comic--url-base "/html/%s.html")
  "URL 8comic front page format.")

(defconst 8comic--chapter-id-regex "[0-9]+[卷話][ <]"
  "Chapter identifier.")

(defconst 8comic--description-max-characters 20
  "Maximum of description characters..")

(defvar 8comic--show-debug-message t
  "Show the debug message from this package.")

(defvar 8comic--menu-dic '()
  "Comic menu dictionary.")

(defvar 8comic--request-index 0
  "Request page index.")

(defvar 8comic--request-completed 0
  "Request completed counter.")

;;
;; (@* "Util" )
;;

(defun 8comic--debug-message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when 8comic--show-debug-message
    (apply 'message fmt args)))

(defun 8comic--form-front-page-url-by-id (id)
  "Form front page URL by ID."
  (format 8comic--url-html-format id))

(defun 8comic--set-hash-by-index (index val)
  "Assign VAL to hash by id INDEX."
  (setf (gethash index 8comic--menu-dic) val))

(defun 8comic--get-hash-by-index (index)
  "Return the hash data by key INDEX."
  (when (stringp index) (setq index (string-to-number index)))
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

;;
;; (@* "Request" )
;;

(defun 8comic--total-requests ()
  "Totally requests calculation."
  (length 8comic-watch-list))

(defun 8comic--done-all-requests-p ()
  "Check if all page requests done."
  (<= (8comic--total-requests) 8comic--request-index))

(defun 8comic--received-all-requests-p ()
  "Check if all page requests done."
  (<= (8comic--total-requests) (1+ 8comic--request-completed)))

(defun 8comic--add-request-index ()
  "Add up the request counter by 1."
  (setq 8comic--request-index (1+ 8comic--request-index)))

(defun 8comic--add-request-completed ()
  "Add up the request counter by 1."
  (setq 8comic--request-completed (1+ 8comic--request-completed)))

(defun 8comic--form-base-url (post-url)
  "Form full URL by POST-URL."
  (concat 8comic--url-base post-url))

(defun 8comic--form-page-data (name desc img-url episodes)
  "Form the comic data by front page information, NAME, DESC, IMG-URL, EPISODES."
  (list :name name :description desc :image img-url :episodes episodes))

(defun 8comic--page-404-p (data)
  "Check if page 404 by html string DATA."
  (string-match-p "<title>404" data))

(defun 8comic--comic-page-p (data)
  "Check if page a comic page by html string DATA."
  (string-match-p 8comic--chapter-id-regex data))

(defun 8comic--comic-name (data)
  "Return a list of target comic's name by html string DATA."
  (let ((key-str "letter-spacing:1px\">") (name "") start end)
    (setq start (+ (string-match-p key-str data) (length key-str))
          end (string-match-p "</" data start)
          name (substring data start end))
    name))

(defun 8comic--comic-description (data)
  "Get the comice description from DATA."
  (let ((key-str "line-height:25px\">") (desc "") start end max-chars limited)
    (setq start (+ (string-match-p key-str data) (length key-str) 2)
          end (string-match-p "</" data start)
          desc (substring data start end)
          desc (string-trim desc)
          max-chars (length desc)
          limited (< 8comic--description-max-characters max-chars))
    (when limited (setq max-chars 8comic--description-max-characters))
    (setq desc (substring desc 0 max-chars))
    (if limited (concat desc "..") desc)))

(defun 8comic--comic-image (index)
  "Get the front page image URL by page INDEX."
  (format (8comic--form-base-url "/pics/0/%s.jpg") index))

(defun 8comic--comic-episodes (data)
  "Return a list of target comic's episodes by html string DATA."
  (let ((search-start (string-match-p 8comic--chapter-id-regex data 0))
        (search-end -1)
        start end lst)
    (while search-start
      (setq search-end (string-match-p "[</]" data search-start)
            start search-start
            end search-end)
      (push (substring data start end) lst)
      (setq search-start (string-match-p 8comic--chapter-id-regex data search-end)))
    (reverse (cl-remove-duplicates lst :test (lambda (x y) (string= x y))))))

(defun 8comic--get-front-page-data (index data)
  "Get all needed front page data by mange INDEX and html DATA."
  (when (and (not (8comic--page-404-p data)) (8comic--comic-page-p data))
    (let* ((name (8comic--comic-name data))
           (desc (8comic--comic-description data))
           (img-url (8comic--comic-image index))
           (episodes (8comic--comic-episodes data))
           (page-data (8comic--form-page-data name desc img-url episodes)))
      (8comic--set-hash-by-index index page-data))))

(defun 8comic--request-front-page (index &optional callback)
  "Requet the comic page by INDEX.
If CALLBACK is non-nil, we move towards to comic menu page."
  (8comic--debug-message "[INFO] Requesting 8comic page ID %s" index)
  (8comic--ensure-hash-table)
  (request
    (8comic--form-front-page-url-by-id index)
    :type "GET"
    :parser 'buffer-string
    :encoding 'big5
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (8comic--get-front-page-data index data)
       (when callback (8comic--after-request))))
    :error
    (cl-function
     (lambda (&rest args &key _error-thrown &allow-other-keys)
       (when callback (8comic--after-request))))))

(defun 8comic--after-request ()
  "Function that call after request."
  (8comic--add-request-completed) (8comic--add-request-index)
  (8comic--next-request)
  (8comic--done-refresh-callback))

(defun 8comic--next-request ()
  "Send the next page request."
  (unless (8comic--done-all-requests-p)
    (8comic--request-front-page (nth 8comic--request-index 8comic-watch-list) t)))

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
  (setq 8comic--request-index 0
        8comic--request-completed 0)
  (let (comic-id break)
    (while (and (not break) (< 8comic--request-index 8comic-max-request-limit))
      (setq comic-id (nth 8comic--request-index 8comic-watch-list))
      (if comic-id (8comic--request-front-page comic-id t) (setq break t))
      (8comic--add-request-index))))

;;
;; (@* "Comic Page" )
;;

(defconst 8comic--url-view-1-format (concat 8comic--url-base "/view/%s.html?ch=%s")
  "URL 8comic view format for first page.")

(defconst 8comic--url-view-a-format (concat 8comic--url-base "/view/%s.html?ch=%s-%s")
  "URL 8comic view format after first page.")

(defvar 8comic--display-episode -1
  "Episode currently displaying.")

(defun 8comic--request-comic-page (episode)
  "Request the current EPISODE page."
  (request
    (format 8comic--url-view-1-format 8comic--display-id 8comic--display-episode)
    :type "GET"
    :parser 'buffer-string
    :encoding 'utf-8
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (8comic--collect-image-data data)))
    :error
    (cl-function
     (lambda (&rest args &key _error-thrown &allow-other-keys)
       (user-error "[ERROR] Request error getting %s, episode %s"
                   8comic--display-name 8comic--display-episode)))))

(defun 8comic--collect-image-data (data)
  "Collect all image DATA."
  (jcs-log-clean "%s" data)
  (message "%s"
           (format 8comic--url-view-1-format 8comic--display-id 8comic--display-episode)))

(defun 8comic--display-comic (data)
  "Display comic by first page DATA."
  (switch-to-buffer (format "*8comic: %s <%s>*"
                            8comic--display-name 8comic--display-episode))
  (insert (format 8comic--url-view-1-format 8comic--display-id 8comic--display-episode))
  (insert "\n")

  ;; TODO: ..
  )

(defun 8comic--to-comic-page (episode)
  "Lead the buffer to comic EPISODE display page."
  (setq 8comic--display-episode episode)
  (8comic--request-comic-page episode))

;;
;; (@* "Front Page" )
;;

(defconst 8comic--front-page-table-format
  (vector (list "Episode" 6 t))
  "Format to assign to `tabulated-list-format' variable for front page.")

(defvar 8comic-front-page-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '8comic-front-page-enter)
    (define-key map (kbd "<mouse-1>") '8comic-front-page-enter)
    map)
  "Keymap for `8comic-front-page-mode'.")

(defvar 8comic--display-id -1
  "Current displayed comic ID.")

(defvar 8comic--display-data nil
  "Current displayed comic data, data is a plist.")

(defvar 8comic--display-name ""
  "Current displayed comic name.")

(defun 8comic-front-page-enter ()
  "Return key in front page."
  (interactive)
  (let* ((episode (elt (tabulated-list-get-entry) 0))
         (end (string-match-p "[^0-9]" episode)))
    (setq episode (substring episode 0 end)
          episode (string-to-number episode))
    (8comic--to-comic-page episode)))

(defun 8comic--make-entry-front-page (id ep)
  "Make new entry by data, ID, episode (EP)."
  (let (new-entry new-entry-value)
    (progn
      (when (numberp id) (setq id (number-to-string id)))
      (when (numberp ep) (setq ep (number-to-string ep))))
    (push ep new-entry-value)  ; Episode Number
    (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
    (push id new-entry)  ; ID, index is ID.
    new-entry))

(defun 8comic--get-front-page-entries ()
  "Get the front page data."
  (let (entries new-entry
                (episodes (plist-get 8comic--display-data :episodes)))
    (dolist (ep episodes)
      (setq new-entry
            (8comic--make-entry-front-page
             ep
             (propertize (format "%s" ep) 'face 'font-lock-builtin-face)))
      (push new-entry entries))
    (reverse entries)))

(define-derived-mode 8comic-front-page-mode tabulated-list-mode
  "8comic-front-page-mode"
  "Major mode for 8comic front page mode."
  :group '8comic
  (setq tabulated-list-format 8comic--front-page-table-format
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Episode" t)
        tabulated-list--header-string
        (format "URL: %s" (8comic--form-front-page-url-by-id 8comic--display-id)))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (8comic--get-front-page-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

(defun 8comic--to-front-page (id)
  "Move from menu page to front page by compic ID."
  (setq 8comic--display-id id
        8comic--display-data (8comic--get-hash-by-index id)
        8comic--display-name (plist-get 8comic--display-data :name))
  (switch-to-buffer (format "*8comic: %s*" 8comic--display-name) nil)
  (8comic-front-page-mode))

;;
;; (@* "Menu" )
;;

(defconst 8comic--menu-table-format
  (vector (list "ID" 6 t)
          (list "Name" 15 t)
          (list "Description" 20 t))
  "Format to assign to `tabulated-list-format' variable for menu page.")

(defvar 8comic-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '8comic-menu-enter)
    (define-key map (kbd "<mouse-1>") '8comic-menu-enter)
    map)
  "Keymap for `8comic-menu-mode'.")

(defun 8comic-menu-enter ()
  "Enter the selected item, goto the front page of the selected comic."
  (interactive)
  (let ((id (tabulated-list-get-id))) (when id (8comic--to-front-page id))))

(defun 8comic--make-entry-menu (id name desc)
  "Make new entry by data, ID, NAME, DESC."
  (let (new-entry new-entry-value)
    (setq id (number-to-string id))
    (push desc new-entry-value)  ; Description
    (push name new-entry-value)  ; Name
    (push id new-entry-value)  ; ID, index is ID.
    (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
    (push id new-entry)  ; ID, index is ID.
    new-entry))

(defun 8comic--get-menu-entries ()
  "Get all the entries for table."
  (let (entries data new-entry)
    (dolist (index 8comic-watch-list)
      (setq data (8comic--get-hash-by-index index))
      (when data
        (setq new-entry
              (8comic--make-entry-menu index
                                       (propertize (plist-get data :name)
                                                   'face 'font-lock-builtin-face)
                                       (plist-get data :description)))
        (push new-entry entries)))
    entries))

(define-derived-mode 8comic-menu-mode tabulated-list-mode
  "8comic-menu-mode"
  "Major mode for 8comic menu mode."
  :group '8comic
  (setq tabulated-list-format 8comic--menu-table-format
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (8comic--get-menu-entries))
  (tabulated-list-print t))

(defun 8comic--done-refresh-callback ()
  "Callback when done all page requests."
  (when (8comic--received-all-requests-p)
    (pop-to-buffer "*8comic-menu*" nil)
    (8comic-menu-mode)))

;;;###autoload
(defun 8comic ()
  "Start 8comic menu."
  (interactive)
  (8comic-refresh-menu-list))

(provide '8comic)
;;; 8comic.el ends here
