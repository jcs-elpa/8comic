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


(provide '8comic)
;;; 8comic.el ends here
