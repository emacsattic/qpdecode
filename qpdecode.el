;;; qpdecode.el --- decode mime quoted-printable messages

;; Copyright (C) 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: mime, decoding, extensions, vm, rmail
;; Created: 1996-09-23

;; $Id: qpdecode.el,v 1.7 2003/03/26 06:45:39 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I recommend putting this in your .emacs, after preloading the
;; appropriate packages or using eval-after-load:
;;
;;         (autoload 'qpdecode-decode-message "qpdecode" nil t)
;;         (define-key vm-mode-map "D" 'qpdecode-decode-message)
;;         (define-key rmail-mode-map "D" 'qpdecode-decode-message)

;; TODO: Recompute or remove Content-MD5 header?
;; The gnus article mode hacking doesn't seem to work quite right yet.

;;; Code:

(require 'sendmail)
(require 'fmailutils)

(defconst qpdecode-decode-headers '("From"))

(defconst qpdecode-content-type-header "Content-Transfer-Encoding")
(defconst qpdecode-content-length-header "Content-Length")

;; These keys are compared against Content-Transfer-Encoding.
(defconst qpdecode-decode-body-methods
  '(("quoted-printable" . qpdecode-quoted-printable-decode-body)))

(defconst qpdecode-decode-header-methods
  '(("=\\?iso-8859-1\\?\\(q\\|quoted-printable\\)\\?"
    . qpdecode-quoted-printable-decode-header)))

;; Mapping between major mode and method used to expose "missing" mail headers.
;; Some mail readers have an option to hide uninteresting mail headers,
;; i.e. to move them outside the narrowed region to display, but offer a
;; way to display them.  This command should toggle between those states.
(defconst qpdecode-toggle-all-mail-headers-method
  '((rmail-mode        . rmail-toggle-header)
    (vm-mode           . vm-expose-hidden-headers)
    (gnus-article-mode . qpdecode-gnus-toggle-article-headers)))

;; This function should widen the currently-narrowed region of the buffer
;; to expose all the headers associated with the current message.
;; It should return whatever state information is required to restore the
;; condition of the headers by qpdecode-hide-exposed-mail-headers-method.
(defconst qpdecode-expose-all-mail-headers-method
  '((rmail-mode        . qpdecode-rmail-widen-message-headers)
    (vm-mode           . qpdecode-vm-widen-message-headers)
    (gnus-article-mode . qpdecode-gnus-widen-article-headers)))

;; This function should use the state information given by
;; qpdecode-expose-all-mail-headers-method to restore the condition of the
;; mail headers (e.g. hide those not normally exposed).
(defconst qpdecode-hide-exposed-mail-headers-method
  '((rmail-mode        . qpdecode-rmail-narrow-message-headers)
    (vm-mode           . qpdecode-vm-narrow-message-headers)
    (gnus-article-mode . qpdecode-gnus-narrow-article-headers)))


(defun qpdecode-decode-message ()
  (interactive)
  (let* ((mail-header-separator "")
         (buffer-read-only nil)
         (case-fold-search t)
         (headers qpdecode-decode-headers)
         (type (qpdecode-get-message-type)))
    (while headers
      (qpdecode-decode-header (car headers))
      (setq headers (cdr headers)))
    (and (null type)
         (yes-or-no-p "Cannot determine msg type.  Assume quoted-printable? ")
         (setq type '("quoted-printable")))
    (cond (type
           (setq type (downcase (car type)))
           (qpdecode-decode-body type)
           (qpdecode-update-encoding-headers type)))))

(defun qpdecode-decode-body (type)
  (let ((method (assoc type qpdecode-decode-body-methods)))
    (and method
         (funcall (cdr method)))))

;; Update the Content-Transfer-Encoding and Content-Length headers.
(defun qpdecode-update-encoding-headers (&rest ignore)
  (let ((re-header-sep (concat "^" (regexp-quote mail-header-separator) "$"))
        (expand-fn (cdr (assq major-mode
                              qpdecode-expose-all-mail-headers-method)))
        (hide-fn (cdr (assq major-mode
                            qpdecode-hide-exposed-mail-headers-method)))
        (state nil)
        (len 0))
    (cond ((and expand-fn
                (setq state (funcall expand-fn)))
           (qpdecode-put-unique-header qpdecode-content-type-header "8bit" t)
           (cond ((fmailutils-position-on-field qpdecode-content-length-header 'soft)
                  (re-search-forward re-header-sep)
                  (setq len (- (point-max) (point) 1))
                  (qpdecode-put-unique-header qpdecode-content-length-header
                                          (int-to-string len) t)))
           (funcall hide-fn state)))))


(defun qpdecode-get-message-type ()
  (let ((mail-header-separator ""))
    (cond ((qpdecode-get-header-contents qpdecode-content-type-header))
          ((let ((fn (cdr (assq major-mode
                                qpdecode-toggle-all-mail-headers-method))))
             (and fn
                  (save-excursion
                    (funcall fn)
                    (prog1
                        (qpdecode-get-header-contents
                         qpdecode-content-type-header)
                      (funcall fn)))))))))

(defun qpdecode-decode-header (header)
  "Decode and replace mime-quoted header contents in current mail message."
  (let* ((contents (car (qpdecode-get-header-contents header)))
         (new-contents nil)
         (methods qpdecode-decode-header-methods))
    (while methods
      (cond ((string-match (car (car methods)) contents)
             (setq new-contents (funcall (cdr (car methods)) contents))
             (setq methods nil)
             (or (eq new-contents contents)
                 (qpdecode-put-unique-header header new-contents t)))
            (t
             (setq methods (cdr methods)))))))

;; Decodes quoted-printable mail headers, returning the replacement string.
;;
;; Example:
;;   (qpdecode-header-decode-quoted-printable
;;    "From: =?ISO-8859-1?Q?Fran=E7ois_Pinard?= <pinard@iro.umontreal.ca>")
;;   => "François Pinard <pinard@iro.umontreal.ca>"
;;
(defun qpdecode-quoted-printable-decode-header (contents)
  (if (string-match "=\\?[^?]*\\?[^?]*\\?\\([^?]*\\)\\?=" contents)
      ;; skip past mime formatting data in `beg' and `end'
      (let ((beg (substring contents 0 (match-beginning 0)))
            (end (substring contents (match-end 0)))
            (mid (substring contents (match-beginning 1) (match-end 1))))
        (concat beg (qpdecode-quoted-printable-decode-string mid) end))
    contents))

(defun qpdecode-quoted-printable-decode-body ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (goto-char (match-end 0))
    (qpdecode-quoted-printable-decode-region (point) (point-max))))

(defun qpdecode-quoted-printable-decode-region (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (search-forward "=\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "=[0123456789ABCDEF][0123456789ABCDEF]" nil t)
      (replace-match (qpdecode-quoted-printable-decode-string
                      (buffer-substring (match-beginning 0)
                                        (match-end 0)))))))

(defun qpdecode-quoted-printable-decode-string (str)
  (let (quoted-p high-nybble low-nybble)
    (mapconcat (function
                (lambda (char)
                  (cond ((eq char ?_)
                         " ")

                        ((eq char ?=)
                         (setq quoted-p t)
                         "")

                        (quoted-p
                         (setq high-nybble
                               (cond ((<= ?a char) (+ (- char ?a) 10))
                                     ((<= ?A char) (+ (- char ?A) 10))
                                     ((<= ?0 char) (- char ?0))))
                         (setq quoted-p nil)
                         "")

                        (high-nybble
                         (setq low-nybble
                               (cond ((<= ?a char) (+ (- char ?a) 10))
                                     ((<= ?A char) (+ (- char ?A) 10))
                                     ((<= ?0 char) (- char ?0))))
                         (prog1
                             (char-to-string (logior (ash high-nybble 4)
                                                     low-nybble))
                           (setq high-nybble nil)))

                        (t (char-to-string char)))))
               str "")))

(defun qpdecode-get-header-contents (header)
  "Return a list containing contents of any headers named HEADER.
If no occurrences of HEADER exist in the current mail buffer, return nil."
  (save-excursion
    (save-restriction
      (let (contents-list
            end
            beg)
        (while (fmailutils-position-on-field header 'soft)
          (setq end (point)
                beg (progn
                      (re-search-backward (concat header ": "))
                      (goto-char (match-end 0)))
                contents-list (cons (buffer-substring beg end) contents-list))
          (narrow-to-region (1+ end) (point-max)))
        (nreverse contents-list)))))

(defun qpdecode-put-unique-header (header contents &optional replace)
  "Add HEADER to the current mail message, with the given CONTENTS.
If the header already exists, the contents are left unchanged, unless optional
argument REPLACE is non-nil."
  (save-excursion
    (let ((header-exists (fmailutils-position-on-field header)))
      ;; Delete old contents if replace is set
      (if (and header-exists replace)
          (let ((end (point))
                (beg (progn
                       (re-search-backward (concat header ": "))
                       (goto-char (match-end 0)))))
            (delete-region beg end)))
      ;; Add new contents if replace is set, or this is a new header.
      (if (or (not header-exists) replace)
          (progn (insert contents) contents)))))


;; Note that these functions change the narrowed region by side effect!

;;; VM

;; Expose all headers of the current message, returning a marker pointing
;; to the original start of the narrowed region.
(defun qpdecode-vm-widen-message-headers ()
  (let ((beg (point-min))
        (end (point-max))
        (re-header-start "^From "))
    (goto-char (point-min))
    (widen)
    ;; In VM, if the headers are already toggled show-all, then the
    ;; envelope address is also already in view (i.e. be at point-min), so
    ;; first scan forward to the end of the message headers, then go back.
    (re-search-forward "^$" nil t)
    (cond ((re-search-backward re-header-start nil t)
           (end-of-line)
           (forward-char 1)
           (narrow-to-region (point) end)
           (set-marker (make-marker) beg))
          (t
           ;; Couldn't find start of messages; restore original region and
           ;; return nil to indicate no changes.
           (narrow-to-region beg end)
           nil))))

;; Use the saved marker to re-hide temporarily-exposed headers.
(defun qpdecode-vm-narrow-message-headers (mark)
  (and mark
       (narrow-to-region mark (point-max))))

;;; RMAIL

(defun qpdecode-rmail-all-headers-exposed-p ()
  (save-restriction
    (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
    (goto-char (point-min))
    (forward-line 1)
    (= (following-char) ?0)))

(defun qpdecode-rmail-widen-message-headers ()
  (cond ((qpdecode-rmail-all-headers-exposed-p)
         'ignore)
        (t
         (rmail-toggle-header)
         'hide)))

(defun qpdecode-rmail-narrow-message-headers (action)
  (and (eq action 'hide)
       (rmail-toggle-header)))

;;; GNUS

(defun qpdecode-gnus-toggle-article-headers ()
  (gnus-article-hide-headers 0))

(defun qpdecode-gnus-widen-article-headers ()
  (gnus-article-hide-headers -1))

(defun qpdecode-gnus-narrow-article-headers (&rest ignore)
  (gnus-article-hide-headers 1))

(provide 'qpdecode)

;;; qpdecode.el ends here
