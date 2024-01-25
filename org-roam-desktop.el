;;; org-roam-desktop.el --- tool for inspection and revision of an org-roam zettlekasten.

;; Author: Jesse Burke <jtb445@gmail.com>
;;
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (org-roam "2.2.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; vars and customizations
(require 'org-roam)
(require 'magit-section)

(defgroup org-roam-desktop nil
  "Inspection and revision of org-roam notes."
  :group 'org-roam
  :prefix "org-roam-desktop-"
  :link '(url-link :tag "Github" "https://github.com/jesseburke/org-roam-desktop"))

(defcustom org-roam-desktop-dir "ord-collections/"
  "Default directory to for collection files."
  :group 'org-roam-desktop
  :type 'string)

(cl-defstruct org-roam-desktop-collection
  name id nodes)

(defvar org-roam-desktop-collection-list '())
;; (setq org-roam-desktop-collection-list '())

;;; basic functions for collections
(defun org-roam-desktop-create-collection (collection-name)
  (interactive (list (read-string "name of new collection: " "")))
  (add-to-list
   'org-roam-desktop-collection-list
   (make-org-roam-desktop-collection :name collection-name
                                     :id (concat "ord-" (org-id-uuid))
                                     :nodes [])))

(defun ord--collection-to-plist (collection)
"COLLECTION should be an org-roam-desktop-collection struct. Returns a
  plist representing that struct."
(list :name (org-roam-desktop-collection-name collection)
      :id (org-roam-desktop-collection-id collection)
      :nodes (org-roam-desktop-collection-nodes
              collection)))

(defun ord--collection-from-plist (collection-plist)
  "COLLECTION-PLIST should have keys :name, :id, and :nodes. Returns a
  struct of type org-roam-desktop-collection."
  )

;; (setq test-collection (car org-roam-desktop-collection-list))
;; (ord--collection-to-plist test-collection)
;; (json-serialize (ord--collection-to-plist test-collection))

(defun ord--collection-to-json (collection)
  "COLLECTION should be an org-roam-desktop-collection struct. Returns a
  json string representing that struct."
  (json-serialize (ord--collection-to-plist collection)))

(defun ord--collection-from-json (collection-json)
  ""
)
  
(defun ord--choose-collection-by-name ()
  "To be passed to interactive form: lets the user choose from among
the names of collections currently in
  org-roam-desktop-collection-list, and then returns the full
  collection of the name selected."
  (let ((extended-list
         (seq-map (lambda (collection)
                    (cons
                     (org-roam-desktop-collection-name collection)
                     collection))
                  org-roam-desktop-collection-list)))
    (cdr (assoc (completing-read "Choose collection: " extended-list)
                extended-list))))

;; (ord--collection-to-json test-collection)

;;; add nodes to a collection

(defun org-roam-desktop-add-node-at-point (collection)
  (interactive (list (ord--choose-collection-by-name)))  
  (setf
   (org-roam-desktop-collection-nodes collection)
   (vconcat (org-roam-desktop-collection-nodes collection)
            (list (org-roam-id-at-point)))))

;;; viewing a collection
(define-derived-mode org-roam-desktop-mode magit-section-mode "OrgRoamDesktop"
  "Major mode for displaying collection of org-roam nodes.")

(defun ord--buffer-name-for-collection (collection)
  "The name of a buffer for a collection starts with `*ord
collection: ` plus collection name and the
first 6 digits of its id."
  (concat
   "*ord collection: "
   (org-roam-desktop-collection-name collection)
   " ("
   (substring (org-roam-desktop-collection-id collection) 5 11)
   ")"))

(defun ord--default-file-name-for-collection (collection)
  "The name of a file for a collection is its name plus json file extension."
  (concat   
   (org-roam-desktop-collection-name collection)   
   ".json"))

(defun magit-section-mode-buffer-setup (title display-func)
  "Wrapper function for displaying sections in a buffer whose mode is
derived from magit-section. TITLE is the title of the buffer. The
  buffer is erased, a root magit-section is added and
  DISPLAY-FUNC is called inside of the root section. Then
  switch-buffer-other-window is called to show the buffer."
  (let ((buffer (get-buffer-create title)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-desktop-mode)
        (magit-insert-section (root)          
          (funcall display-func))))
    (switch-to-buffer-other-window buffer)))

;; should define function that takes an entry from a collection, and
;; inserts a section for that entry

(defun ord-view-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name))) 
   (let ((node-ids (org-roam-desktop-collection-nodes collection)))
     (magit-section-mode-buffer-setup
      (ord--buffer-name-for-collection collection)
      (lambda ()
        (seq-do
         (lambda (id)
           (magit-insert-section section (org-roam-node-section)
             (let ((node (org-roam-node-from-id id)))
               (insert (concat (propertize (org-roam-node-title node)
                                           'font-lock-face 'org-roam-title)))
               (magit-insert-heading)
               (oset section node node))))
         node-ids)))))

;;; save, close, and load collections

(defun ord-save-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name)))
    (let ((file-name (read-file-name
                     "Save collection as: "
                     (file-name-concat org-roam-directory org-roam-desktop-dir)
                     nil
                     nil
                     (ord--default-file-name-for-collection
                      collection)))
          (json-str (ord--collection-to-json collection)))
      (unless (file-directory-p (file-name-directory file-name))
        (make-directory (file-name-directory file-name) t))
      (with-temp-file file-name
        (insert json-str))))

(defun ord-close-collection (collection-to-remove)
  (setq org-roam-desktop-collection-list
        (seq-remove (lambda (collection)
                      (eq collection collection-to-remove))
                    org-roam-desktop-collection-list)))

(defun ord-save-and-close-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name)))
  (ord-save-collection collection)
  (ord-close-collection collection))

(defun ord-load-collection ()
  (interactive)
  (let ((file-name (read-file-name
                     "Find collection: "
                     (file-name-concat org-roam-directory org-roam-desktop-dir)
                     nil
                     t)))
    (message file-name)))

(define-prefix-command 'org-roam-desktop-map)
(define-key org-roam-desktop-map (kbd "c")
            #'org-roam-desktop-create-collection)
(define-key org-roam-desktop-map (kbd "a")
            #'org-roam-desktop-add-node-at-point)
(define-key org-roam-desktop-map (kbd "v")
            #'ord-view-collection)
(define-key org-roam-desktop-map (kbd "s") #'ord-save-collection)
(define-key org-roam-desktop-map (kbd "S") #'ord-save-and-close-collection)
(define-key org-roam-desktop-map (kbd "l") #'ord-load-collection)

(global-set-key (kbd "M-d") org-roam-desktop-map)


 ;; (define-minor-mode org-roam-desktop-minor-mode
 ;;  "Global minor mode to add nodes to org-roam-desktop collections."
 ;;  :lighter " or-desktop"
 ;;  :keymap org-roam-desktop-mode-map
 ;;  :global t
 ;;  :group 'org-roam-desktop)
  
(provide 'org-roam-desktop)
