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

(defcustom ord-mode-entry-section-functions
  (list 'ord-mode-entry-section-function-default)
  "Function that draws the section for each entry in a collection."
  :group 'org-roam-desktop
  :type 'function)

(cl-defstruct org-roam-desktop-collection
  name id nodes)

(defvar list-of-org-roam-desktop-collections '())

(defvar ord-buffer-current-collection nil
  "A buffer local variable: the collection which an a buffer in
   ord-mode is displaying.")
(put 'ord-buffer-current-collection 'permanent-local t)

;; (setq test-collection (car list-of-org-roam-desktop-collections))
;; (setq test-node-id (elt (org-roam-desktop-collection-nodes test-collection) 0))

;; (funcall ord-mode-entry-section-functions test-node-id)

;;; basic functions for collections
(defun org-roam-desktop-create-collection (collection-name)
  (interactive (list (read-string "name of new collection: " "")))
  (add-to-list
   'list-of-org-roam-desktop-collections
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

;; (setq test-collection (car list-of-org-roam-desktop-collections))
;; (ord--collection-to-plist test-collection)
;; (json-serialize (ord--collection-to-plist test-collection))

(defun ord--collection-from-plist (collection-plist)
  "COLLECTION-PLIST should have keys :name, :id, and :nodes. Returns a
  struct of type org-roam-desktop-collection."
  (cl-destructuring-bind (&key name id nodes) collection-plist
    (make-org-roam-desktop-collection :name name :id id :nodes nodes)))

;; (ord--collection-from-plist (ord--collection-to-plist test-collection))

(defun ord--collection-to-json (collection)
  "COLLECTION should be an org-roam-desktop-collection struct. Returns a
  json string representing that struct."
  (json-serialize (ord--collection-to-plist collection)))

;; (ord--collection-to-json test-collection)

(defun ord--collection-from-json (collection-json)
  "COLLECTION-JSON should be a string of json, with keys name, id, and nodes. Returns a
  struct of type org-roam-desktop-collection with corresponding
  values."
  (ord--collection-from-plist (json-parse-string collection-json
                                                 :object-type
                                                 'plist)))

;; (ord--collection-from-json (ord--collection-to-json test-collection))
  
(defun ord--choose-collection-by-name ()
  "To be passed to interactive form: lets the user choose from among
the names of collections currently in
  list-of-org-roam-desktop-collections, and then returns the full
  collection of the name selected."
  (let ((extended-list
         (seq-map (lambda (collection)
                    (cons
                     (org-roam-desktop-collection-name collection)
                     collection))
                  list-of-org-roam-desktop-collections)))
    (cdr (assoc (completing-read "Choose collection: " extended-list)
                extended-list))))

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

(defun magit-section-mode-buffer-setup (buffer-title collection-name display-func)
  "Wrapper function for displaying sections in a buffer whose mode is
derived from magit-section. BUFFER-TITLE is the title of the buffer. The
  buffer is erased, a root magit-section is added and
  DISPLAY-FUNC is called inside of the root section. Then
  switch-buffer-other-window is called to show the buffer."
  (let ((buffer (get-buffer-create buffer-title)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-desktop-mode)        
        (org-roam-buffer-set-header-line-format collection-name)
        (magit-insert-section (root)
          (magit-insert-heading)
          (funcall display-func))))
    (switch-to-buffer-other-window buffer)))

(defun ord-mode-entry-section-function-default (node)
      (insert (concat (prin1-to-string node) "\n")))

(setq ord-mode-entry-section-functions (list 'ord-mode-entry-section-function-default))

(setq ord-mode-entry-section-functions
      (list (lambda (node)
              (org-roam-backlinks-section node
                                          :section-title " Back links"
                                          :show-backlink-p 'jb/filter-org-roam-backlink--for-no-orls-or-reading
                                          :unique t))
            (lambda (node)
              (org-roam-backlinks-section node
                                          :section-title " Reading back links:"
                                          :show-backlink-p 'jb/filter-org-roam-backlink--for-reading
                                          :unique t))
            (lambda (node)
              (org-roam-backlinks-section node
                                          :section-title " ORL back links:"
                                          :show-backlink-p 'jb/filter-org-roam-backlink--for-orl
                                          :unique t))))            

;; test-collection
;; (setq test-buffer-name (ord--buffer-name-for-collection
;;                         test-collection))
;; (get-buffer test-buffer-name)
;; (ord--create-and-render-collection-view test-collection)
;; (switch-to-buffer-other-window test-buffer-name)

;; (setq test-node-ids (org-roam-desktop-collection-nodes
;;                      test-collection))
;; (seq-do
;;  (lambda (node-id)
;;    (let ((node (org-roam-node-from-id node-id)))     
;;        (message (org-roam-node-title node))))
;;  test-node-ids)

;; (setq test-node (org-roam-node-from-id (elt test-node-ids 2)))
;; (org-roam-node-title test-node)

(defun ord--create-and-render-collection-view (collection)
  (let ((node-ids (org-roam-desktop-collection-nodes collection)))    
    (magit-section-mode-buffer-setup
     (ord--buffer-name-for-collection collection)
     (org-roam-desktop-collection-name collection)     
     (lambda ()
       (setq-local ord-buffer-current-collection collection)
       (seq-do
        (lambda (node-id)
          (when-let ((node (org-roam-node-from-id node-id)))
            (magit-insert-section section (org-roam-node-section)              
              (insert (concat (propertize (org-roam-node-title node)
                                          'font-lock-face 'org-roam-title)))
              (magit-insert-heading)
              (oset section node node)              
              (seq-do
               (lambda (func)
                 (funcall func node))
               ord-mode-entry-section-functions)
              (insert "\n"))))
        node-ids)))))

(defun ord-view-collection (collection)
  (interactive (list
                (ord--choose-collection-by-name)))
  (let ((buffer-name (ord--buffer-name-for-collection collection)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (ord--create-and-render-collection-view collection))))
   
(defun ord-refresh-view ()
  (interactive)
  "Refresh the contents of the currently selected org-roam-desktop
  buffer."
  (unless (not (derived-mode-p 'org-roam-desktop-mode))
    (ord--create-and-render-collection-view ord-buffer-current-collection)))

;;; save, close, and load collections

(defun ord-save-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name)))
  (let
      ((file-name (read-file-name
                   "Save collection as: "
                   (file-name-concat org-roam-directory org-roam-desktop-dir)
                   nil nil
                   (ord--default-file-name-for-collection collection)))
       (json-str (ord--collection-to-json collection)))
    (unless (file-directory-p (file-name-directory file-name))
      (make-directory (file-name-directory file-name) t))
    (with-temp-file file-name
      (insert json-str))))

(defun ord-close-collection (collection-to-remove)
  (setq list-of-org-roam-desktop-collections
        (seq-remove (lambda (collection)
                      (eq collection collection-to-remove))
                    list-of-org-roam-desktop-collections)))

(defun ord-save-and-close-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name)))
  (ord-save-collection collection)
  (ord-close-collection collection))

(defun ord-load-collection ()
  (interactive)
  (let ((file-name
         (read-file-name
          "Find collection: "
          (file-name-concat org-roam-directory org-roam-desktop-dir)
          nil
          t)))
    (with-temp-buffer
      (insert-file-contents file-name)
      (add-to-list
       'list-of-org-roam-desktop-collections
       (ord--collection-from-json (buffer-substring-no-properties
                                   (point-min) (point-max)))))))

;;; org-roam-desktop-top map, to be used anywhere in emacs
(define-prefix-command 'org-roam-desktop-map)

(global-set-key (kbd "M-d") org-roam-desktop-map)
(define-key org-roam-desktop-map (kbd "M-c")
            #'org-roam-desktop-create-collection)
(define-key org-roam-desktop-map (kbd "M-a")
            #'org-roam-desktop-add-node-at-point)
(define-key org-roam-desktop-map (kbd "M-v")
            #'ord-view-collection)
(define-key org-roam-desktop-map (kbd "M-s") #'ord-save-collection)
(define-key org-roam-desktop-map (kbd "M-S") #'ord-save-and-close-collection)
(define-key org-roam-desktop-map (kbd "M-l") #'ord-load-collection)

;;; org-roam-desktop-mode-map, used in ord-mode buffers that display collections
(define-key org-roam-desktop-mode-map (kbd "g") #'ord-refresh-view)

 ;; (define-minor-mode org-roam-desktop-minor-mode
 ;;  "Global minor mode to add nodes to org-roam-desktop collections."
 ;;  :lighter " or-desktop"
 ;;  :keymap org-roam-desktop-mode-map
 ;;  :global t
 ;;  :group 'org-roam-desktop)
  
(provide 'org-roam-desktop)
