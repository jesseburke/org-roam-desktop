;;; -*- lexical-binding: t; -*-
;;; org-roam-desktop.el --- tool for inspection and revision of an org-roam zettlekasten.

;; Author: Jesse Burke <jtb445@gmail.com>
;;
;; URL: https://github.com/jesseburke/org-roam-desktop
;; Keywords: org-mode, roam, convenience, 
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
(require 'cl-lib)

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

(defvar ord-collection-list '())

(defclass ord-node-section (org-roam-node-section)
  ((keymap :initform 'org-roam-desktop-mode-map))
  "An `org-roam-node-section' based class, changing the initial keymap
  of the former.")

(defvar ord-buffer-current-collection nil
  "A buffer local variable: the collection which an a buffer in
   ord-mode is displaying.")
(put 'ord-buffer-current-collection 'permanent-local t)

;;; basic functions for collections
(defun ord-create-collection (collection-name)
  (interactive (list (read-string "name of new collection: " "")))
  (let ((new-collection
         (make-org-roam-desktop-collection :name collection-name
                                     :id (concat "ord-" (org-id-uuid))
                                     :nodes [])))
    (add-to-list 'ord-collection-list new-collection)
    new-collection))

(defun ord--close-collection (collection-to-remove)
  (setq ord-collection-list
        (seq-remove (lambda (collection)
                      (eq collection collection-to-remove))
                    ord-collection-list)))

(defun ord--choose-collection-by-name (&optional force-prompt require-match)
  "To be passed to interactive form, to choose a collection: if there
  is only one collection loaded, then that is it; otherwise, if
  the buffer-local variable ORD-BUFFER-CURRENT-COLLECTION is
  non-nil, use that (it will be in org-mode buffers); otherwise,
  the user is prompted to choose from among the names of
  collections currently loaded. If optional argument
  FORCE-PROMPT is true, then prompt the user no matter what."
  (if current-prefix-arg (setq force-prompt t))
  (if (and (not force-prompt) (= (length ord-collection-list) 1))
      (car ord-collection-list)
    (if (and (not force-prompt) ord-buffer-current-collection)  ord-buffer-current-collection
      (let* ((extended-list
              (seq-map (lambda (collection)
                         (cons
                          (org-roam-desktop-collection-name collection)
                          collection))
                       ord-collection-list))
             (read-text (completing-read "Choose collection: "
                                         extended-list nil require-match)))
             (if-let ((chosen-collection (cdr (assoc read-text
                                                     extended-list))))
                 chosen-collection
               (ord-create-collection read-text))))))

(defun ord--add-node-ids-to-collection (node-ids collection)  
  (setf
   (org-roam-desktop-collection-nodes collection)
   (cl-delete-duplicates (vconcat (org-roam-desktop-collection-nodes collection)
                                  node-ids) :test 'equal)))

(defun ord--delete-node-id-from-collection (node-id-to-delete collection)
  (let* ((id-array (org-roam-desktop-collection-nodes collection))
         (new-id-array
          (seq-filter (lambda (node-id)
                        (not (string= node-id node-id-to-delete)))
                      id-array)))
    (setf (org-roam-desktop-collection-nodes
           ord-buffer-current-collection) new-id-array)))

(defun ord--get-link-address ()
  "When point is at an org-link, returns the link address as string."
  (when (org-in-regexp org-link-bracket-re 1)
    ;; We do have a link at point. 
    (match-string-no-properties 1)))

(defun ord--get-id-of-id-link ()
  (save-excursion
    (when-let ((link-address (ord--get-link-address)))
      (if (string= (substring link-address 0 2) "id")
          (substring link-address 3)))))

(defun ord--links-in-region () 
  (let ((min-marker (make-marker))
        (max-marker (make-marker))
        (return-list ()))
    (when (use-region-p)
      (set-marker min-marker (region-beginning))
      (set-marker max-marker (region-end))
      (save-mark-and-excursion 
        (goto-char (marker-position min-marker))
        (let ((old-point (point)))
          (org-next-link)
          (while (and (< old-point (point)) (< (point) (marker-position max-marker)))
            (setq old-point (point)) 
            (when-let ((id (ord--get-id-of-id-link)))
              (push id return-list))
            (org-next-link))))
      return-list)))

(defun ord--node-ids-at-point ()
  "If region is active, then return id of all links in region. point is on an org-roam link, then return id of that entry;
otherwise, if in org-roam, org-roam-mode, or org-roam-desktop
buffer, then return id of entry point is on."
  (if (and (use-region-p) (ord--links-in-region))
      (ord--links-in-region)
    (let ((id (or (ord--get-id-of-id-link)
                  (org-roam-node-id (org-roam-node-at-point)))))
      (list id))))

;;; translate collection repn between class, plist, and json
(defun ord--collection-to-plist (collection)
  "COLLECTION should be an org-roam-desktop-collection struct. Returns a
  plist representing that struct."
  (list :name (org-roam-desktop-collection-name collection)
        :id (org-roam-desktop-collection-id collection)
        :nodes (vconcat (seq-filter (lambda (id) id) (org-roam-desktop-collection-nodes
                                                      collection)))))

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

;;; save, close, and load collections

(defun ord--default-file-name-for-collection (collection)
  "The name of a file for a collection is its name plus json file extension."
  (concat   
   (org-roam-desktop-collection-name collection)   
   ".json"))

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

(defun ord-save-and-close-collection (collection)
  (interactive
   (list
    (ord--choose-collection-by-name)))
  (ord-save-collection collection)
  (ord--close-collection collection))

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
       'ord-collection-list
       (ord--collection-from-json (buffer-substring-no-properties
                                   (point-min) (point-max)))))))
  
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

(defun ord-mode-entry-section-function-default (node)
  (insert (concat (prin1-to-string node) "\n")))

(setq ord-mode-entry-section-functions (list
                                        'ord-mode-entry-section-function-default))

(defun ord--node-id-list-to-node-list (id-list)
  "Returns list of nodes, sorted by SORT-FN."
  (let ((node-list ()))
    (seq-do (lambda (node-id)
              (when-let ((node (org-roam-node-from-id node-id)))
                (push node node-list))) id-list)
    node-list))


(defun ord--render-collection-view ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-desktop-mode)
    (org-roam-buffer-set-header-line-format
     (org-roam-desktop-collection-name ord-buffer-current-collection))
    (if-let ((node-ids (org-roam-desktop-collection-nodes
                        ord-buffer-current-collection)))
        (let* ((node-list (ord--node-id-list-to-node-list node-ids))
               (sorted-node-list (sort
                                  node-list
                                  (lambda (first second)
                                    (string< (upcase (org-roam-node-title
                                              first))
                                             (upcase (org-roam-node-title second)))))))
          (magit-insert-section (root)
            (magit-insert-heading)      
            (seq-do
             (lambda (node)
               (magit-insert-section section (ord-node-section nil t)
                 (insert (concat (propertize (org-roam-node-title node)
                                             'font-lock-face 'org-roam-title)))
                 (magit-insert-heading)
                 (oset section node node)              
                 (seq-do
                  (lambda (func)
                    (funcall func node))
                  ord-mode-entry-section-functions)
                 (insert "\n")))
             sorted-node-list))))))

(defun ord--create-and-display-collection-view (collection)
  (let* ((buffer-name (ord--buffer-name-for-collection collection))
         (buffer (get-buffer-create buffer-name)))        
    (with-current-buffer buffer      
      (setq-local ord-buffer-current-collection collection)
      (ord--render-collection-view))
    (switch-to-buffer-other-window buffer)))

(defun ord-view-collection (collection)
  (interactive (list
                (ord--choose-collection-by-name)))
  (let ((buffer-name (ord--buffer-name-for-collection collection)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (ord--create-and-display-collection-view collection))))
   
;;; org-roam-desktop-mode-map, used in ord-mode buffers that display
;;; collections

(defun ord-mode-refresh-view ()
  (interactive)
  "Refresh the contents of the currently selected org-roam-desktop
  buffer."
  (unless (not (derived-mode-p 'org-roam-desktop-mode))
    (save-excursion
      (ord--render-collection-view))))

(defun ord-add-node-at-point (collection)
  (interactive (list (ord--choose-collection-by-name)))
  (ord--add-node-ids-to-collection (ord--node-ids-at-point) collection)
  (ord-mode-refresh-view))

(defun ord-mode-delete-entry ()
  (interactive)
  "Delete the entry at point from collection."
  (ord--delete-node-id-from-collection (car (ord--node-ids-at-point))
                                       ord-buffer-current-collection)
  (ord-mode-refresh-view))

(defun ord-mode-add-node ()
  (interactive)
  "Add the entry at point to current collection."
  (ord-add-node-at-point ord-buffer-current-collection)
  (ord-mode-refresh-view))

(defun ord-mode-save-collection ()
  (interactive)
  "Save the current collection."
  (ord-save-collection ord-buffer-current-collection))

(defun ord-mode-show-org-roam-buffer ()
  (interactive)
  "Show the org-roam-mode buffer for entry point is on."
  (org-roam-buffer-display-dedicated (org-roam-node-at-point)))

(define-key org-roam-desktop-mode-map (kbd "g")
            #'ord-mode-refresh-view)
(define-key org-roam-preview-map [remap org-roam-buffer-refresh] #'ord-mode-refresh-view)
(define-key org-roam-desktop-mode-map (kbd "k")
            #'ord-mode-delete-entry)
(define-key org-roam-desktop-mode-map (kbd "a") #'ord-add-node-at-point)
(define-key org-roam-desktop-mode-map (kbd "s")
            #'ord-mode-save-collection)
(define-key org-roam-desktop-mode-map (kbd "b")
            #'ord-mode-show-org-roam-buffer)
(define-key org-roam-desktop-mode-map (kbd "<RET>")
            (lambda () (interactive)
              (org-roam-node-visit (org-roam-node-from-id (car (ord--node-ids-at-point))))))

;;; org-roam-desktop-top map, to be used anywhere in emacs
(define-prefix-command 'org-roam-desktop-map)

(defun ord-close-all-collections ()
  (interactive)
  (when (y-or-n-p (format "Close all %d collections?" (length ord-collection-list)))
    (setq ord-collection-list ())))

(define-key org-roam-desktop-map (kbd "M-c")
            #'ord-create-collection)
(define-key org-roam-desktop-map (kbd "M-a")
            #'ord-add-node-at-point)
(define-key org-roam-desktop-map (kbd "M-v")
            #'ord-view-collection)
(define-key org-roam-desktop-map (kbd "M-s") #'ord-save-collection)
(define-key org-roam-desktop-map (kbd "M-S") #'ord-save-and-close-collection)
(define-key org-roam-desktop-map (kbd "M-l") #'ord-load-collection)
(define-key org-roam-desktop-map (kbd "M-k") #'ord-close-all-collections)

;; (define-minor-mode org-roam-desktop-minor-mode
;;  "Global minor mode to add nodes to org-roam-desktop collections."
;;  :lighter " or-desktop"
;;  :keymap org-roam-desktop-mode-map
;;  :global t
;;  :group 'org-roam-desktop)

(provide 'org-roam-desktop)


