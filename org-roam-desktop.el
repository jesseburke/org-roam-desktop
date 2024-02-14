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
  :prefix "ord-"
  :link '(url-link :tag "Github" "https://github.com/jesseburke/org-roam-desktop"))

(defcustom ord-save-dir "ord-collections/"
  "Default directory to save/look for collection files."
  :group 'org-roam-desktop
  :type 'string)

(defcustom ord-mode-entry-section-functions
  (list 'ord-mode-entry-section-function-default)
  "Function that draws the section for each entry in a collection."
  :group 'org-roam-desktop
  :type 'function)

(defun ord--default-sort-function (first second)
  (not (string< (upcase (org-roam-node-title
                         first))
                (upcase (org-roam-node-title second)))))

(defcustom ord-mode-sort-function  
    'ord--default-sort-function
  "Function that sorts the entries in a collection, before being
  displayed in an ord-mode buffer."
  :group 'org-roam-desktop
  :type 'function)

(defface ord-entry-title
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

(cl-defstruct ord-collection
  name id nodes)

(defvar ord-collection-list '() "Global list of `loaded` collections.")

(defvar ord-buffer-current-collection nil
  "A buffer local variable: the collection which an a buffer in
   ord-mode is displaying.")
(put 'ord-buffer-current-collection 'permanent-local t)

;;; basic functions for collections
(defun ord-create-collection (collection-name)
  (interactive (list (read-string "name of new collection: " "")))
  (let ((new-collection
         (make-ord-collection :name collection-name
                                     :id (concat "ord-" (org-id-uuid))
                                     :nodes '())))
    (add-to-list 'ord-collection-list new-collection)
    new-collection))

(cl-defun ord--choose-collection (&optional force-prompt require-match
  (prompt-string "Choose collection: "))
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
                          (ord-collection-name collection)
                          collection))
                       ord-collection-list))
             (read-text (completing-read prompt-string
                                         extended-list nil require-match)))
             (if-let ((chosen-collection (cdr (assoc read-text
                                                     extended-list))))
                 chosen-collection
               (ord-create-collection read-text))))))

(defun ord--add-node-ids-to-collection (node-ids collection)
  "NODE-IDS is a list "
  (setf
   (ord-collection-nodes collection)
   (cl-delete-duplicates (append (ord-collection-nodes collection)
                                  node-ids) :test 'equal)))

(defun ord--delete-node-id-from-collection (node-id-to-delete collection)
  (let* ((id-list (ord-collection-nodes collection))
         (new-id-list
          (seq-filter (lambda (node-id)
                        (not (string= node-id node-id-to-delete)))
                      id-list)))
    (setf (ord-collection-nodes
           ord-buffer-current-collection) new-id-list)))

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

(defun ord-choose-node-id ()  
  (let ((node (org-roam-node-read nil nil nil t)))
    (list (org-roam-node-id node))))

(defun ord--node-ids-at-point ()
  "If region is active, and there are links in the region, returns ids
  of links in region; else, if point is on an org-roam link, then
  return that id; else, if in a magit-section buffer where
  org-roam-node-at-point returns non-nil, returns id of entry; else,
  prompts user to chooes an entry, and returns id of that."
  (if (and (use-region-p) (ord--links-in-region))
      (ord--links-in-region)
    (if (ord--get-id-of-id-link)
        (list (ord--get-id-of-id-link))
      (if (org-roam-node-at-point)
          (list (org-roam-node-id (org-roam-node-at-point)))
        (ord-choose-node-id)))))


;;; translate collection repn between class, plist, and json
(defun ord--collection-to-plist (collection)
  "COLLECTION should be an ord-collection struct. Returns a
  plist representing that struct."
  (list :name (ord-collection-name collection)
        :id (ord-collection-id collection)
        :nodes (vconcat (seq-filter (lambda (id) id) (ord-collection-nodes
                                                      collection)))))

(defun ord--collection-from-plist (collection-plist)
  "COLLECTION-PLIST should have keys :name, :id, and :nodes. Returns a
  struct of type org-roam-desktop-collection."
  (cl-destructuring-bind (&key name id nodes) collection-plist
    (make-ord-collection :name name :id id :nodes (mapcar 'identity nodes))))

;; (ord--collection-from-plist (ord--collection-to-plist test-collection))

(defun ord--collection-to-json (collection)
  "COLLECTION should be an ord-collection struct. Returns a
  json string representing that struct."
  (json-serialize (ord--collection-to-plist collection)))

;; (ord--collection-to-json test-collection)

(defun ord--collection-from-json (collection-json)
  "COLLECTION-JSON should be a string of json, with keys name, id, and nodes. Returns a
  struct of type ord-collection with corresponding
  values."
  (ord--collection-from-plist (json-parse-string collection-json
                                                 :object-type
                                                 'plist)))

;; (ord--collection-from-json (ord--collection-to-json test-collection))

;;; save, close, and load collections

(defun ord--default-file-name-for-collection (collection)
  "The name of a file for a collection is its name plus json file extension."
  (concat   
   (ord-collection-name collection)   
   ".json"))

(defun ord-save-collection (collection)
  (interactive
   (list
    (ord--choose-collection)))
  (let
      ((file-name (read-file-name
                   "Save collection as: "
                   (file-name-concat org-roam-directory ord-save-dir)
                   nil nil
                   (ord--default-file-name-for-collection collection)))
       (json-str (ord--collection-to-json collection)))
    (unless (file-directory-p (file-name-directory file-name))
      (make-directory (file-name-directory file-name) t))
    (with-temp-file file-name
      (insert json-str))))

(defun ord-close-collection (collection-to-remove)
  (interactive (list (ord--choose-collection nil t "Collection to close?: ")))
  (setq ord-collection-list
        (seq-remove (lambda (collection)
                      (eq collection collection-to-remove))
                    ord-collection-list)))

(defun ord-load-collection ()
  (interactive)
  (let ((file-name
         (read-file-name
          "Find collection: "
          (file-name-concat org-roam-directory ord-save-dir)
          nil
          t)))
    (with-temp-buffer
      (insert-file-contents file-name)
      (add-to-list
       'ord-collection-list
       (ord--collection-from-json (buffer-substring-no-properties
                                   (point-min) (point-max)))))))
  
;;; viewing a collection

(define-derived-mode ord-mode magit-section-mode "OrgRoamDesktop"
  "Major mode for displaying collection of org-roam nodes.")

(defclass ord-node-section (org-roam-node-section)
  ((keymap :initform 'ord-mode-map))
  "An `org-roam-node-section' based class, changing the initial keymap
  of the former.")

(cl-defun ord-node-insert-section (&key node (fill-column-number 2))
  "Insert section for a link from SOURCE-NODE to some other node.
The other node is normally `org-roam-buffer-current-node'.

SOURCE-NODE is an `org-roam-node' that links or references with
the other node.

POINT is a character position where the link is located in
SOURCE-NODE's file.

PROPERTIES (a plist) contains additional information about the
link.

Despite the name, this function actually inserts 2 sections at
the same time:

1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE. Acts as a parent section of the following one.

2. `ord-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT. Acts a child section of the previous
   one."
  (magit-insert-section section (ord-node-section nil t)
    (insert (concat (propertize (concat "  " (org-roam-node-title node))
                                'font-lock-face 'ord-entry-title)))
    (magit-insert-heading)
    (oset section node node)
    (magit-insert-section section (ord-preview-section nil t)
      (let* ((fill-prefix (make-string fill-column-number ?/ ))
            (filled-string
             (with-temp-buffer
               (insert (ord-preview-get-contents (org-roam-node-file node)) "\n")
               (indent-region (point-min) (point-max) fill-column-number)
               (buffer-string))))
      (insert (org-roam-fontify-like-in-org-mode filled-string))
      (oset section file (org-roam-node-file node))
      (insert ?\n)))))

;;;; Preview
(define-prefix-command 'ord-preview-map)
(set-keymap-parent 'ord-preview-map ord-mode-map)
(define-key ord-preview-map (kbd "<RET>") 'ord-preview-visit)
  
(defclass ord-preview-section (magit-section)
  ((keymap :initform 'ord-preview-map)
   (file :initform nil)
   (point :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, and the link as at POINT.")

(defun ord--start-of-file-node ()
  (save-excursion
    (goto-char (point-min))
    (goto-char (cdr (org-get-property-block)))
    (next-line)
    (next-line)
    (point)))

(defun ord-preview-visit (&optional not-other-window)
  "Visit the node whose preview section point is in; return the
visited buffer. With OTHER-WINDOW non-nil do so in another
window. In interactive calls OTHER-WINDOW is set with
`universal-argument'."
  (interactive (list current-prefix-arg))
  (if (org-in-regexp org-link-bracket-re 1)
      (org-open-at-point)
    (let* ((relative-point (- (point) (marker-position (oref
                                                        (magit-section-at)
                                                        start))))
           (file (oref (magit-section-at) file))
           (buf (find-file-noselect file))
           (display-buffer-fn (if not-other-window                               
                                  #'pop-to-buffer-same-window #'switch-to-buffer-other-window)))
      (funcall display-buffer-fn buf)
      (with-current-buffer buf
        (widen)
        (goto-char (+ relative-point (ord--start-of-file-node)))
        (when (org-invisible-p) (org-show-context))
        buf))))

(defun ord-preview-default-display-function ()
  "Return the preview content at point.

This function returns the all contents under the current
headline, up to the next headline."
  (let ((beg (save-excursion
               (org-roam-end-of-meta-data t)
               (point)))
        (end (save-excursion
               (org-next-visible-heading 1)
               (point))))
    (string-trim (buffer-substring-no-properties beg end))))

(defun ord-preview-full-display-function ()
  "Return the preview content at point.

This function returns the entire body of the entry."
  (let ((beg (save-excursion
               (org-roam-end-of-meta-data t)
               (point)))
        (end (point-max)))
    (string-trim (buffer-substring-no-properties beg end))))

(defcustom ord-preview-display-function #'ord-preview-default-display-function
  "The preview function to use to populate the Org-roam buffer.

The function takes no arguments, but the point is temporarily set
to the exact location of the backlink."
  :group 'org-roam
  :type 'function)

(setq ord-preview-display-function #'ord-preview-default-display-function)

(defun ord-preview-get-contents (file)
  "Get preview content for FILE."
  (save-excursion
    (org-roam-with-temp-buffer file
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((s (funcall ord-preview-display-function)))
         (dolist (fn org-roam-preview-postprocess-functions)
           (setq s (funcall fn s)))
         s)))))

;;;; setting up the buffer
(defun ord--buffer-name-for-collection (collection)
  "The name of a buffer for a collection starts with `*ord
collection: ` plus collection name and the
first 6 digits of its id."
  (concat
   "*ord collection: "
   (ord-collection-name collection)
   " ("
   (substring (ord-collection-id collection) 5 11)
   ")"))

(defun ord--node-id-list-to-node-list (id-list)
  "ID-LIST is a sequence of id's; returns a list of nodes corresponding
to the ids."
  (let ((node-list ()))
    (seq-do (lambda (node-id)
              (when-let ((node (org-roam-node-from-id node-id)))
                (push node node-list))) id-list)
    node-list))

(defun ord-mode-entry-section-function-default (node)
  (ord-node-insert-section
   :node node))

(setq ord-mode-entry-section-functions (list             
                                        'ord-mode-entry-section-function-default))

(defvar ord--node-to-position-plist '()
  "A buffer local variable: stores where the section of a given node
  starts in the current buffer.")
(put 'ord--node-to-position-plist 'permanent-local t)

(defun ord--render-collection-view ()
  (setq-local ord--node-to-position-plist '())
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ord-mode)
    (org-roam-buffer-set-header-line-format
     (ord-collection-name ord-buffer-current-collection))
    (if-let ((node-ids (ord-collection-nodes
                        ord-buffer-current-collection)))
        (let* ((node-list (ord--node-id-list-to-node-list node-ids))
               (sorted-node-list (sort
                                  node-list
                                  ord-mode-sort-function)))
          (magit-insert-section (root)
            (magit-insert-heading)      
            (seq-do
             (lambda (node)
               (setq-local ord--node-to-position-plist
                     (plist-put ord--node-to-position-plist
                          (org-roam-node-id node) (point)))
               (seq-do
                (lambda (func)
                  (funcall func node))
                ord-mode-entry-section-functions))
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
                (ord--choose-collection)))
  (let ((buffer-name (ord--buffer-name-for-collection collection)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (ord--create-and-display-collection-view collection))))


;;; turn collection into org-mode buffer

(defun ord-export-collection-to-org-buffer (collection)
  "Creates an org-mode buffer displaying COLLECTION. Will be one top level
entry, whose heading is the name of the section. Then a subentry
  for each node in COLLECTION. For the subentries, the headline
  is the node title and the body is the preview section text."
  (interactive
   (list
    (ord--choose-collection)))  
  (let* ((buffer-name (concat (ord--buffer-name-for-collection
                               collection) ".org"))
         (buffer (get-buffer-create buffer-name))
         ;; need nodes, as opposed to ids, to get names, to sort.
         (node-list (ord--node-id-list-to-node-list
                     (ord-collection-nodes collection))) 
         (sorted-node-list (sort
                            node-list
                            ord-mode-sort-function)))
    (with-current-buffer buffer
      (setq-local ord-buffer-current-collection collection)
      (erase-buffer)
      (org-mode)
      (org-insert-heading)
      (insert (ord-collection-name collection))
      (newline)
      (let ((line-to-delete (make-marker)))
        (set-marker line-to-delete (point))
        (org-insert-subheading 1)        
        (seq-do
         (lambda (node)
           (newline)
           (org-insert-heading)
           (insert
            (org-link-make-string
             (concat "id:" (org-roam-node-id node))
             (org-roam-node-title node)))
           (newline 2)
           (insert (ord-preview-get-contents
                    (org-roam-node-file node)) "\n")
           (newline))
         sorted-node-list)
        (goto-char (marker-position line-to-delete))
        (kill-line))
      (switch-to-buffer-other-window buffer))))

(defun ord-export-collection-to-org-list (collection)
  (interactive
   (list
    (ord--choose-collection)))
  (let* ((buffer-name (concat (ord--buffer-name-for-collection
                               collection) "-list" ".org"))
         (buffer (get-buffer-create buffer-name))
          ;; need nodes, as opposed to ids, to get names, to sort.
         (node-list (ord--node-id-list-to-node-list
                     (ord-collection-nodes collection))) 
         (sorted-node-list (sort
                            node-list
                            ord-mode-sort-function)))
    (with-current-buffer buffer
      (setq-local ord-buffer-current-collection collection)
      (erase-buffer)
      (org-mode)
      (let ((first-node (car sorted-node-list)))
        (insert
         (concat "- "
                 (org-link-make-string
                  (concat "id:" (org-roam-node-id first-node))
                  (org-roam-node-title first-node)))))
      (newline)
      (seq-do
       (lambda (node)         
         (org-insert-item)
          (insert
            (org-link-make-string
             (concat "id:" (org-roam-node-id node))
             (org-roam-node-title node)))         
           (newline))
         (cdr sorted-node-list)))
    (switch-to-buffer-other-window buffer)))

   
;;; ord-mode-map, used in ord-mode buffers that display
;;; collections

(defun ord-mode-refresh-view ()
  (interactive)
  "Refresh the contents of the currently selected org-roam-desktop
  buffer."
  (unless (not (derived-mode-p 'ord-mode))
    (save-mark-and-excursion
        (magit-section-cache-visibility (magit-current-section))
        (ord--render-collection-view))))

(defun ord-add-node-at-point (collection)
  (interactive (list (ord--choose-collection)))
  (ord--add-node-ids-to-collection (ord--node-ids-at-point) collection)
  (ord-mode-refresh-view))

(defun ord-mode-delete-entry ()
  (interactive)
  "Delete the entry at point from collection."
  (ord--delete-node-id-from-collection (car (ord--node-ids-at-point))
                                       ord-buffer-current-collection)
  (ord-mode-refresh-view))

(defun ord-mode-choose-entry-from-collection (collection)
  "User can select entry from current collection; after selection
  point is moved there."
  (interactive (list (ord--choose-collection)))
  (let* ((node-ids (ord-collection-nodes collection))
         (node-ids-and-names (seq-map
                              (lambda (node-id)
                                (let ((title (org-roam-node-title
                                              (org-roam-node-from-id node-id))))
                                  (cons title node-id)))
                              node-ids))
         (selected-name (completing-read "Choose entry: "
                                  node-ids-and-names nil
                                  'require-match)))   
    (if-let ((selected-id (cdr (assoc selected-name
                                      node-ids-and-names))))
        (goto-char (plist-get ord--node-to-position-plist selected-id)))))

(defun ord-mode-save-collection ()
  (interactive)
  "Save the current collection."
  (ord-save-collection ord-buffer-current-collection))

(defun ord-mode-show-org-roam-buffer ()
  (interactive)
  "Show the org-roam-mode buffer for entry point is on."
  (org-roam-buffer-display-dedicated (org-roam-node-at-point)))

(defun ord-choose-and-add-node (collection)
  (interactive (list (ord--choose-collection)))
  (let* ((node (org-roam-node-read "Entry to add: " nil nil t))
         (node-id (org-roam-node-id node)))
    (ord--add-node-ids-to-collection (list node-id))))

(defun ord-close-collection-and-buffer ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to close this collection?")
    (let ((inhibit-read-only t))
      (ord-close-collection ord-buffer-current-collection)
      (kill-this-buffer))))

(defun ord-view-other-collection ()
  (interactive)
  (let ((collection (ord--choose-collection t)))
    (ord-view-collection collection)))

(define-key ord-mode-map (kbd "g")
            #'ord-mode-refresh-view)
(define-key ord-preview-map [remap org-roam-buffer-refresh] #'ord-mode-refresh-view)
(define-key ord-mode-map (kbd "k")
            #'ord-mode-delete-entry)
(define-key ord-mode-map (kbd "a") #'ord-add-node-at-point)
(define-key ord-mode-map (kbd "s")
            #'ord-mode-save-collection)
(define-key ord-mode-map (kbd "b")
            #'ord-mode-show-org-roam-buffer)
(define-key ord-mode-map (kbd "<RET>")
            (lambda () (interactive)
              (org-roam-node-visit (org-roam-node-from-id (car (ord--node-ids-at-point))) t)))
(define-key ord-mode-map (kbd "t")            
            (lambda () (interactive)
              (other-tab-prefix)
              (org-roam-node-visit (org-roam-node-from-id (car
                                                           (ord--node-ids-at-point))))))
(define-key ord-mode-map (kbd "e") #'ord-mode-choose-entry-from-collection)
(define-key ord-mode-map (kbd "o")
            #'ord-export-collection-to-org-buffer)
(define-key ord-mode-map (kbd "w") #'ord-close-collection-and-buffer)
(define-key ord-mode-map (kbd "v") #'ord-view-other-collection)


;;; ord-map, to be used anywhere in emacs
(define-prefix-command 'ord-map)

(defun ord-close-all-collections ()
  (interactive)
  (when (y-or-n-p (format "Close all %d collections?" (length ord-collection-list)))
    (setq ord-collection-list ())))

(global-set-key (kbd "M-d") #'ord-map)
(define-key ord-map (kbd "M-c")
            #'ord-create-collection)
(define-key ord-map (kbd "M-a")
            #'ord-add-node-at-point)
(define-key ord-map (kbd "M-v")
            #'ord-view-collection)
(define-key ord-map (kbd "M-s") #'ord-save-collection)
(define-key ord-map (kbd "M-l") #'ord-load-collection)
(define-key ord-map (kbd "M-k") #'ord-close-all-collections)

(provide 'org-roam-desktop)

