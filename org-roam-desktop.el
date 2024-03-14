;; -*- lexical-binding: t; -*-
;; org-roam-desktop.el --- tool for inspection and revision of an org-roam zettlekasten.

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

(defun ord--default-sort-by-id-function (first-id second-id)
  (if-let (first-name (ord--node-name-from-id first-id))
      (if-let (second-name (ord--node-name-from-id second-id))
          (string< (upcase first-name) (upcase second-name)))))

(defcustom ord-sort-by-id-function  
  'ord--default-sort-by-id-function
  "Function that sorts the entries in a collection, before being
  displayed in an ord-mode buffer. The function should accept two
  node-ids, and return true if the first should come before the
  second, and nil otherwise."
  :group 'org-roam-desktop
  :type 'function)

(setq ord-sort-by-id-function 'ord--default-sort-by-id-function)

(defface ord-entry-title-face
  '((t :weight bold))
  "Face for Org-roam titles."
  :group 'org-roam-faces)

(cl-defstruct ord-collection  
  name id node-ids marked-node-ids history-stack)

(defvar ord-collection-list '() "Global list of `loaded` collections.")

;;; org(-roam) utility functions

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

(defun ord--node-ids-at-point (&optional force-prompt)
  "If region is active, and there are links in the region, returns ids
  of links in region; else, if point is on an org-roam link, then
  return that id; else, if in a magit-section buffer where
  org-roam-node-at-point returns non-nil, returns id of entry;
  else, prompts user to chooes an org-roam entry, and returns id
  of that. If FORCE-PROMPT is true, then prompt no matter what."
  (if force-prompt
      (ord-choose-node-get-id)
    ;; in following, are checking if there are actually links in the region
    (if (and (use-region-p) (ord--links-in-region)) 
        (ord--links-in-region)
      (if (ord--get-id-of-id-link)
          (list (ord--get-id-of-id-link))
        (if ord-node-id-at-point-function
            (list (funcall ord-node-id-at-point-function))
          (ord-choose-node-get-id))))))

(defun ord--start-of-file-node ()
  "This is an approximation."
  (save-excursion
    (goto-char (point-min))
    (org-fold-hide-drawer-toggle 'off t)
    (goto-char (cdr (org-get-property-block)))
    (next-line)
    (while (looking-at-p org-keyword-regexp)
      (next-line))
    (org-fold--hide-drawers (point-min) (point))
    (point)))

(cl-defun ord--query-forlinks (node-id)
  "Return list of ids of the forward links of NODE-ID."
  (let* ((sql [:select [source dest pos properties]
                       :from links
                       :where (= source $s1)
                       :and (= type "id")])
         (forlinks (org-roam-db-query sql node-id)))
    (cl-loop for forlink in forlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) forlink))
                       dest-id))))

;; (ord--query-forlinks "29235CB5-7D53-4D2B-9112-61A3DCF4A66C")
;; (ord--query-forlinks "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B")

;; following should break/be nil, since node no longer exists
;; (ord--query-forlinks "CF3888AA-2432-4E72-9356-5187B802815D")

;; (cl-defun ord--query-forlinks-of-list (node-ids)
;;   "Return the ids of the forward links for all ids in NODE-IDS."
;;   (let* ((sql [:select [source dest pos properties]
;;                        :from links
;;                        :where (in $s1 source)
;;                        :and (= type "id")])
;;          (forlinks (org-roam-db-query sql node-id)))
;;     (cl-loop for forlink in forlinks
;;              collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) forlink))
;;                        dest-id))))

;; (setq test-list '("29235CB5-7D53-4D2B-9112-61A3DCF4A66C" "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B"))
;; (setq sql-query-str-old (format "SELECT * FROM links WHERE dest IN (%s);"
;;                             (mapconcat (lambda (name) (format "'%s'" name)) test-list ",")))
;; (setq sql-query-str (format "SELECT * FROM links WHERE dest IN (%s);"
;;                             (mapconcat #'identity (mapcar (lambda (name) (format "'%s'" name))
;;                                                           test-list) ",")))

;; (emacsql-with-transaction (org-roam-db)
;;   (emacsql (org-roam-db) [:execute sql-query-str-old]))
;; (emacsql (org-roam-db) sql-query-str)
;; (emacsql (org-roam-db) "SELECT * FROM links")
;; (apply #'emacsql (org-roam-db) sql args)

;; (mapconcat (lambda (node-id) (concat ":or " node-id "\n")) test-list)

;; (setq test-list '("29235CB5-7D53-4D2B-9112-61A3DCF4A66C" "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B"))
;; (setq test-str (mapconcat (lambda (name) (format "'%s'" name)) test-list ","))

;; (ord--query-forlinks-of-list test-str)

;; (ord--query-forlinks "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B")

;; following should break/be nil, since node no longer exists
;; (ord--query-forlinks "CF3888AA-2432-4E72-9356-5187B802815D")

(cl-defun ord--query-backlinks (node-id &key unique)
  "Return the backlinks for NODE-ID.

 When UNIQUE is nil, show all positions where references are found.
 When UNIQUE is t, limit to unique sources."
  (let* ((sql (if unique
                  [:select :distinct [source dest pos properties]
                           :from links
                           :where (= dest $s1)
                           :and (= type "id")
                           :group :by source
                           :having (funcall min pos)]
                [:select [source dest pos properties]
                         :from links
                         :where (= dest $s1)
                         :and (= type "id")]))
         (backlinks (org-roam-db-query sql node-id)))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       source-id))))

;; (ord--query-backlinks "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B")

(defun ord--get-backlink-ids (node-id &optional show-backlink-p)
  "SHOW-BACKLINK-P is a filter function, that takes a node-id and
  returns nil or t."
  (let* ((backlink-ids (ord--query-backlinks node-id :unique t))
         (filtered-backlink-ids
          (seq-filter (lambda (node-id)
                        (when (or (null show-backlink-p)
                                  (and (not (null show-backlink-p))
                                       (funcall show-backlink-p node-id)))
                          node-id))
                      backlink-ids)))
         filtered-backlink-ids))         

;; (ord--get-backlink-ids "8F0AED6C-CA49-4101-B5E7-D5BAA6DB4B7B")

(cl-defun ord--links-in-region (&optional (start (region-beginning)) (end (region-end)))
  (let ((min-marker (make-marker))
        (max-marker (make-marker))
        (return-list ()))
    (set-marker min-marker start)
    (set-marker max-marker end)
    (save-mark-and-excursion 
      (goto-char (marker-position min-marker))
      (let ((old-point (point)))
        (org-next-link)
        (while (and (< old-point (point)) (< (point) (marker-position max-marker)))
          (setq old-point (point)) 
          (when-let ((id (ord--get-id-of-id-link)))
            (push id return-list))
          (org-next-link))))
    return-list))

(defun ord-choose-node-id ()  
  (let ((node (org-roam-node-read nil nil nil t)))
    (list (org-roam-node-id node))))

(defun ord--node-name-from-id (node-id)
  (let* ((sql [:select [title]
                         :from nodes
                         :where (= id $s1)]))
    (car (car (org-roam-db-query sql node-id)))))

;; (ord--node-name-from-id "29235CB5-7D53-4D2B-9112-61A3DCF4A66C")
;; (ord--node-name-from-id "CF3888AA-2432-4E72-9356-5187B802815D")

(defun ord--list-of-node-names ()
  (apply #'append (org-roam-db-query [:select [title] :from nodes])))

;; (ord--list-of-node-names)

(defun ord--file-from-id (node-id)
  (let* ((sql [:select [file]
                         :from nodes
                         :where (= id $s1)]))
    (car (car (org-roam-db-query sql node-id)))))

;; (ord--file-from-id "29235CB5-7D53-4D2B-9112-61A3DCF4A66C")
;; (ord--file-from-id "CF3888AA-2432-4E72-9356-5187B802815D")

;;; basic functions for collections
(defun ord-create-collection (collection-name)
  (interactive (list (read-string "name of new collection: """)))
  (while (string= collection-name "")
    (setq collection-name (read-string "name of new collection (must be non-empty): """)))
  (let ((new-collection
         (make-ord-collection :name collection-name
                              :id (concat "ord-" (org-id-uuid))
                              :node-ids '()
                              :marked-node-ids '()
                              :history-stack '())))
    (add-to-list 'ord-collection-list new-collection)
    new-collection))

(cl-defun ord--choose-collection (&optional force-prompt require-match
                                            (prompt-string "Choose collection: "))
  "To be passed to interactive form, to choose a collection: if there
  is only one collection loaded, then that is it; otherwise, the
  user is prompted to choose from among the names of loaded
  collections. If user enters the name of a collecion that
  doesn't exist (or isn't already loaded), then a new colleciton
  with that name is created. If optional argument FORCE-PROMPT is
  true, then prompt the user no matter what."
  (if current-prefix-arg (setq force-prompt t))
  (if (and (not force-prompt) (= (length ord-collection-list) 1))
      (car ord-collection-list)
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
        (ord-create-collection read-text)))))

(defun ord--add-node-ids-to-collection (node-ids collection)
  "NODE-IDS is a list. Returns nil if no id's were added (i.e., they
all were already in the collection), else returns t."
  (let* ((old-id-list (ord-collection-node-ids collection))         
         (new-id-list (cl-delete-duplicates (append old-id-list
                                                    node-ids) :test 'equal))
         (nonnil-id-list (seq-filter 'ord--node-name-from-id new-id-list))
         (already-there (= (length old-id-list) (length nonnil-id-list))))
    (if already-there nil
      (setf (ord-collection-node-ids collection) nonnil-id-list)
      (push old-id-list (ord-collection-history-stack collection))
      t)))

;; (setq testcollectionname (random-letter-string 10))
;; (setq testcollectiontest (ord-create-collection testcollectionname))

;; (ord--add-node-ids-to-collection '("29235CB5-7D53-4D2B-9112-61A3DCF4A66C"
;; "CF3888AA-2432-4E72-9356-5187B802815D") testcollectiontest)


(defun ord--delete-node-ids-from-collection (node-ids-to-delete collection)
  (let* ((old-id-list (ord-collection-node-ids collection))
         (new-id-list
          (seq-filter (lambda (node-id)
                        (not (memq node-id node-ids-to-delete)))
                      old-id-list)))
    (setf (ord-collection-node-ids collection) new-id-list)
    (push old-id-list (ord-collection-history-stack collection))))

 ;;(ord--delete-node-ids-from-collection
  ;;'("1DC0D5CB-D786-4EAC-B0A6-9D81CB3E6492") (cadr ord-collection-list))

;;; viewing collections

;; parent keymap for view modes
(define-prefix-command 'ord-view-map)

;; any buffer that is viewing a collection will have the following buffer-local variable set

(defvar ord-buffer-collection nil
  "A buffer local variable: the collection which the buffer is displaying.")
(put 'ord-buffer-collection 'permanent-local t)

(defun ord--base-buffer-name-default (collection)
  "The name of a buffer for a collection starts with `*ord
collection: ` plus collection name."
  (concat
   "*ord collection: "
   (ord-collection-name collection)))

(defcustom ord-base-buffer-name
  'ord--base-buffer-name-default
  "Function that creates most of name of view buffers. The function should acceptargument COLLECTION."
  :group 'org-roam-desktop
  :type 'function)

(setq ord-base-buffer-name 'ord--base-buffer-name-default)

;;;; buffer-local work functions

(defvar ord-refresh-view-function nil
  "A buffer local variable: function to call to refresh view.")
(put 'ord-refresh-view-function 'permanent-local t)

(defvar ord-entries-in-region-function nil
  "A buffer local variable: function to call to get entries in current
  region.")
(put 'ord-entries-in-region-function 'permanent-local t)

(defvar ord-goto-entry-function nil
"A buffer local variable: function to call to move point to an entry
in the collection being displayed.")
(put 'ord-goto-entry-function 'permanent-local t)

(defvar ord-node-id-at-point-function nil
"A buffer local variable: function that returns the id of node at point in current view buffer,
e.g., the entry of the current row in list mode, or the entry being
previewed in section mode.")
(put 'ord-node-id-at-point-function 'permanent-local t)

;;;; commands for view buffers
;; (assumes that buffers have a non-nil ord-buffer-collection local value)

(defun ord--local-collection-or-choose (&optional force-prompt)
  "If current buffer has a non-nil ord-buffer-collection value,
returns that, otherwise uses standard function to find
collection. If optional argument FORCE-PROMPT is true, then
prompt the user no matter what."
  (if current-prefix-arg (setq force-prompt t))
  (if force-prompt (ord--choose-collection t)
    (if ord-buffer-collection  ord-buffer-collection
      (ord--choose-collection))))

(defun ord-add-node-at-point (collection)
  (interactive (list (ord--local-collection-or-choose)))  
  (unless (ord--add-node-ids-to-collection (ord--node-ids-at-point)
                                           collection)
    (ord--add-node-ids-to-collection (ord--node-ids-at-point t)
                                     collection))    
  (if ord-refresh-view-function (funcall ord-refresh-view-function)))

(defun ord-mode-choose-entry-from-collection (collection)
  "User can select entry from current collection; after selection,
point is moved there (if buffer-local var
ord-goto-entry-function is non-nil)."
  (interactive (list (ord--local-collection-or-choose)))  
  (let* ((node-ids (ord-collection-node-ids collection))
         (node-ids-and-names (seq-map
                              (lambda (node-id)
                                (let ((name (ord--node-name-from-id node-id)))
                                  (cons name node-id)))
                              node-ids))
         (selected-name (completing-read "Choose entry: "
                                         node-ids-and-names nil
                                         'require-match)))   
    (if-let ((selected-id (cdr (assoc selected-name
                                      node-ids-and-names))))
        (if ord-goto-entry-function (funcall ord-goto-entry-function selected-id)))))

(defun ord-mode-duplicate-collection (collection)  
  (interactive (list (ord--local-collection-or-choose)))
  (let* ((new-name (read-string "Name for new collection: "
                                (ord-collection-name collection)))
         (new-collection (ord-create-collection new-name)))
    (ord--add-node-ids-to-collection (ord-collection-node-ids collection)
                                     new-collection)
    (ord-section-view new-collection)))

(defun ord-mode-show-org-roam-buffer ()
  (interactive)
  "Show the org-roam-mode buffer for entry point is on."
  (org-roam-buffer-display-dedicated (org-roam-node-at-point)))

(defun ord-choose-and-add-node (collection)
  (interactive (list (ord--local-collection-or-choose)))  
  (let* ((node (org-roam-node-read "Entry to add: " nil nil t))
         (node-id (org-roam-node-id node)))
    (ord--add-node-ids-to-collection (list node-id)))
  (if ord-refresh-view-function (funcall ord-refresh-view-function)))

(defun ord-close-collection-and-buffer ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to close this collection?")
    (let ((inhibit-read-only t))
      (ord-close-collection ord-buffer-collection)
      (kill-this-buffer))))

(defun ord-view-other-collection ()
  (interactive)
  (let ((collection (ord--choose-collection t)))
    (ord-section-view collection)))

(defun ord-rename-collection (collection)  
  (interactive (list (ord--local-collection-or-choose)))
  (let ((new-name (read-string "New name for collection: "
                               (ord-collection-name collection))))
    (setf (ord-collection-name collection) new-name))
  (if ord-refresh-view-function (funcall ord-refresh-view-function)))

(defun ord-mode-delete-entries (collection)
  "Delete the entry at point from collection."
  (interactive (list (ord--local-collection-or-choose)))    
  (let ((nodes-to-delete (if (and (region-active-p) ord-entries-in-region-function)
                             (funcall ord-entries-in-region-function)
                           (ord--node-ids-at-point))))
    (ord--delete-node-ids-from-collection nodes-to-delete
                                          collection))
  (if ord-refresh-view-function (funcall ord-refresh-view-function)))

(defun ord-undo ()
  (interactive)
  (setf (ord-collection-node-ids ord-buffer-collection)
        (pop (ord-collection-history-stack ord-buffer-collection)))
  (if ord-refresh-view-function (funcall ord-refresh-view-function)))

(defun ord--goto-collection-notes (collection)
  (interactive (list (ord--local-collection-or-choose)))
  (let* ((notes-buffer-name
          (concat (funcall ord-base-buffer-name collection) " (notes)")))
    (if (get-buffer notes-buffer-name)
        (display-buffer (get-buffer notes-buffer-name))
      (let ((buffer (get-buffer-create notes-buffer-name)))
        (with-current-buffer buffer
          (org-mode))
        (display-buffer buffer)))))

;;;;; expand collection       

(defvar ord--default-expand-alist
  '(("backlinks" ?b (lambda (node-id) (ord--get-backlink-ids node-id)))
    ("forwardlinks" ?f ord--query-forlinks)))

(defcustom ord-mode-expand-alist ord--default-expand-alist
  "Ways to expand the collection. Format of the elements of the alist
should be: (SHORT-ANSWER HELP-MESSAGE EXPAND-FUNCTION), where
  EXPAND-FUNCTION takes a node-id, and returns a list of node-ids."
  :group 'org-roam-desktop
  :type 'elisp)

(setq ord-mode-expand-alist ord--default-expand-alist)

(defun ord-expand-collection (collection)
  (interactive (list (ord--local-collection-or-choose)))  
  (let* ((answer-list
          (append (seq-map
                   (lambda (alist-member)
                     (list (car alist-member) (cadr alist-member)  (car alist-member)))             
                   ord-mode-expand-alist) '(("quit" ?q "exit"))))
         (user-selection
          (read-answer "Expand collection by: " answer-list)))
    (if (not (string= user-selection "quit"))
        (if-let (expand-function (nth 2 (assoc user-selection
                                               ord-mode-expand-alist)))
            ;; want to adjust node-list based on whether region is active.
            (let* ((node-id-list (if (and (region-active-p) ord-entries-in-region-function)
                                     (funcall ord-entries-in-region-function)
                                   (ord-collection-node-ids collection)))
                   (new-node-id-list '()))
              (seq-do
               (lambda (node-id)               
                 (setq new-node-id-list (append new-node-id-list (funcall expand-function node-id))))
               node-id-list)
              (ord--add-node-ids-to-collection new-node-id-list collection)
              (if ord-refresh-view-function (funcall ord-refresh-view-function)))))))


;;;; section-view

(define-derived-mode ord-section-view-mode magit-section-mode "OrgRoamDesktop"
  "Major mode for displaying collection of org-roam nodes."
  (setq ord-section-view-mode-map (make-sparse-keymap))
  (set-keymap-parent ord-section-view-mode-map
                     (make-composed-keymap ord-view-map magit-section-mode-map)))

;;;;; preview sections, mostly copied from org-roam
(define-prefix-command 'ord-preview-map)
(set-keymap-parent 'ord-preview-map ord-section-view-mode-map)
(define-key ord-preview-map (kbd "<RET>") 'ord-preview-visit)

(defclass ord-preview-section (org-roam-node-section)
  ((keymap :initform 'ord-preview-map)
   (file :initform nil))
  "A `magit-section' used by `org-roam-mode' to contain preview content.
The preview content comes from FILE, and the link as at POINT.")

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

(cl-defun ord-preview-full-display-function (&optional (demote-level 2))
  "Return the preview content at point.

This function returns the entire body of the entry, with each entry
  demonted DEMOTE-LEVEL number of times."
  (let ((beg (save-excursion
               (org-roam-end-of-meta-data t)
               (point)))
        (end (point-max)))
    (dotimes (i demote-level)
      (org-map-entries 'org-do-demote))
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

;;;;; rendering the section buffer

(defclass ord-node-section (org-roam-node-section)
  ((keymap :initform 'ord-section-view-mode-map))
  "An `org-roam-node-section' based class, changing the initial keymap
  of the former.")

(cl-defun ord-node-insert-section (&key node-id (fill-column-number 2))
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
    (insert (concat (propertize (concat "  " (ord--node-name-from-id node-id))
                                'font-lock-face 'ord-entry-title-face)))
    (magit-insert-heading)
    (oset section node (org-roam-node-from-id node-id))
    (magit-insert-section section (ord-preview-section nil t)
      (let* ((fill-prefix (make-string fill-column-number ?/ ))
             (filled-string
              (with-temp-buffer
                (insert (ord-preview-get-contents (ord--file-from-id node-id)) "\n")
                (indent-region (point-min) (point-max) fill-column-number)
                (buffer-string))))        
        (insert (org-roam-fontify-like-in-org-mode filled-string))
        (oset section file (ord--file-from-id node-id))
        (oset section node (org-roam-node-from-id node-id))
        (insert ?\n)))))

(defun ord-section-view-function-default (node-id)
  (ord-node-insert-section
   :node-id node-id))

(defcustom ord-section-view-functions
  (list 'ord-section-view-function-default)
  "Function that draws the section for each entry in a collection."
  :group 'org-roam-desktop
  :type 'function)

(setq ord-section-view-functions (list             
                                        'ord-section-view-function-default))

(defvar ord--section-view-node-to-position-plist '()
  "A buffer local variable: stores where the section of a given node
  starts and ends in the current buffer. More specifically, the
  keys of the plist are the id's of the entries in the
  collection, and the value at an id has the form (start . end),
  which give positions in the buffer. Used for moving point to a
  given section.")
(put 'ord--section-view-node-to-position-plist 'permanent-local t)

(defun ord--section-buffer-name (collection)  
  (concat
   (funcall ord-base-buffer-name collection)
   " (section view)"))

(defun ord--section-view-render ()
  (setq-local ord--section-view-node-to-position-plist '())
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ord-section-view-mode)
    (org-roam-buffer-set-header-line-format
     (concat (ord-collection-name ord-buffer-collection) 
             " ("
             (number-to-string (length (ord-collection-node-ids
                                        ord-buffer-collection)))
             " entries)"))
    (if-let (node-ids (seq-filter 'ord--node-name-from-id (ord-collection-node-ids
                        ord-buffer-collection)))
        (let ((sorted-node-id-list (sort
                                  node-ids
                                  ord-sort-by-id-function)))
          (magit-insert-section (root)
            (magit-insert-heading)      
            (seq-do
             (lambda (node-id)
               (let ((section-start (point))
                     section-end)                 
                 (seq-do
                  (lambda (func)
                    (funcall func node-id))
                  ord-section-view-functions)
                 (setq-local ord--section-view-node-to-position-plist
                             (plist-put ord--section-view-node-to-position-plist
                                        node-id (list section-start (point))))))
             sorted-node-id-list))))))

(cl-defun ord--entries-in-region-section (&optional (start (region-beginning)) (end (region-end)))
  "If in an ord-section-view-mode buffer, return the entries are displayed between
  START and END, inclusive."  
  (let ((node-ids (ord-collection-node-ids
                   ord-buffer-collection))
        (node-ids-in-region '()))
    (seq-do (lambda (node-id)
              (if-let ((positions (plist-get ord--section-view-node-to-position-plist
                                             node-id)))
                  (let ((section-start (car positions))
                        (section-end (cadr positions)))
                    (if (and (and section-start section-end)
                             (or (and (<= start section-start) (<= section-start end))
                                 (and (<= start section-end) (<= section-end
                                                                 end))
                                 (and (<= section-start start) (<= end
                                                                   section-end))))
                        (push node-id node-ids-in-region)))))
            node-ids)
    node-ids-in-region))

(defun ord-refresh-view-section ()
  "Refresh the contents of the currently selected org-roam-desktop
  buffer."
  (interactive)  
  (unless (not (derived-mode-p 'ord-section-view-mode))
    (let ((point (point)))
      (if (magit-current-section)
          (magit-section-cache-visibility (magit-current-section)))
      (ord--section-view-render)
      (goto-char point))))

(defun ord-goto-entry-function-section (node-id)
  (goto-char (car (plist-get ord--section-view-node-to-position-plist
                             node-id))))

(defun ord-section-view (collection)
  (interactive (list
                (ord--local-collection-or-choose)))
  (let* ((buffer-name (ord--section-buffer-name collection))
         (buffer (get-buffer-create buffer-name)))        
    (with-current-buffer buffer      
      (setq-local ord-buffer-collection collection)
      (setq-local ord-refresh-view-function 'ord-refresh-view-section)
      (setq-local ord-entries-in-region-function
                  'ord--entries-in-region-section)
      (setq-local ord-goto-entry-function
                  'ord-goto-entry-function-section)
      (setq-local ord-node-id-at-point-function (lambda () (org-roam-node-id (org-roam-node-at-point))))
      (ord--section-view-render))
    (switch-to-buffer-other-window buffer)))

;;;; tabulated-list view

;; seems to slow things down a lot
(defun ord--list-sort (first second)
  "FIRST and SECOND are elements of =tabulated-list-entries=."
  (let ((first-id (car first))
        (second-id (car second)))
  (funcall ord-sort-by-id-function first-id second-id)))

(define-derived-mode ord-list-view-mode tabulated-list-mode "org-roam desktop"
  "Major mode for displaying collection of org-roam nodes."
  (setq ord-list-view-mode-map (make-sparse-keymap))
  (set-keymap-parent ord-list-view-mode-map
                     (make-composed-keymap ord-view-map tabulated-list-mode-map))
  (if-let (collection-name (ord-collection-name
                            ord-buffer-collection))      
      (let ((list-name
             (concat (ord-collection-name ord-buffer-collection) 
                     " ("
                     (number-to-string (length (ord-collection-node-ids
                                                ord-buffer-collection)))
                     " entries)")))
        (setq tabulated-list-format (vector `(,list-name 40 ord--list-sort)
                                            '("Backlinks"      10 t) '("Forlinks"      10 t)))  
        (setq tabulated-list-sort-key (cons list-name nil))
        (add-hook 'tabulated-list-revert-hook
                  (lambda () (if ord-refresh-view-function (funcall
                                                            ord-refresh-view-function)))))))

(defun ord--list-buffer-name (collection)  
  (concat
   (funcall ord-base-buffer-name collection)
   " (list view)"))

(cl-defun ord--entries-in-region-list (&optional (start (region-beginning)) (end (region-end)))
  "If in an ord-list-view-mode buffer, return the entries are displayed between
  START and END, inclusive."    
   (let ((node-ids-in-region '()))
    (save-excursion      
      (goto-char start)
      (while (< (point) end)        
        (push (tabulated-list-get-id) node-ids-in-region)
        (next-line)))
    node-ids-in-region))
                   
(defun ord--list-view-render (collection)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ord-list-view-mode)    
    (setq tabulated-list-entries nil)
    (tabulated-list-init-header)
    (dolist (node-id (seq-filter 'ord--node-name-from-id (ord-collection-node-ids collection)))
      (let ((name (ord--node-name-from-id node-id))
            (backlinks-str (number-to-string (length
                                              (ord--get-backlink-ids node-id))))
            (forlinks-str (number-to-string (length
                                              (ord--query-forlinks node-id)))))
        (push (list node-id (vector name backlinks-str forlinks-str)) tabulated-list-entries))
      (tabulated-list-print))))

(defun ord-refresh-view-list ()
  (let ((point (point)))
    (ord--list-view-render ord-buffer-collection)    
    (goto-char point)))
    
(defun ord-list-view (collection)
  (interactive (list
                (ord--local-collection-or-choose)))
  (let* ((buffer-name (ord--list-buffer-name collection))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer      
      (setq-local ord-buffer-collection collection)
      (setq-local ord-entries-in-region-function
                  'ord--entries-in-region-list)
      (setq-local ord-node-id-at-point-function 'tabulated-list-get-id)
      (setq-local ord-refresh-view-function 'ord-refresh-view-list)
      ;; (setq-local ord-goto-entry-function 'ord-goto-entry-function-section)
      (ord--list-view-render collection))
    (switch-to-buffer-other-window buffer)))

;;; save, close, and load collections

;;;; translate collection between struct, plist, and json
(defun ord--collection-to-plist (collection)
  "COLLECTION should be an ord-collection struct. Returns a
  plist representing that struct."
  (list :name (ord-collection-name collection)
        :id (ord-collection-id collection)
        :nodes (vconcat (seq-filter (lambda (id) id) (ord-collection-node-ids
                                                      collection)))))

(defun ord--collection-from-plist (collection-plist)
  "COLLECTION-PLIST should have keys :name, :id, and :nodes. Returns a
  struct of type org-roam-desktop-collection."
  (cl-destructuring-bind (&key name id nodes) collection-plist
    (make-ord-collection :name name :id id
                         :node-ids (mapcar 'identity nodes)
                         :marked-node-ids '())))

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

;;;; save, close, and load functions

(defun ord--default-file-name-for-collection (collection)
  "The name of a file for a collection is its name plus json file extension."
  (concat   
   (ord-collection-name collection)   
   ".json"))

(defun ord-save-collection (collection)  
  (interactive (list (ord--local-collection-or-choose)))
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

;;; export collection into org-mode buffer

(defun ord-export-collection-to-org-buffer (collection)
  "Creates an org-mode buffer displaying COLLECTION. Will be one top level
entry, whose heading is the name of the section. Then a subentry
  for each node in COLLECTION. For the subentries, the headline
  is the node title and the body is the preview section text."
  (interactive (list (ord--local-collection-or-choose)))
  (let* ((buffer-name (concat (ord--section-buffer-name
                               collection) ".org"))
         (buffer (get-buffer-create buffer-name))         
         (node-id-list (ord-collection-node-ids collection)) 
         (sorted-node-id-list (sort
                            node-id-list
                            ord-sort-by-id-function))
         (ord-preview-display-function #'ord-preview-full-display-function))
    (with-current-buffer buffer
      (setq-local ord-buffer-collection collection)
      (erase-buffer)
      (org-mode)      
      (seq-do
       (lambda (node-id)
         (org-insert-heading nil nil t)         
         (insert
          (org-link-make-string
           (concat "id:" node-id)
           (ord--node-name-from-id node-id)))
         (newline)
         (insert (ord-preview-get-contents
                  (ord--file-from-id node-id)) "\n")
         (newline))
       sorted-node-id-list)
      (goto-char (point-min))
      (org-cycle-global 1)
      (switch-to-buffer-other-window buffer))))

(defun ord-print-collection-as-org-list (collection)
  (interactive (list (ord--local-collection-or-choose)))  
  (let* ((node-id-list (sort (ord-collection-node-ids collection) ord-sort-by-id-function))
         (first-node-id (car node-id-list)))
    (newline)
    (insert
         (concat "- "
                 (org-link-make-string
                  (concat "id:" first-node-id)
                  (ord--node-name-from-id first-node-id))))
      (newline)
      (seq-do
       (lambda (node-id)         
         (org-insert-item)
         (insert
          (org-link-make-string
           (concat "id:" node-id)
           (ord--node-name-from-id node-id))))
       (cdr node-id-list))))

;; (setq testcollectionname (random-letter-string 10))
;; (setq testcollectiontest (ord-create-collection testcollectionname))
;; (ord-print-collection-as-org-list testcollectiontest)
;; (ord-close-collection testcollectiontest)

(defun ord-export-collection-to-org-list (collection)
  (interactive (list (ord--local-collection-or-choose)))
  (let* ((buffer-name (concat (ord--section-buffer-name
                               collection) "-list" ".org"))
         (buffer (get-buffer-create buffer-name))
         (node-id-list (ord-collection-node-ids collection)) 
         (sorted-node-id-list (sort
                               node-id-list
                               ord-sort-by-id-function))) 
    (with-current-buffer buffer
      (setq-local ord-buffer-collection collection)
      (erase-buffer)
      (org-mode)
      (ord-print-collection-as-org-list collection)
      (switch-to-buffer-other-window buffer))))

(defun ord-links-in-region-to-org-buffer ()
  "Opens new org-mode buffer containing the entries of all of the
links in the current region."
  (interactive)
  (let* ((node-ids
          (if (and (region-active-p) ord-entries-in-region-function)
                             (funcall ord-entries-in-region-function)
            (ord--node-ids-at-point)))
         (temp-name (org-id-uuid))
         (temp-collection (make-ord-collection :name temp-name
                              :id (concat "ord-" temp-name)
                              :node-ids node-ids
                              :marked-node-ids '()
                              :history-stack '())))
    (ord-export-collection-to-org-buffer temp-collection)))

;;; ord-section-mode-map

(define-key ord-view-map (kbd "g")
            (lambda () (interactive) (if ord-refresh-view-function (funcall ord-refresh-view-function))))
(define-key ord-preview-map [remap org-roam-buffer-refresh] (lambda () (interactive) (if ord-refresh-view-function (funcall ord-refresh-view-function))))
(define-key ord-view-map (kbd "k")
            #'ord-mode-delete-entries)
(define-key ord-view-map (kbd "a") #'ord-add-node-at-point)
(define-key ord-view-map (kbd "s")
            #'ord-save-collection)
(define-key ord-view-map (kbd "b")
            #'ord-mode-show-org-roam-buffer)
(define-key ord-view-map (kbd "<RET>")
            (lambda () (interactive)
              (org-roam-node-visit (org-roam-node-from-id (car (ord--node-ids-at-point))) t)))
(define-key ord-view-map (kbd "t")            
            (lambda () (interactive)
              (other-tab-prefix)
              (org-roam-node-visit (org-roam-node-from-id (car
                                                           (ord--node-ids-at-point))))))
(define-key ord-view-map (kbd "c") #'ord-mode-choose-entry-from-collection)
(define-key ord-view-map (kbd "o")
            #'ord-export-collection-to-org-buffer)
(define-key ord-view-map (kbd "w") #'ord-close-collection-and-buffer)
(define-key ord-view-map (kbd "v") #'ord-view-other-collection)
(define-key ord-view-map (kbd "r") #'ord-rename-collection)
(define-key ord-view-map (kbd "e") #'ord-expand-collection)
(define-key ord-view-map (kbd "u") #'ord-undo)
(define-key ord-view-map (kbd "d") #'ord-mode-duplicate-collection)
(define-key ord-view-map (kbd "q") #'quit-window)


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
            #'ord-section-view)
(define-key ord-map (kbd "M-l")
            #'ord-list-view)
(define-key ord-map (kbd "M-s") #'ord-save-collection)
(define-key ord-map (kbd "M-o") #'ord-load-collection)
(define-key ord-map (kbd "M-k") #'ord-close-all-collections)
(define-key ord-map (kbd "M-e") #'ord-mode-choose-entry-from-collection)
(define-key ord-map (kbd "M-n") #'ord--goto-collection-notes)
(define-key ord-map (kbd "M-e") #'ord-links-in-region-to-org-buffer)
(provide 'org-roam-desktop)


