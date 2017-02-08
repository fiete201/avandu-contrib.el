;;; avandu-extended.el extension of avandu
;;; this code adds a new mode for avandu ttrss reader
(require 'avandu)
(defvar article-id nil)
(defun print-current-line-id ()
  (interactive)
  (message (concat "current line ID is: " (tabulated-list-get-id))))

(defun avandu2-open-next-article-in-table ()
  (interactive)
  "find next article in overviews table and open it"
  (let ((buffer (get-buffer-create "*avandu-table-overview*")))
    (with-current-buffer buffer
      ;;;(make-local-variable 'article-id)
      (goto-char (point-min))
      (while (not (equal (tabulated-list-get-id) article-id))
      ;;;focus next line
	(next-line))
    ;;;focus next line
      (next-line)
    ;;;open article with that id
      (avandu2-view-article (tabulated-list-get-id)))))

;;; Since the original avandu-view-article function runs an UI function 
;;; that does not work with my ui I had to copy the whole function excluding
;;; that call
(defun avandu2-view-article (id)
  "Show a single article identified by ID in a new buffer."
  (interactive "nArticle id: ")
  (setq article-id id)
  (let* ((data (avandu-get-article id))
         (buffer (get-buffer-create "*avandu-article*"))
         (inhibit-read-only t)
         content-start
         content-end)
    (with-current-buffer buffer
      (erase-buffer)
      (mapc #'(lambda (item)
                (insert
                 (propertize (avu-prop item title)
                             'face 'avandu-article-title))
                (insert "\n")
                (insert
                 (propertize (concat "by: " (avu-prop item author))
                             'face 'avandu-article-author))
                (insert " (")
                (insert-button
                 "Browse original"
                 'url (avu-prop item link)
                 'action #'(lambda (button)
                             (browse-url (button-get button 'url))))
                (insert ")")
                (insert "\n")(insert "\n")
                (setq content-start (point))
                (insert (avu-prop item content))
                (setq content-end (point))
                (insert "\n")(insert "\n"))
            data)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu-article-mode))
    (avandu-mark-article-read id)
    (switch-to-buffer buffer)
    (when avandu-article-render-function
      (funcall
       avandu-article-render-function content-start
       (min content-end (point-max))))
    (goto-char (point-min))))

(defun avandu2-open-article ()
  (interactive)
  "Get id of article in focused line and pass it to avandu-view-article"
  (avandu2-view-article (tabulated-list-get-id)))

(defun avandu2-table-overview ()
  "Request the headlines of unread articles and list them.

The list is grouped and sorted by feed ID.  Sorting by feed ID is
meaningless, but it's easy."
  (interactive)
  (avandu--check-login)
  (let ((buffer (get-buffer-create "*avandu-table-overview*"))
	(result (sort (cl-coerce (avandu-headlines -4 :show-excerpt t
						   :view-mode "unread")
				 'list)
		      #'avandu--order-feed))
	feed-id )
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (setq tabulated-list-entries '())
      (mapc #'(lambda (elt)
		(unless (equal feed-id (assq 'feed_id elt))
		  (add-to-list 'tabulated-list-entries
			       (list (avu-prop elt id) (vector
							(avu-prop elt title)
							(avu-prop elt feed_title)
							(format-time-string "%a %D %R" (avu-prop elt updated))
							)))))
	    result)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu2-mode)
      (tabulated-list-print t))
    (switch-to-buffer buffer)))

(defvar avandu2-overview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "o" 'avandu2-open-article)
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap for `avandu2-mode'.")

(define-derived-mode avandu2-mode tabulated-list-mode "Avandu2"
  "Major mode for ttrss displaying headlines in tabulated list.
\\{avandu2-mode-map}"
  (use-local-map avandu2-overview-map)
  (setq tabulated-list-format [("Headline" 64 t)
			       ("Kategorie" 40 t)
			       ("Updated" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Updated" t))
  (tabulated-list-init-header))


(eval-after-load 'avandu
  '(define-key avandu-article-mode-map "n" 'avandu2-open-next-article-in-table))
