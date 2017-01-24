(require 'avandu)
(define-derived-mode avandu2-mode tabulated-list-mode "Avandu2"
  "Major mode for ttrss displaying headlines in tabulated list.
\\{avandu2-mode-map}"
  (setq tabulated-list-format [("Col1" 24 t)
			       ("Col2" 80 t)
			       ("Col3"  10 nil)
			       ("Col4" 10 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Col3" nil))
  (tabulated-list-init-header))

(defun print-current-line-id ()
  (interactive)
  (message (concat "current line ID is: " (tabulated-list-get-id))))

(defun my-listing-command ()
  (interactive)
  (pop-to-buffer "*Avandu2 Overview*" nil)
  (avandu2-mode)
  (setq tabulated-list-entries (list 
				(list "1" ["1" "2" "3" "4"])
				(list "2" ["a" "b" "c" "d"])
				(list "3" (vector "e" "f" "g" "h"))))
  (tabulated-list-print t))

(defun avandu-table-overview ()
  "Request the headlines of unread articles and list them.

The list is grouped and sorted by feed ID.  Sorting by feed ID is
meaningless, but it's easy."
  (interactive)
  (avandu--check-login)
  (let ((buffer (get-buffer-create "*avandu-overview*"))
	(result (sort (cl-coerce (avandu-headlines -4 :show-excerpt t
						   :view-mode "unread")
				 'list)
		      #'avandu--order-feed))
	feed-id )
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (setq tabulated-list-entries (list 
				(list "1" ["1" "2" "3" "4"])
				(list "2" ["a" "b" "c" "d"])))
      (mapc #'(lambda (elt)
		(unless (equal feed-id (assq 'feed_id elt))
		  (add-to-list 'tabulated-list-entries
			       (list (avu-prop elt id) (vector
							(avu-prop elt feed_title)
							(avu-prop elt title)
							(avu-prop elt excerpt)
							(avu-prop elt link)
							)))))
	    result)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu2-mode)
      (tabulated-list-print t))
    (switch-to-buffer buffer)))


