(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files '("~/.org/notes.org"
						 "~/.org/tasks.org"
						 "~/.org/ideas.org"
						 "~/.org/project.org"
						 ))


(setq org-capture-templates
      (quote (
              ("n"         ;; hotkey
               "Notes" ;; name
               entry       ;; type
               (file+datetree "~/.org/notes.org") ;; target
               "* TODO %^{title note} %T \n  1. %^{write a note}" ;; template
               )
              ("t" "Schedule an event or a task" entry
               (file+datetree "~/.org/tasks.org")
               "* TODO [#%^{Priority}] %^{Schedule of event or task}\n** %^{title of event or task}\n SCHEDULED: %^t\n  1. %^{write a description:}"
               )
			  ("i" "Ideas" entry
               (file+datetree "~/.org/ideas.org")
               "* TODO [0%] %^{Title idea} %T\n  - [-] %^{Description of idea}")
              ("b" "Add a book to read" entry
               (file+headline "~/.org/books.org" "Books to read")
               "* TODO %^{Book name}\n  - %^{Why to read this book?}"
               )
			  ("l" "Notes of the linux" item
			   (file+datetree "~/.org/linux.org")
			   "%^{title}")
			  ("p" "Projects" item
			   (file+datetree "~/.org/project.org")
			   "[-] %^{name project} - %^{description the project} %t")
              )))
