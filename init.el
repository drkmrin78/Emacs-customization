(package-initialize)
(setq user-full-name "Drake M. Petersen"
      user-mail-address "drakemp@cs.umd.edu"
      calendar-location-name "College Park, MD")

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("org" . "http://orgmode.org/elpa")))
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;tramp
(use-package tramp
  :ensure t
  :init (setq tramp-default-method "sshx")
  )

;;EVIL
(use-package evil
  :ensure t
  :init
  (evil-mode t)
  ;;Better Undo than default
  (setq evil-want-fine-undo t)
  ;;M-x l-c-d <----for color options
  (setq evil-insert-state-cursor '((bar . 4) "dark violet")
	evil-normal-state-cursor '(box "medium spring green"))
  )

;;iedit
(use-package iedit
  :ensure t)
(use-package evil-iedit-state
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  )

;;expand-region
(use-package expand-region
  :ensure t
  :init
  ;;Note this binding is used because evil-mode is active
  ;;global-set-key is needed for users not using evil mode
  (define-key evil-motion-state-map (kbd "C-e") 'er/expand-region)
  )

;;basics all users should set
;; swap CTRL and CAPS (highly recommend)
(global-linum-mode)
(setq-default c-basic-offset 3)
(fset 'yes-or-no-p 'y-or-n-p)
;;auto refresh documents
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq auto-window-vscroll nil) ;;speed up?

;;multi-compile
(use-package multi-compile
  :ensure t
  :init
  (global-set-key (kbd "H-c") 'multi-compile-run)
  (setq multi-compile-alist
	'(
	  (c-mode . (("gcc-216" . "gcc -ansi -Wall -g -O0 -Wwrite-strings -Wshadow -pedantic-errors -fstack-protector-all %file-name")
		     ("gcc" . "gcc -g %file-name")
		     ("make" . "make")
		     ("mclean" . "make clean")))
	  )
	;; more compile commands can be added here.
	)
  (defun bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (when (and
	   (buffer-live-p buffer)
	   (string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string)
	   (not
	    (with-current-buffer buffer
	      (goto-char (point-min))
	      (search-forward "warning" nil t))))
      (run-with-timer 1 nil
		      (lambda (buf)
			(bury-buffer buf)
			(switch-to-prev-buffer (get-buffer-window buf) 'kill))
		      buffer)))
  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
  )

;;ORG MODE
(use-package org
  :ensure t
  :init
  (setq org-ellipsis "▾")
  
  (add-hook 'org-mode-hook 'flyspell-mode)
  (global-set-key (kbd "H-z") 'ispell-word)
  (setq org-agenda-files '("~/Dropbox"))
  (global-set-key (kbd "H-q") 'org-agenda)
  ;;(add-to-list 'load-path "elpa/org-bullets-20140918.1137")
  (use-package org-bullets
    :ensure t
    :init 
    (add-hook 'org-mode-hook
	      (lambda ()
		(org-bullets-mode t)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (ruby . t)
       (ocaml . t)
       (sh . t)
       (dot . t)
       (R . t)
       ))
    )
  (defun fill-setup()
    "sets the column to 80 and sets minor mode auto-fill-mode"
    (set-fill-column 80)
    (auto-fill-mode t))
  (add-hook 'org-mode-hook 'fill-setup)
  )

;;ace-mc
(use-package ace-mc
  :ensure t
  :init
  (global-set-key (kbd "M-f") 'ace-mc-add-multiple-cursors)
  )
;;never actually works
(defun add-pretty-symbols-wtf ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(("lambda" . 955)))) ; λ

(add-pretty-symbols-wtf)
(global-prettify-symbols-mode)

;;smartparens
(use-package smartparens
  :ensure t
  :init
  (add-hook 'after-init-hook 'smartparens-global-mode)
  ;;gets rid of the annoying compile warnings from smartparens
  (add-hook 'window-setup-hook
	    '(lambda ()
	       (kill-buffer "*Compile-Log*")
	       (delete-other-windows)))
  )


;;Emacs wiki
(defun window-swap-rotate ()
  "Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
	  (other-window-hscroll (window-hscroll other-window))
	  (other-window-point (window-point other-window))
	  (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))
(global-set-key (kbd "H-r") 'window-swap-rotate)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;; If you want to create a Org/Latex doc, use the code below.

#+title: TITLE
#+options: toc:nil num:nil tex:t tit
#+startup: latexpreview showall
#+latex_header:	\\usepackage{graphicx}  \\graphicspath{{}}
#+latex_header:	\\renewcommand{\\baselinestretch}{1}
#+latex_header:	\\newcommand\\tab[1][1cm]{\\noindent\\hspace*{#1}}
#+latex_header:	\\renewcommand{\\maketitle}{}
#+latex_header:	\\usepackage[top=0.5in, bottom=1in, left=1in, right=1in]{geometry}
#+latex_header:	\\usepackage{mathtools} \\DeclarePairedDelimiter\\ceil{\\lceil}{\\rceil} \\DeclarePairedDelimiter\\floor{\\lfloor}{\\rfloor}
* \\begin{center} Drake Petersen \\tab TITLE \\tab DATE 2018 \\end{center}
")
 '(org-list-allow-alphabetical t)
 '(package-selected-packages
   (quote
    (flycheck use-package evil-iedit-state iedit w3m smartparens expand-region multi-compile graphviz-dot-mode demo-it ace-mc prolog tuareg org-bullets evil ess)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(region ((t (:background "light slate gray")))))
