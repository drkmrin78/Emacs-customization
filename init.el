(package-initialize)
(setq user-full-name "Drake M. Petersen"
      user-mail-address "drakemp@cs.umd.edu"
      calendar-location-name "College Park, MD")

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

;;EVIL
(require 'evil)
(evil-mode t)
;;Better Undo than default
(setq evil-want-fine-undo t)
(setq evil-insert-state-cursor '((bar . 4) "dark violet")
      evil-normal-state-cursor '(box "medium spring green"))

;;basics all users should set
;; swap CTRL and CAPS (highly recommend)
(global-linum-mode)
(setq-default c-basic-offset 3)
(fset 'yes-or-no-p 'y-or-n-p)

;;multi-compile
(require 'multi-compile)
(setq multi-compile-alist
      '(
	(c-mode . (("gcc-216" . "gcc -ansi -Wall -g -O0 -Wwrite-strings -Wshadow -pedantic-errors -fstack-protector-all %file-name")
		   ("gcc" . "gcc -g %file-name")
		   ("make" . "make")))
	)
      ;; more compile commands can be added here.
      )
(global-set-key (kbd "H-c") 'multi-compile-run)

;;ORG MODE
(setq org-ellipsis "▾")

(add-hook 'org-mode-hook 'flyspell-mode)
(global-set-key (kbd "H-z") 'ispell-word)
(setq org-agenda-files '("~/Dropbox"))
(global-set-key (kbd "H-q") 'org-agenda)
;;(add-to-list 'load-path "elpa/org-bullets-20140918.1137")
(require 'org-bullets)
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
(defun fill-setup()
  "sets the column to 80 and sets minor mode auto-fill-mode"
  (set-fill-column 80)
  (auto-fill-mode t))

(add-hook 'org-mode-hook 'fill-setup)

;;ace-mc
(require 'ace-mc)
(global-set-key (kbd "M-f") 'ace-mc-add-multiple-cursors)

;;never actually works 
(defun add-pretty-symbols-wtf ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(("lambda" . 955)))) ; λ

(add-pretty-symbols-wtf)
(global-prettify-symbols-mode)

;; ESS mode configuration (only if ess is in a nonstandard place)
(require 'ess)

;;ace-mc
(global-set-key (kbd "M-f") 'ace-mc-add-multiple-cursors)

;;Org-ac
(require 'org-ac)
(org-ac/config-default)

;;Prolog-mode
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;Hydra
(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(defhydra hydra-windows (global-map "H-b")
  "windowing hydra"
  ("o" other-window "other")
  ("1" delete-other-windows "kill others")
  ("2" split-window-below "split-vert")
  ("3" split-window-right "split-hort")
  ("0" delete-window "delete-window")
  ("l" enlarge-window "enlarge-window")
  
  ("s" (lambda ()
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
	   (select-window other-window))) "swap windows"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;; If you want to create a Org/Latex doc, use the code below.
;; remember to change the PATH, TITLE, and DATE appropriately.

(org-mode) ;;`C-x C-e` then `C-c C-c` below
#+begin_src sh 
    cd ~/Desktop/PATH
    echo \"#+title: TITLE 
#+options: toc:nil num:nil tex:t tit
#+startup: latexpreview showall 
#+latex_header:	\\usepackage{graphicx}  \\graphicspath{{}} 
#+latex_header:	\\renewcommand{\\baselinestretch}{1} 
#+latex_header:	\\newcommand\\tab[1][1cm]{\\noindent\\hspace*{#1}} 
#+latex_header:	\\renewcommand{\\maketitle}{} 
#+latex_header:	\\usepackage[top=0.5in, bottom=1in, left=1in, right=1in]{geometry} 
#+latex_header:	\\usepackage{mathtools} \\DeclarePairedDelimiter\\ceil{\\lceil}{\\rceil} \\DeclarePairedDelimiter\\floor{\\lfloor}{\\rfloor}
* \\begin{center} Drake Petersen \\tab TITLE \\tab DATE 2017 \\end{center}
\" > TITLE.org
#+end_src")
 '(package-selected-packages
(quote
 (multi-compile graphviz-dot-mode demo-it ace-mc caml prolog tuareg org-bullets org-ac hydra evil ess)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(region ((t (:background "light slate gray")))))
