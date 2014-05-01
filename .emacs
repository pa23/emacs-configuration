;; -*-Emacs-Lisp-*-

;;    pa23's emacs configuration file    ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))

;; directory for additional modules
(add-to-list 'load-path "~/.site-lisp")

;; disable startup message
(setq inhibit-startup-message t)

;; short command confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#E5E5FF")

;; smooth text scrolling by arrow keys
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

;; disable scrolling acceleration by using mouse wheel
(setq mouse-wheel-progressive-speed nil)

;; set UTF-8 as work encoding
(set-language-environment 'utf-8)

;; disable tabs
(setq-default indent-tabs-mode nil)

;; set tab width to 4 symbols
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; disable line wrap
(setq default-truncate-lines t)

;; set scrolling step by using mouse wheel
(defun scroll-up-1-lines ()
  "Scroll up 1 lines"
  (interactive)
  (scroll-up 1))
(defun scroll-down-1-lines ()
  "Scroll down 1 lines"
  (interactive)
  (scroll-down 1))
(global-set-key (kbd "<mouse-4>") 'scroll-down-1-lines) ;
(global-set-key (kbd "<mouse-5>") 'scroll-up-1-lines) ;

;; change line wrapping for current buffer
(setq default-truncate-lines t)
(global-visual-line-mode 1)
(global-set-key [f9] 'toggle-truncate-lines)

;; set style for C and C++ source codes
(setq c-default-style '((java-mode . "java") (other . "stroustrup")))

;; scroll compilation log buffer
(setq compilation-scroll-output 1)

;; activation cmake mode
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(autoload 'cmake-mode "cmake-mode.el" t)

;; load and setup popup switcher dialog (needs popup.el and popup-switcher.el)
(require 'popup-switcher)
(setq psw-in-window-center t)
(global-set-key [f2] 'psw-switch-buffer)

;; close all opened buffers with a single command
(defun kill-all-buffers ()
  "Close all opened buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; change coding for current buffer
(setq my-working-codings '(utf-8 windows-1251 koi8-r cp866))
(setq my-current-coding-index -1)
(defun change-coding ()
  "Change coding for current buffer."
  (interactive)
  (setq my-current-eol
        (coding-system-eol-type buffer-file-coding-system))
  (setq my-next-coding-index (+ my-current-coding-index 1))
  (if (equal my-next-coding-index (safe-length my-working-codings))
      (setq my-next-coding-index 0))
  (setq my-new-coding-system
        (prin1-to-string (nth my-next-coding-index my-working-codings)))
  (if (equal my-current-eol 0)
      (setq my-new-coding (concat my-new-coding-system "-unix"))
    (if (equal my-current-eol 1)
        (setq my-new-coding (concat my-new-coding-system "-dos"))
      (setq my-new-coding (concat my-new-coding-system "-mac"))))
  (setq my-current-coding-index my-next-coding-index)
  (setq coding-system-for-read (read my-new-coding))
  (revert-buffer t t)
  (message "Set coding %s." my-new-coding)
  )
(global-set-key [f11] 'change-coding)

;; change eol for current buffer
(defun change-eol ()
  "Change EOL for current buffer."
  (interactive)
  (setq my-current-eol (coding-system-eol-type buffer-file-coding-system))
  (if (equal my-current-eol 2)
      (setq my-new-eol 0)
    (setq my-new-eol (+ my-current-eol 1)))
  (setq my-new-coding
        (coding-system-change-eol-conversion
         buffer-file-coding-system my-new-eol))
  (set-buffer-file-coding-system my-new-coding)
  )
(global-set-key [f12] 'change-eol)

;; marmalade repository
;; ( update package list: M-x package-refresh-contents )
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
