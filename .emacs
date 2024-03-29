;; -*-Emacs-Lisp-*-

;;;;  pa23's emacs configuration file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes '(dichromacy))
 '(package-selected-packages '(undo-tree xterm-color multiple-cursors))
 '(scroll-bar-mode 'right)
 '(select-enable-clipboard t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; fonts
;(set-frame-font "DejaVu Sans Mono 10")
;(setq default-frame-alist '((font . "DejaVu Sans Mono-10")))

;; default frame size
(setq default-frame-alist '((width . 80) (height . 25)))

;; directory for additional modules
(add-to-list 'load-path "~/.site-lisp")

;; repositories
;; ( update package list: M-x package-refresh-contents )
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; disable startup message
(setq inhibit-startup-message t)

;; short command confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight current line
(global-hl-line-mode 1)

;; turn on undo-tree
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;; highligh expression between brackets
;;(setq show-paren-style 'expression)

;; copy from one dired dir to the next dired dir shown in a split window
(setq dired-dwim-target t)

;; go to file directory and place cursor on its name
(require 'dired-x)
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; overwrite text selection
(delete-selection-mode 1)

;; remember the cursor position of files
(save-place-mode 1)

;; smooth text scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

;; disable scrolling acceleration by using mouse wheel
(setq mouse-wheel-progressive-speed nil)

;; set UTF-8 as work encoding
(set-language-environment 'utf-8)

;; disable tabs
(setq-default indent-tabs-mode nil)

;; set tab width to 4 symbols
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default standart-indent 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; delete excess whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; week first day for calendar
(setq calendar-week-start-day 1)

;; change line wrapping for current buffer
(setq default-truncate-lines t)
(global-visual-line-mode 1)
(global-set-key [f9] 'toggle-truncate-lines)

;; set style for C and C++ source codes
(setq c-default-style '((java-mode . "java") (other . "stroustrup")))

;; activation matlab mode for *.m files
;; (setq auto-mode-alist
;;       (cons
;;        '("\\.m$" . octave-mode)
;;        auto-mode-alist))

;; scroll compilation log buffer
(setq compilation-scroll-output 1)

;; activation xah-elisp-mode for emacs lisp sources
;; (require 'xah-elisp-mode)
;; (autoload 'xah-elisp-mode "xah-elisp-mode.elc" t)
;; (add-hook 'emacs-lisp-mode-hook 'xah-elisp-mode)

;; setup popup switcher dialog
;(require 'popup-switcher)
;;(setq psw-in-window-center t)
;(setq psw-popup-position 'center)
;(setq psw-mark-modified-buffers t)
;(global-set-key [f2] 'psw-switch-buffer)

;; close all opened buffers with a single command
(defun pa23-kill-all-buffers ()
  "Close all opened buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)) )

;; change coding for current buffer
(setq my-working-codings ["utf-8" "windows-1251" "koi8-r" "cp866" "utf-16le"])
(setq my-current-coding-index -1)
(defun pa23-change-coding ()
  "Change coding for current buffer."
  (interactive)
  (let (my-current-eol
        my-next-coding-index
        my-new-coding-system
        my-new-coding)
    (setq my-current-eol
          (coding-system-eol-type buffer-file-coding-system))
    (setq my-next-coding-index (1+ my-current-coding-index))
    (if (equal my-next-coding-index (length my-working-codings))
        (setq my-next-coding-index 0))
    (setq my-new-coding-system
          (elt my-working-codings my-next-coding-index))
    (cond ((equal my-current-eol 0)
           (setq my-new-coding (concat my-new-coding-system "-unix")))
          ((equal my-current-eol 1)
           (setq my-new-coding (concat my-new-coding-system "-dos")))
          ((equal my-current-eol 2)
           (setq my-new-coding (concat my-new-coding-system "-mac"))))
    (setq coding-system-for-read (read my-new-coding))
    (revert-buffer t t)
    (setq my-current-coding-index my-next-coding-index)
    (message "Set coding %s." my-new-coding) ) )
(global-set-key [f11] 'pa23-change-coding)

;; change eol for current buffer
(defun pa23-change-eol ()
  "Change EOL for current buffer."
  (interactive)
  (let (my-current-eol
        my-new-eol
        my-new-coding)
    (setq my-current-eol
          (coding-system-eol-type buffer-file-coding-system))
    (if (equal my-current-eol 2)
        (setq my-new-eol 0)
      (setq my-new-eol (1+ my-current-eol)))
    (setq my-new-coding
          (coding-system-change-eol-conversion
           buffer-file-coding-system my-new-eol))
    (set-buffer-file-coding-system my-new-coding) ) )
(global-set-key [f12] 'pa23-change-eol)

;; align highlighed by whitespace
(defun pa23-align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

;; copy rectangle to clipboard
(defun copy-rectangle-to-clipboard (p1 p2)
  "Copy region as column (rectangle) to operating system's clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 p1 p2)
    (kill-new (with-temp-buffer
                (insert-register ?0)
                (buffer-string)) ) ) )
(global-set-key (kbd "C-x r w") 'copy-rectangle-to-clipboard)

;; convert selected ASCII codes separated by semicolon to string
(defun pa23-ascii2text (start end)
  "Convert ASCII codes to text."
  (interactive "r")
  (let ((text))
    (progn
      (setq line  (buffer-substring-no-properties start end))
      (setq ascii (split-string line ";"))
      (mapc
       (lambda (code) (push (string-to-number code) text) )
       ascii) )
    (delete-region start end)
    (insert (apply 'string (reverse text))) ) )

;; convert selected text to ASCII codes separated by semicolon
(defun pa23-text2ascii (start end)
  "Convert text to ASCII codes."
  (interactive "r")
  (let ((temp) (ascii))
    (progn
      (setq temp (string-to-list (buffer-substring-no-properties start end)))
      (mapc
       (lambda (code) (setq ascii (concat ascii (number-to-string code) ";")) )
       temp) )
    (delete-region start end)
    (insert (substring ascii 0 -1)) ) )

;; convert decimal to binary
(defun pa23-dec2bin (start end)
  "Convert decimal to binary."
  (interactive "r")
  (let ((var 0) (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end)))
    (while (not (= var 0))
      (setq res (concat (if (= 1 (logand var 1)) "1" "0") res))
      (setq var (lsh var -1)) )
    (if (string= res "") (setq res "0"))
    (delete-region start end)
    (insert res) ) )

;; convert decimal to hexadecimal
(defun pa23-dec2hex (start end)
  "Convert decimal to hexadecimal."
  (interactive "r")
  (let ((var 0) (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end)))
    (setq res (upcase (format "%x" var)))
    (delete-region start end)
    (insert res) ) )

;; convert hexadecimal to binary
(defun pa23-hex2bin (start end)
  "Convert hexadecimal to binary."
  (interactive "r")
  (let ((var "") (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end) 16))
    (while (not (= var 0))
      (setq res (concat (if (= 1 (logand var 1)) "1" "0") res))
      (setq var (lsh var -1)) )
    (if (string= res "") (setq res "0"))
    (delete-region start end)
    (insert res) ) )

;; convert hexadecimal to decimal
(defun pa23-hex2dec (start end)
  "Convert hexadecimal to decimal."
  (interactive "r")
  (let ((var 0) (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end) 16))
    (setq res (number-to-string var))
    (delete-region start end)
    (insert res) ) )

;; convert binary to decimal
(defun pa23-bin2dec (start end)
  "Convert binary to decimal."
  (interactive "r")
  (let ((var 0) (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end) 2))
    (setq res (number-to-string var))
    (delete-region start end)
    (insert res) ) )

;; convert binary to hexadecimal
(defun pa23-bin2hex (start end)
  "Convert binary to hexadecimal."
  (interactive "r")
  (let ((var 0) (res ""))
    (setq var (string-to-number (buffer-substring-no-properties start end) 2))
    (setq res (upcase (format "%x" var)))
    (delete-region start end)
    (insert res) ) )

;; multiple cursor activation shortcut
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; stop creating backup~ and #autosave# files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; using hotkeys in russian keyboard layout
(defun cfg:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
(cfg:reverse-input-method 'russian-computer)

;; CEDET settings
;; (require 'cedet)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
;; (semantic-mode   t)
;; (global-ede-mode t)
;; (require 'ede/generic)
;; (require 'semantic/ia)
;; (ede-enable-generic-projects)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 99 :width normal)))))
