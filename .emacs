;; -*-Emacs-Lisp-*-

;;    pa23's emacs configuration file    ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
;; '(jabber-auto-reconnect t)
;; '(jabber-history-enabled t)
;; '(jabber-keepalive-interval 60)
;; '(jabber-roster-default-group-name "google")
;; '(jabber-show-resources nil)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; каталог с дополнительными модулями
(add-to-list 'load-path "~/.site-lisp")

;; отключить заставку
(setq inhibit-startup-message t)

;; упрощенное подтверждение команд
(fset 'yes-or-no-p 'y-or-n-p)

;; подсвечивать текущую строку
(global-hl-line-mode 1)
(set-face-background 'highlight "#E5E5FF")

;; плавная прокрутка текста с помощью клавиш стрелок
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

;; деактивация ускорения прокрутки мышью
(setq mouse-wheel-progressive-speed nil)

;; шаг прокрутки экрана мышью
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

;; установить UTF-8 в качестве рабочей кодировки
(set-language-environment 'UTF-8)

;; автоопределение кодировки
;; (требует установленной в системе enca, а также наличия auto-enca в */.site-lisp)
(when (load "auto-enca" 'noerror)
  (modify-coding-system-alist 'file "" 'enca-detect-coding))

;; режим переноса по умолчанию
(global-visual-line-mode t)

;; не использовать табы для отступов
(setq-default indent-tabs-mode nil)

;; установить ширину табуляции в 4 символа
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; установить ширину отступа в 4 символа
;;(setq-default standard-indent 4)

;; установить стиль для исходных кодов на С и С++
(setq c-default-style '((java-mode . "java") (other . "stroustrup")))

;; прокручивать буфер компиляции во время вывода
(setq compilation-scroll-output 1)

;; активация режима cmake
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(autoload 'cmake-mode "cmake-mode.el" t)

;; загрузка и настройка диалога переключения буферов
;; (требует наличия popup.el и popup-switcher.el в */.site-lisp)
(require 'popup-switcher)
(setq psw-in-window-center t)
(global-set-key [f2] 'psw-switch-buffer)

;; закрытие всех буферов одной командой
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
