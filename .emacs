

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "brown"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "gold"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "aquamarine"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "wheat"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "chocolate1")))))


;; Common Lisp settings
(require 'cl)
;;(setq-default inferior-lisp-program "~/Work/kml/trunk/CodeGen/bin/re")
(setq-default inferior-lisp-program "sbcl")


(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defvar required-packages '(slime
                            smartparens
                            auto-complete))

(defun packages-installed-p ()
  (loop for package in required-packages
        unless (package-installed-p package)
          do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (package-refresh-contents)
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))

(when (packages-installed-p)
  (require 'smartparens-config)
  (smartparens-global-mode)

  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-auto-show-menu t)
  (defvar *sources* (list
                     'lisp-mode
                     'ac-source-semantic
                     'ac-source-functions
                     'ac-source-variables
                     'ac-source-dictionary
                     'ac-source-words-in-all-buffer
                     'ac-source-files-in-current-dir))
  (let (source)
    (dolist (source *sources*)
      (add-to-list 'ac-sources source)))
  (add-to-list 'ac-modes 'lisp-mode)

  (require 'slime)
  (require 'slime-autoloads)
  (slime-setup '(slime-asdf
                 slime-fancy
                 slime-indentation))
  (setq-default slime-net-coding-system 'utf-8-unix))

(show-paren-mode t) ;; включить выделение выражений между {},[],()
;;(setq show-paren-style 'expression)    ;; выделить цветом выражения между {},[],()


(require 'imenu)
(setq imenu-auto-rescan      t) ;; автоматически обновлять список функций в буфере
(setq imenu-use-popup-menu nil) ;; диалоги Imenu только в минибуфере
(global-set-key (kbd "<f6>") 'imenu) ;; вызов Imenu на F6


(delete-selection-mode t)

(tool-bar-mode     -1) ;; отключаем tool-bar
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера

;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; отдельный список буферов при нажатии C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; запуск buffer selection кнопкой F2

; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            2) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы  
(setq scroll-conservatively 10000)

(if (equal nil (equal major-mode 'org-mode))
    (progn
      ;;(windmove-default-keybindings 'meta)
      (global-set-key (kbd "M-<left>") 'windmove-left)
      (global-set-key (kbd "M-<right>") 'windmove-right)
      (global-set-key (kbd "C-c <down>") 'windmove-down)
      (global-set-key (kbd "C-c <up>") 'windmove-up)))



(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

;;ParEdit ----------

(require 'paredit)
(require 'dash)
;;(require 's)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))


(define-key paredit-mode-map (kbd "C-<right>") nil)
(define-key paredit-mode-map (kbd "C-d") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-a") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-S-a") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-S-d") 'paredit-backward-barf-sexp)

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
;;('paredit-newline 'delete-selection t)

(provide 'setup-paredit)
