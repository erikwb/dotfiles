;; erik bourget
;; startup.el

(add-to-list 'load-path "~/.elisp")

(require 'smart-compile)
(require 'browse-kill-ring)
(require 'cc-mode)
(require 'yaml-mode)
(require 'inf-ruby)
(require 'python)
(require 'nyan-mode)
(require 'go-mode)

(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-cs" '(lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key "\C-co" 'occur)
(global-set-key "\C-c/" 'hippie-expand)
(global-set-key "\C-ck" 'browse-kill-ring)
(global-set-key [(meta c)] 'clone-indirect-buffer)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 5)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key "\C-c'" 'comment-current-line-or-region)
(global-set-key "\C-cr" 'replace-regexp)
(global-set-key "\C-c," 'uncomment-current-line-or-region)
(global-set-key "\C-cg" 'goto-line)
(global-set-key [M-right] 'forward-buffer)
(global-set-key "\C-c\C-c" 'save-and-compile)
(global-set-key "\C-cd" 'diff-buffer-with-associated-file)
(global-set-key "\C-xb" 'iswitchb-buffer)
(global-set-key "\C-xk" 'context-kill-buffer)

(global-font-lock-mode 1) ; font coloring
(auto-insert-mode 1)
(show-paren-mode)
(auto-compression-mode 1) ; automatically read gzip'd files
(icomplete-mode 1) ; autocomplete in minibuffer
(iswitchb-mode 1) ; and for switching buffers
(transient-mark-mode 1) ; highlight selected regions
(display-time)
(menu-bar-mode -1)
(server-start) ; for emacsclient from CLI

; fonts
(custom-set-faces
 '(show-paren-match-face ((t (:bold t
                                    :background "lightblue"
                                    :foreground "darkblue")))))
(setq initial-frame-alist
      `((background-color . "#111")
        (foreground-color . "#ccc")
        (horizontal-scroll-bars . nil)
        (vertical-scroll-bars . nil)))
(setq default-frame-alist (copy-alist initial-frame-alist))

; army of setq
(custom-set-variables
 '(display-time-24hr-format t)
 '(browse-kill-ring-resize-window t)
 '(show-paren-delay 0)
 '(show-paren-style 'parenthesis)
 '(message-log-max 100)
 '(font-lock-maximum-decoration t)
 '(line-number-mode t)
 '(column-number-mode t)
 '(mouse-yank-at-point t)
 '(c-default-style "k&r")
 '(fill-column 80)
 '(scroll-conservatively 100000)
 ; stealth font locking: 1 second instead of 3
 '(jit-lock-stealth-time 1)
 '(frame-title-format "%b - emacs")
 '(indent-tabs-mode nil)
 '(default-tab-width 4)
 '(visible-bell t)
 '(default-major-mode 'text-mode)
 '(kill-whole-line t) ; don't make me hit C-k twice to really kill a line
 '(inhibit-startup-message t)
 ; versioned backups, not littering the working directory
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "~/.backups")))
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(version-control t)
 '(auto-save-directory "~/.backups")
 '(auto-save-directory-fallback "~/.backups")
 '(cperl-indent-level 4)
 '(cperl-auto-newline t)
 '(cperl-invalid-face (quote default))
 '(require-final-newline t)
 '(compilation-window-height 8)
 '(c-basic-offset 4)
 '(tramp-default-method "scp")
 ; make emacs automate stuff i do anyway
 '(c-cleanup-list
   (list 'scope-operator 
         'brace-else-brace
         'brace-elseif-brace
         'brace-catch-braces
         'empty-defun-brace 
         'defun-close-semi
         'list-close-comma))
)

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
; make all yes/no prompts y/n instead
(fset 'yes-or-no-p 'y-or-n-p)

; reclaim screen space
(if (and (>= emacs-major-version 21)
         (not (eq window-system nil)))
    (progn
      (global-linum-mode 1)

      ; nyancat progress bar!
      (setq nyan-wavy-trail t)
      (nyan-mode)
      (nyan-start-animation)

      (tool-bar-mode -1) ; kill toolbar
      (scroll-bar-mode -1) ; kill scrollbar
      (blink-cursor-mode -1) ; blinking sucks
      (set-face-attribute 'default nil 
                          :family "Menlo" 
                          :height 145 
                          :weight 'normal)
      ))

(defun save-and-compile ()
  "Save current buffer and make."
  (interactive "")
  (save-some-buffers 0)
  (smart-compile))

; hide-show mode - code folding
(add-hook 'hs-minor-mode-hook
          (lambda () (progn
                       (local-set-key "\C-cs" 'hs-show-block)
                       (local-set-key "\C-ch" 'hs-hide-block)
                       (local-set-key "\C-cS" 'hs-show-all)  
                       (local-set-key "\C-cH" 'hs-hide-all))))

; bash style tab completion when you hit <tab>
(defun indent-or-complete ()
  "Complete if point is at end of a line, otherwise indent line."
  (interactive)
  (if (and (looking-at "\\>"))
      (hippie-expand nil)
    (indent-for-tab-command)))


;==============================================================================
; language-specific hooks
(add-hook 'c-mode-common-hook
          '(lambda ()
             (hs-minor-mode 1)
             (local-set-key (kbd "<tab>") 'indent-or-complete)
             (local-set-key "\C-m" 'newline-and-indent)
             (turn-on-auto-fill)
             (c-set-offset 'case-label '+)
             (c-toggle-auto-hungry-state 1)
             (c-toggle-auto-newline 1)
             ))

; 2-character indent for Java
(add-hook 'java-mode-hook
          '(lambda () 
             (c-set-style "linux")
             (local-set-key (kbd "<tab>") 'indent-or-complete)
             (setq c-basic-offset 2)
             ))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (local-set-key (kbd "<tab>") 'indent-for-tab-command)
             (local-set-key "\C-m" 
                            'reindent-then-newline-and-indent)
             (local-set-key "\C-c\C-r"
                            'run-ruby)
             ))

; auto-wrap to 80 chars in text modes                        
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;==============================================================================
; buffer cycling
(defun forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
     (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
    (setq list (cdr list))
    (setq buffer (car list))))
    (switch-to-buffer buffer)))

;==============================================================================
; line commenting
(defun start-of-current-line-or-region ()
  (save-excursion
    (if mark-active
        (progn
          (goto-char (region-beginning))
          (point-at-bol))
      (point-at-bol))))

(defun end-of-current-line-or-region ()
  (save-excursion
    (if mark-active
        (progn
          (goto-char (region-end))
          (point-at-eol))
      (point-at-eol))))

(defun comment-current-line-or-region () (interactive)
  "Comment current line or region."
  (progn
    (comment-region (start-of-current-line-or-region)
                    (end-of-current-line-or-region))
    (end-of-current-line-or-region)
    (indent-region (start-of-current-line-or-region)
                   (end-of-current-line-or-region)
                   nil)))

(defun uncomment-current-line-or-region () (interactive)
  "Uncomment current line or region."
  (progn
    (comment-region (start-of-current-line-or-region)
                    (end-of-current-line-or-region) -2)
    (end-of-current-line-or-region)
    (indent-region (start-of-current-line-or-region)
                   (end-of-current-line-or-region)
                   nil)))

;==============================================================================
; diff buffers when ask to save
(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) 
                              tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (list buf-filename tempfile)))
                  (message "No differences found")
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf))))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))
      nil)) 

;; tidy up diff buffers when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                                 (buffer-name)
                                 "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

;; show us a diff of the buffer and the file when we close it
(defun context-kill-buffer (arg)
  "Kill buffer, taking gnuclient into account."
  (interactive "p")
  (when (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (when (file-exists-p buffer-file-name)
      (diff-buffer-with-associated-file))
    (error "Buffer has unsaved changes"))
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))
