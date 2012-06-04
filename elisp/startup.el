;; erik bourget
;; startup.el
;; Use me with GNU Emacs 22!

(setq load-path (append
                 '("~/.elisp")
                 '("~/.elisp/cedet-1.0.1")
                 '("~/.elisp/ecb-2.40")
                 load-path))

;;(require 'whitespace)
(require 'show-temp-buffer "~/.elisp/show-temp-buffer.el")
(require 'build-tags "~/.elisp/build-tags.el")
(require 'ctypes "~/.elisp/ctypes.el")
(require 'smart-compile "~/.elisp/smart-compile.el")
(require 'browse-kill-ring "~/.elisp/browse-kill-ring.el")
(require 'cc-mode)
(require 'nyan-mode "~/.elisp/nyan-mode.el")

; skeletons
(load "skeletons")

; non-special keybinds
(global-set-key "\M-z" 'repeat)
(global-set-key "\M-p" 'next-error)
(global-set-key "\M-u" 'undo)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\M-s" '(lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-co" 'occur) ; occur is the coolest thing since sliced bread
(global-set-key "\M-?" 'just-one-space)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\C-ck" 'browse-kill-ring)
(global-set-key [(meta c)] 'clone-indirect-buffer)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 5)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key [M-left] 'other-window)
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f3] 'ispell-buffer)
(global-set-key [f8] 'dired)

; functions defined in this file
(global-set-key "\C-c'" 'comment-current-line-or-region)
(global-set-key "\C-cr" 'replace-regexp)
(global-set-key "\C-c," 'uncomment-current-line-or-region)
(global-set-key "\C-cg" 'goto-line)
(global-set-key [M-right] 'forward-buffer)
(global-set-key [f9] 'save-and-compile)
(global-set-key "\C-c\C-c" 'save-and-compile)
(global-set-key "\C-cd" 'diff-buffer-with-associated-file)

;(global-set-key [M-left] 'backward-buffer)

; display modes
(global-font-lock-mode 1)
(auto-insert-mode 1)
(show-paren-mode)
; interactive do
; (ido-mode t)
; but I like iswitchb-buffer better
(global-set-key "\C-xb" 'iswitchb-buffer)
; and kill-current-buffer
(global-set-key "\C-xk" 'context-kill-buffer)
;(global-set-key "\C-xk" 'kill-current-buffer)

; automatically read gzip'd files
(auto-compression-mode 1)

; minibuffer autocomplete
(icomplete-mode 1)
(iswitchb-mode 1)

; minibuffer resize
;(resize-minibuffer-mode 1)

; ghetto-vision
(set-background-color "black")
(set-foreground-color "light gray")
(transient-mark-mode 1)

; fonts
(custom-set-faces
 '(show-paren-match-face ((t (:bold t
                                    :background "lightblue"
                                    :foreground "darkblue")))))
(setq initial-frame-alist
      `((background-color . "black")
	(foreground-color . "light gray")
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
 '(debian-changelog-full-name "Erik Bourget")
 '(debian-changelog-mailing-address "ebourg@cs.mcgill.ca")
 '(fill-column 80)
 '(scroll-conservatively 1000)
 ; stealth font locking: 1 second instead of 3
 '(jit-lock-stealth-time 1)
 '(frame-title-format "%b - emacs")
 '(indent-tabs-mode nil)
 '(default-tab-width 4)
 '(visible-bell t)
 '(default-major-mode 'text-mode)
 '(kill-whole-line t)
 '(inhibit-startup-message t)
 ; versioned backups
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "~/.backups")))
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(version-control t)
 '(auto-save-directory "~/.backups")
 '(auto-save-directory-fallback "~/.backups")
; '(cperl-electric-keywords t)
; '(cperl-hairy t)
 '(cperl-indent-level 4)
 '(cperl-auto-newline t)
 '(cperl-invalid-face (quote default))
 '(require-final-newline t)
 '(semantic-load-turn-everything-on t)
 '(compilation-window-height 8)
 '(c-basic-offset 4)
;; this gets annoying especially over tramp
; '(whitespace-auto-cleanup t)
; '(whitespace-global-mode 1)
; '(cursor-in-non-selected-windows nil)
 '(sgml-balanced-tag-edit t)
 '(sgml-auto-insert-required-elements t)
 '(sgml-normalize-trims t)
 '(tramp-default-method "scp")
 '(ecb-options-version "2.40")
 ; make emacs automate stuff i do anyway
 '(c-cleanup-list
   (list 'scope-operator 
         'brace-else-brace
         'brace-elseif-brace
         'brace-catch-braces
         'empty-defun-brace 
         'defun-close-semi
         'list-close-comma))
 ; colors for new types
 '(c-font-lock-extra-types '("FILE" "\\sw+_t" "Pango\\w+" "Gdk\\w+" "Gtk\\w+" "AppletWidget" "Gnome\\w+" "G\\(A\\(llocator\\|rray\\)\\|ByteArray\\|C\\(ache\\|o\\(mpletion\\|nd\\)\\)\\|D\\(at\\(e\\(Day\\|Year\\)\\|[ae]\\)\\|e\\(bugKey\\|stroyNotify\\)\\)\\|EqualFunc\\|H\\(ash\\(Func\\|Table\\)\\|ook\\(List\\)?\\)\\|IO\\(Channel\\|Funcs\\)\\|List\\|M\\(ainLoop\\|emChunk\\|utex\\)\\|Node\\|Object\\(Class\\)?\\|P\\(aramSpec\\|ollFD\\|rivatrArray\\)\\|Quark\\|Relation\\|S\\(List\\|canner\\(Config\\)?\\|ourceFuncs\\|t\\(atic\\(Mutex\\|Private\\)\\|ring\\(Chunk\\)?\\)\\)\\|T\\(hreadFunctions\\|ime\\(Val\\|r\\)?\\|okenValue\\|ree\\|uples\\)\\|Value\\)\\|g\\(boolean\\|c\\(har\\|onstpointer\\)\\|double\\|float\\|int\\(16\\|32\\|64\\|8\\)?\\|l\\(double\\|ong\\)\\|pointer\\|s\\(hort\\|ize\\|size\\)\\|u\\(char\\|int\\(16\\|32\\|64\\|8\\)?\\|long\\|short\\)\\)")))

(display-time)
; regular autocomplete
(abbrev-mode 1)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

; yes, i really mean it
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; timestamps rule
(add-hook 'write-file-hooks 'time-stamp)

; CPerl rules
(add-to-list 'auto-mode-alist '("\\.pov$" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

; autoloads
(autoload 'cperl-mode "cperl-mode" "" t)
(autoload 'python-mode "python-mode" "" t)
(autoload 'pov-mode "pov-mode" "" t)
(autoload 'go-mode "go-mode" "" t)

; stupid mouse
(mouse-avoidance-mode 'jump)

(menu-bar-mode -1) ; kill menubar

; reclaim screen space
(if (and (>= emacs-major-version 21)
         (not (eq window-system nil)))
    (progn
      (global-linum-mode 1)
      (require 'cedet)
      (require 'semantic/analyze)
      (provide 'semantic-analyze)
      (provide 'semantic-ctxt)
      (provide 'semanticdb)
      (provide 'semanticdb-find)
      (provide 'semanticdb-mode)
      (provide 'semantic-load)
      (require 'ecb)

      (setq nyan-wavy-trail t) ;; NYAN CAT
      (nyan-mode)
      (nyan-start-animation)

      (tool-bar-mode -1) ; kill toolbar
      (scroll-bar-mode -1) ; kill scrollbar
      (blink-cursor-mode -1) ; blinking sucks
      (set-face-attribute 'default nil :family "Menlo" :height 145 :weight 'normal)
      ;(set-face-font 'default
;"-*-lucidatypewriter-medium-*-*-*-12-*-*-*-*-*-*-*")
;"-dec-terminal-medium-r-normal-*-*-140-*-*-c-*-iso8859-1"
;"nh10"
      ))

; make all yes/no prompts y/n instead
(fset 'yes-or-no-p 'y-or-n-p)

; smart-compile is cool
(defun save-and-compile ()
  "Save current buffer and make."
  (interactive "")
  (save-some-buffers 0)
  (smart-compile))

(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-c\C-c" 'save-and-compile)))

; hide-show mode
(add-hook 'c-mode-common-hook
          (lambda () (hs-minor-mode 1)))

(add-hook 'hs-minor-mode-hook
          (lambda () (local-set-key "\C-cs" 'hs-show-block)))
(add-hook 'hs-minor-mode-hook
          (lambda () (local-set-key "\C-ch" 'hs-hide-block)))
(add-hook 'hs-minor-mode-hook
          (lambda () (local-set-key "\C-cS" 'hs-show-all)))
(add-hook 'hs-minor-mode-hook
          (lambda () (local-set-key "\C-cH" 'hs-hide-all)))

; cc-mode behavior
(ctypes-auto-parse-mode 1) ; this rules - automatically spider and find 
                           ; typedefs to highlight
(add-hook 'c-mode-common-hook
          (lambda () (progn
                       (local-set-key "\C-m" 'newline-and-indent)
                       (turn-on-auto-fill)
                       (c-set-offset 'case-label '+)
                       (c-toggle-auto-newline 1))))
                        
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'java-mode-hook
          '(lambda () 
             (c-set-style "linux")
             (setq c-basic-offset 2)))

;==============================================================================

;==============================================================================
; bash-style tab completion
(defun indent-or-complete ()
  "Complete if point is at end of a line, otherwise indent line."
  (interactive)
  (if (and (looking-at "\\>"))
      (hippie-expand nil)
    (indent-for-tab-command)))

(add-hook 'python-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "<tab>") 'indent-or-complete))))
(add-hook 'c-mode-common-hook
          (function (lambda ()
                        (local-set-key (kbd "<tab>") 'indent-or-complete))))

(add-hook 'c-mode-common-hook
	  (lambda () (c-toggle-auto-hungry-state 1)))

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
; kill current buffer
(defun kill-current-buffer (arg)
  "Kill current buffer."
  (interactive "p")
  (when (and (buffer-modified-p)
             (not (string-match "\\*.*\\*" (buffer-name)))
             (= 1 arg))
    (if (y-or-n-p (format "<%s> is modified, save " (buffer-name)))
        (save-buffer)))
    (let ((buffer-modified-p nil))
      (kill-buffer (current-buffer))))

;==============================================================================
; automatically generate etags
(defadvice find-tag (before c-tag-file activate)
  "Automatically create tags file."
  (let ((tag-file (concat default-directory "TAGS")))
    (unless (file-exists-p tag-file)
      (shell-command "etags *.[ch] -o TAGS 2>/dev/null"))
    (visit-tags-table tag-file)))

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
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
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

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                                 (buffer-name)
                                 "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

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
