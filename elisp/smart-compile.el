;;; smart-compile.el --- better compile

;; Copyright (C) 1998-2001  Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Maintainer: zenitani@mac.com
;; Version: 2.0
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 20, 21

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associates a particular file with a paticular compile functions,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 20 or later.

;;; Code:


(defvar smart-compile-alist '(
  ("\\.c$"    . (compile "gcc -O2 %f -lm -o %n"))
  ("\\.[Cc]+[Pp]*$" . (compile "g++ -O2 %f -lm -o %n"))
  ("\\.java$" . (compile "javac %f"))
  ("\\.f90$"  . (compile "f90 %f -o %n"))
  ("\\.[Ff]$" . (compile "f77 %f -o %n"))
  ("\\.tex$"  . (tex-file))
  ("\\.pl$"   . (compile "perl -cw %f"))
  ("\\.cgi$"  . (compile "perl -cw %f"))
  ("\\.el$"   . (emacs-lisp-byte-compile))
)  "List of compile commands. In argument of `compile',
some keywords beginning with '%' will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

")

(defvar smart-compile-replace-alist '(
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (file-name-extension (buffer-file-name)))
))

(defun smart-compile ()
  "Better `compile'.
It calls `compile' or other compile functions.
If you set `smart-compile-alist' in your .emacs,
you can define your own compile commands."
  (interactive)
  (let ((name (buffer-file-name))
        (not-yet t))
    
    (if (not name)(error "cannot get filename."))

    ;; make?
    (if (and
         (or (file-readable-p "Makefile")
             (file-readable-p "makefile"))
         (not
          (and (local-variable-p 'compile-command)
               compile-command))
         (not
          (and (local-variable-p 'smart-compile-skip-make)
               smart-compile-skip-make))
         )
        (if (y-or-n-p "Makefile is found. Try 'make'? ")
            (set (make-local-variable 'compile-command) "make ")
            (set (make-local-variable 'smart-compile-skip-make) t)
          )
      )

    (if (and (local-variable-p 'compile-command)
              compile-command)
        (progn
          (call-interactively 'compile)
          (setq not-yet nil)
          )
      )

    ;; compile
    (let( (alist smart-compile-alist) 
          (rlist smart-compile-replace-alist)
          (case-fold-search nil)
          (function nil) )
      (while (and alist not-yet)
        (if (string-match (car (car alist)) name)
            (progn
              (setq function (cdr (car alist)))
              (if (equal (car function) 'compile)
                  (progn
                    (if (not
                         (and (local-variable-p 'compile-command)
                              compile-command))
                        (let ((command (car (cdr function))))
                          (while rlist
                            (while (string-match (car (car rlist)) command)
                              (setq command
                                    (replace-match
                                     (eval (cdr (car rlist))) t nil command)))
                            (setq rlist (cdr rlist))
                            )
                          (set (make-local-variable 'compile-command) command)
                          ))
                    (call-interactively 'compile)
                    )
                (eval function)
                )
              (setq alist nil)
              (setq not-yet nil)
              )
          (setq alist (cdr alist)) )
        ))

    ;; If compile-command is not defined and the contents begins with "#!",
    ;; set file name to the default compile-command.
    (if (and not-yet
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command))
             )
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name)
            ))
      )
    
    ;; compile
    (if not-yet (call-interactively 'compile) )

    ))

(provide 'smart-compile)

;;; smart-compile.el ends here
