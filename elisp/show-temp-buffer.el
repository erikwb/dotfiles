;; Improved help-window display.
;; Copyright (C) 1989 Free Software Foundation, Inc.
;; Copyright (C) 1989, 1991, 1993, 1994 by Joe Wells

;; This file is not officially part of GNU Emacs.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves any
;; particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.
;; 
;; Everyone is granted permission to copy, modify and redistribute this
;; file, but only under the conditions described in the GNU Emacs General
;; Public License, with one exception mentioned below.  A copy of this
;; license is supposed to have been given to you along with GNU Emacs so
;; you can know your rights and responsibilities.  It should be in a file
;; named COPYING.  Among other things, the copyright notice and this
;; notice must be preserved on all copies.
;; 
;; Permission to redistribute this file in a public archive site is not
;; granted unless the file's name contains "show-temp-buffer.el".  I will
;; lift this restriction if someone pays me $25 to do it.  See, I can be
;; agreeable.  :-)
;;
;; Explanation of why this is copyrighted by both the FSF and me:
;; The core of the code was derived from the C code that was invoked by
;; Emacs when show-temp-buffer-hook was nil.  So they own the copyright on
;; that portion of the expression.  I then added lots and lots of features
;; to it.

;; Created by: Joe Wells, jbw@bucsf
;; Created on: 1988?
;; Last modified by: Joe Wells, jbw@csd
;; Last modified on: Mon Mar  7 23:11:25 1994
;; Filename: show-temp-buffer.el
;; Purpose: enhanced display of help windows
;; Change log: 
;; 
;; Mon Mar  7 22:17:25 1994  Joe Wells  (jbw at csd.bu.edu)
;; 
;; 	* Fix hide-temp-buffers to work when current buffer is a
;; 	  temp-buffer, in which case it now hides the current buffer as
;; 	  well.
;; 
;; Mon Mar  7 21:45:53 1994  Joe Wells  (jbw at csd.bu.edu)
;; 
;;      * Added changes by Jeff Sparkes <jsparkes@bnr.ca> to support the
;;        hide-temp-buffers function to get rid of all temp-buffer windows
;;        at once.
;; 
;; Fri Nov 12 21:46:42 1993  Joe Wells  (jbw at csd)
;; 
;;      * Ported to Emacs 19 primarily by Mosur Mohan
;;        <mosurm@wv.MENTORG.COM>.
;; 
;; Mon Aug  9 18:44:47 1993  Joe Wells  (jbw at csd)
;; 
;;      * Paranoia regarding process-mark, which may sometimes not point
;;        anywhere.
;; 
;; Thu Mar 11 20:14:31 1993  Joe Wells  (jbw at bigbird.bu.edu)
;; 
;; 	* Process output will now carry along point if point is at the end
;; 	  of the most recent process output.  Point will be initially set
;; 	  to the end of the processes output (if any) or to point max when
;; 	  there is a buffer process.
;; 
;; Wed Aug 21 12:55:29 1991  Joe Wells  (jbw at maverick.uswest.com)
;; 
;; 	* More comments and updated doc strings.
;; 
;; Mon Jul 22 18:09:04 1991  Joe Wells  (jbw at maverick.uswest.com)
;; 
;; 	* Fixed two bugs: sometimes deleted current window by enlarging
;; 	other window, when reselecting current window this caused an
;; 	error.  Also, can't resize window above a window when the window
;; 	is the top window!
;; 
;; Thu Jun 13 11:45:18 1991  Joe Wells  (jbw at teton.uswest.com)
;; 
;; 	* Added handling of processes that add to buffers' contents.
;; 
;; Wed May 22 19:46:39 1991  Joe Wells  (jbw at bucsd)
;; 
;; 	* Avoid increasing the size of the next window after newly
;; 	popped-up window.
;; 
;; Sat Dec  8 17:52:01 1990  Joe Wells  (jbw at bigbird)
;; 
;; 	* Formerly, vertical-motion always worked on the current buffer,
;; 	but in Epoch it works on the buffer of the selected window.  Also,
;; 	screen-height now returns the screen-height of the screen in which
;; 	the selected window occurs.  Fixes for both of these changes.
;; 

;; LCD Archive Entry:
;; show-temp-buffer|Joe Wells|jbw@cs.bu.edu|
;; Display temporary pop-up buffers in appropriately sized windows.
;; |March 7, 1994|||

;; Features:
;;
;; 1. Automatically works with all functions that call
;; with-output-to-temp-buffer by setting the value of
;; temp-buffer-show-hook (temp-buffer-show-function in Emacs 19).
;;
;; 2. Ensures the current directory of the temp buffer is the current
;; directory of the current buffer when the temp buffer is first displayed.
;;
;; 3. Automatically resizes the temp buffer's window to exactly hold the
;; text in the buffer.  This is done within customizable limits that
;; default to between half the screen and 1 line.
;;
;; 4. Ensures no blank lines show at the end or the beginning of the
;; temp buffer.  Does not modify the temp buffer to accomplish this.
;;
;; 5. Resizes temp buffers receiving process output as the output arrives.
;; This works only for temp buffers whose processes have been started
;; before with-output-to-temp-buffer is called.
;; 
;; 6. The hide-temp-buffer command will hide all of the temp buffers at
;; once.  It will be bound to "C-x t" if that key sequence is not already
;; used.

;; Known Problems:
;;
;; 1. Some people don't like having a lot of temp buffers pop up on their
;; screen.  This is a feature.
;;
;; 2. Space for the new window is not evenly stolen from all the existing
;; windows, but will be disproportionately taken from one or two windows.
;; Over time with many temp buffer windows this evens out.  Someday I'll
;; write the code to steal the space evenly.  (This problem may have
;; changed or gone away in Emacs 19.)
;;
;; 3. The new window's space is generally taken from the bottom of the
;; largest window, which might not be the best place on the screen to put
;; it.  I'd like the new window to show up below the selected window.
;; (This problem may have changed or gone away in Emacs 19.)
;;
;; 4. The message "Type C-x 4 b RET to restore old contents of help
;; window" is now wrong.
;;
;; 5. It no longer works with compilation buffers in Emacs 19 because the
;; relevant code in compile.el no longer calls with-output-to-temp-buffer.

;; Differences with popper.el:
;; 
;; 1. popper.el unconditionally overrides the keybinding for C-z.
;; 
;; 2. popper.el displays only one temporary buffer at a time.  This is
;; a deliberate feature of popper.
;; 
;; 3. popper.el doesn't allow you to switch to the window of a temporary
;; buffer with C-x o.  Thus, you will have difficulty cutting and pasting
;; text from temporary buffers.

;; The ideas for this package were derived from the C code in src/window.c
;; and from the Lisp code in lisp/ehelp.el.  Mike Patton <map@lcs.mit.edu>
;; and Dave Lawrence <tale@cs.rpi.edu> contributed the idea and code for
;; temp-buffer-{min,max}-height-form.  Jeff Sparkes <jsparkes@bnr.ca>
;; contributed hide-temp-buffers.

(defvar temp-buffer-override-defaults t
  "*Non-nil means override default values of window-min-height and
split-height-threshold.  Recommended because it allows much more
flexibility in displaying temp buffers.")

(defvar temp-buffer-max-height-form '(/ (- screen-height 2) 2)
  "*Maximum window height (text + mode-line) for temp buffers.
This form is evalled each time it is used by show-temp-buffer.
The symbol screen-height will hold the proper screen's height.")

(defvar temp-buffer-min-height-form 2
  ;;'(/ (- screen-height 2) 10) ; 1/10th
  "*Minimum window height (text + mode-line) for temp buffers.
This form is evalled each time it is used by show-temp-buffer.
The symbol screen-height will hold the proper screen's height.")

;; Changes for Emacs 19 based on suggestions by Mosur Mohan.
(if (boundp 'temp-buffer-show-function)
    (or temp-buffer-show-function
        (setq temp-buffer-show-function 'show-temp-buffer)))
(if (boundp 'temp-buffer-show-hook)
    (or temp-buffer-show-hook
        (setq temp-buffer-show-hook 'show-temp-buffer)))

;; Per-buffer variable recording the buffer's process's original filter.
;; It is assumed that the buffer only has one process.
(defvar original-buffer-process-filter nil)
(make-variable-buffer-local 'original-buffer-process-filter)

;; Per-buffer variable recording the buffer's process's original sentinel.
;; It is assumed that the buffer only has one process.
(defvar original-buffer-process-sentinel nil)
(make-variable-buffer-local 'original-buffer-process-sentinel)

;; Whether this buffer has been displayed via temp-buffer-show-function.
;; This variable is buffer-local.
(defvar temp-buffer-p nil)
(make-variable-buffer-local 'temp-buffer-p)

(defun show-temp-buffer (buffer &optional just-resize)
  "Display BUFFER on the screen in a window of appropriate size.
If JUST-RESIZE is non-nil, then certain initialization of the buffer is
not done.  See temp-buffer-max-height-form and temp-buffer-min-height-form."
  (let* ((current-window (selected-window))
	 (window (display-buffer buffer))
         ;; Fix for this expression for Emacs 19 suggested by Mosur Mohan.
	 (screen-height (if (fboundp 'window-frame)
                            (screen-height (window-frame window))
                          (if (fboundp 'epoch::screen-height)
                              (epoch::screen-height (screen-of-window window))
                            (screen-height))))
	 (window-lines (window-height window))
	 ;; Check if we can safely resize this window
	 (minibuffer-line (nth 1 (window-edges (minibuffer-window))))
	 (window-edges (window-edges window))
	 ;; Is this insufficient with my new resizing strategy?
	 ;; TODO: fix this for all cases in Epoch
	 (minibuffer-safe (not (and (= (nth 1 window-edges) 0)
				    (= (nth 3 window-edges)
				       minibuffer-line))))
	 (current-directory default-directory)
	 (window-min-height
	  (if temp-buffer-override-defaults 2 window-min-height))
	 (split-height-threshold
	  (if temp-buffer-override-defaults 4 split-height-threshold))
	 window-lines-needed max-height min-height size-change
	 candidate-start-point safe-window-config)
    
    ;; Ensure sane initial window configuration
    (cond ((not just-resize)
	   (setq minibuffer-scroll-window window)
	   (set-window-hscroll window 0)))
    (save-excursion
      (set-buffer buffer)
      ;; Yes, you really need two nested save-excursion commands!
      (save-excursion
	(save-restriction
	  (or just-resize
	      (widen))
	  ;;(set-buffer buffer)		; needed?
	  
	  ;; Find the beginning of useful text
	  (goto-char (point-min))
	  (skip-chars-forward " \t\n")
	  (skip-chars-backward " \t")
	  (narrow-to-region (point) (point-max))
	  (setq candidate-start-point (point))
	  
	  ;; If it is safe to resize, then do so
	  (cond (minibuffer-safe
		 ;; Find the end of useful text
		 (goto-char (point-max))
		 (skip-chars-backward " \t\n")
		 (narrow-to-region (point-min) (point))
		 (goto-char (point-min))
		 
		 ;; Make sure temp-buffer max and min height within limits
		 (setq max-height (min (eval temp-buffer-max-height-form)
				       (max (- screen-height 2)
					    2)))
		 (setq min-height (max (eval temp-buffer-min-height-form)
				       window-min-height))
		 
		 ;; Calculate vertical lines needed by text
		 (unwind-protect
		     (progn
		       (setq safe-window-config (current-window-configuration))
		       (select-window window)
		       ;; Fucking select-window can bash point back to the
		       ;; value of window-point.
		       (goto-char (point-min))
		       (setq window-lines-needed
			     (max (+ 2 (vertical-motion (- max-height 2)))
				  min-height)) ; ensure new size isnt too small
		       (goto-char (point-min))
		       
		       ;; Do the resize operation
		       (setq size-change (- window-lines-needed window-lines))
		       ;; If we resize the window whose size we want to
		       ;; change, this enlarges the window below, which is
		       ;; usually a temp buffer.  So instead we resize the
		       ;; window above this window by the opposite amount
		       ;; of the desired change in this window.  This
		       ;; usually does what is wanted.  Of course we can't
		       ;; do this if the window we want to resize is the
		       ;; top window.

		       ;; TODO:
		       ;; No, this is wrong.  Divide the amout of resizing
		       ;; done in this window and the previous window
		       ;; depending on the size of the previous window.
		       (if (eq (previous-window) (minibuffer-window))
			   (enlarge-window size-change)
			 (select-window (previous-window))
			 (enlarge-window (- size-change))))
		   ;; Check if the current window was deleted before
		   ;; reselecting it.
		   (if (numberp (condition-case nil
				    (window-point current-window)
				  (error nil)))
		       (select-window current-window)
		     ;; Oh well, we couldn't resize after all.
		     ;; TODO: retry with smaller size
		     (set-window-configuration safe-window-config)))))))
      
      ;; Scroll window just past initial blank lines if that leaves point
      ;; visible.
      (let ((original-window-start (window-start window)))
	(set-window-start window candidate-start-point)
	;; can get interrupted in time gap here ... hmm
	(or (pos-visible-in-window-p (point) window)
	    (set-window-start window original-window-start)))
      
      ;; Change temp buffer's default directory based on previous buffer
      (or just-resize
	  (setq default-directory current-directory))
      
      ;; Mark buffer as a temp-buffer so the command to hide temp buffers
      ;; can work.
      (setq temp-buffer-p t)

      ;; Arrange process filter and sentinel to resize window as buffer changes
      (or just-resize
	  ;; Clear any recorded filters or sentinels from prior use of the
	  ;; same buffer.
	  (setq original-buffer-process-filter nil
		original-buffer-process-sentinel nil))
      (let* ((process (get-buffer-process (current-buffer)))
	     (filter (if process (process-filter process)))
	     (sentinel (if process (process-sentinel process))))
	(cond
	 (process
	  
	  ;; We need to try overloading not just when the window is
	  ;; initially displayed but also later in case the process's
	  ;; sentinel is set after calling with-output-to-temp-buffer.
	  (if (or original-buffer-process-filter
		  (eq filter 'temp-buffer-process-filter))
	      nil
	    (setq original-buffer-process-filter (or filter t))
	    (set-process-filter process 'temp-buffer-process-filter))
	  (if (or original-buffer-process-sentinel
		  (eq sentinel 'temp-buffer-process-sentinel))
	      nil
	    (setq original-buffer-process-sentinel (or sentinel t))
	    (set-process-sentinel process 'temp-buffer-process-sentinel))
	  
	  ;; Arrange for most recent process output to always be visible
	  ;; by default by moving point to the process mark or to the end
	  ;; of the buffer.
	  ;; *** Should this be optional?
	  (goto-char (or (marker-position (process-mark process))
                         (point-max)))))))
    
    ;; Prevent buffer from being default buffer for switching
    ;; Must do this after select-window
    (bury-buffer buffer)
    
    ;; return window for compatibility with display-buffer
    window))

;; Overloaded process filter to handle window resizing.
(defun temp-buffer-process-filter (process string)
  (temp-buffer-process-handler process string nil))

;; Overloaded process sentinel to handle window resizing.
(defun temp-buffer-process-sentinel (process string)
  (temp-buffer-process-handler process string t))

;; Overloaded process filter or sentinel to handle window resizing.
;; Called as a filter or sentinel with arguments PROCESS, STRING, and
;; SENTINEL-P which is true if it should behave as a sentinel, false if it
;; should behave as a filter.  If PROCESS originally had no filter (or
;; sentinel) imitates the default handling of process output or state
;; changes.  Otherwise the original filter (or sentinel) is called.  After
;; this, the process's buffer is displayed (if not already) in a window
;; and its window is resized using show-temp-buffer.
(defun temp-buffer-process-handler (process string sentinel-p)
  (let ((buffer (process-buffer process)))
    (or buffer (error "Process %s no longer has a buffer" process))
    (if (not (buffer-name buffer))
	(or (and sentinel-p
		 (memq (process-status process) '(exit closed)))
	    (error "Buffer %s of process %s killed" buffer process))

      ;; Arrange for inserting the string in the buffer.
      (save-excursion
	(set-buffer buffer)
	(let ((original-function (if sentinel-p
				     original-buffer-process-sentinel
				   original-buffer-process-filter)))
	  (if (memq original-function '(nil t))
	      (let* ((process-mark (process-mark process))
		     (point-at-process-mark-p
                      (= (point)
                         (or (marker-position process-mark)
                             -1)))
		     (marker-buffer (marker-buffer process-mark))
		     buffer-read-only)
		(save-excursion
		  (if (and marker-buffer
			   ;; *** Shouldn't I just raise an error if these
			   ;; *** don't match???
			   (eq marker-buffer buffer))
		      (goto-char process-mark)
		    (goto-char (point-max)))
		  (if sentinel-p
		      (insert "\nProcess "
			      (process-name process)
			      " "
			      string)
		    (insert string))
		  (set-marker process-mark (point)))
		
		;; This produces the desirable behavior of
		;; always showing the latest output from the process if
		;; point was already after the last process output or at
		;; the end of the buffer.
		(cond (point-at-process-mark-p
		       (goto-char process-mark)
		       ;; *** Why do I need this?
		       (let ((window (get-buffer-window buffer)))
			 (if window (set-window-point window (point))))))
		
		;; This updates the mode line.
		(set-buffer-modified-p (buffer-modified-p)))
	    
	    ;; Otherwise let the original process filter handle things.
	    (funcall original-function process string))))
      
      ;; This results in any necessary window display and resizing.
      (show-temp-buffer buffer t))))

;; A possible key-binding for this is "C-x t", but that does not seem very
;; mnemonic.  Other suggestions?
;; Avoid trashing user's key binding.
(if (null (global-key-binding "\C-xt"))
    (global-set-key "\C-xt" 'hide-temp-buffers))

;; By Jeff Sparkes <jsparkes@bnr.ca>.
(defun hide-temp-buffers ()
  "Delete windows associated with temporary buffers.
Temporary buffers are any that have been displayed via
temp-buffer-show-function."
  (interactive)
  (mapcar (function (lambda (buffer)
                      (if (save-excursion
                            (set-buffer buffer)
                            ;; read the buffer-local value
                            temp-buffer-p)
                          (delete-windows-on buffer))))
          (buffer-list)))

(provide 'show-temp-buffer)
