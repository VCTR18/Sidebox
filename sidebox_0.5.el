;;; sidebox --- Summary
;;; Commentary:
;; The code below allows me to have minimap on top of a speedbar without interfering
;; on any other process.
;; It can be turn off or on, you can customize the persistance in other cases
;; It uses the width provided by minimap and the vertical relation between the components is
;; customizable.


;;; Code:

(require 'speedbar)
(require 'minimap)
(require 'sr-speedbar)
  
(dotimes (i 2) (sr-speedbar-toggle))

(defvar sidebox-blacklist '(" *MINIMAP*" " *SPEEDBAR*" " *Minibuf-1*" " *Minibuf-0*"))
(defvar sidebox-current-buffer (current-buffer))
(defvar sidebox-temporal-buffer nil)
(defvar sidebox-lower-window nil)
(defvar sidebox-current-window nil)
(defvar sidebox-minimap-dead nil)
(defvar sidebox-should-be-on-p nil)
(defvar sidebox-first-time t)

(defgroup sidebox nil
  "Useful arrangement for minimap and speedbar"
  :group 'convenience)

(defcustom sidebox-vertical-relation 11
  "Standard screen relation between minimap (top) and speedbar (bottom)."
  :type 'number
  :group 'sidebox)
(defcustom sidebox-never-close nil
  "Non-nil values means the window arrangement can only be killed through commands."
  :type 'boolean
  :group 'sidebox)

(defun sidebox-main ()
  "Set speedbar below minimap."
  (when (minimap-get-window)
    (minimap-kill)
    (cancel-function-timers 'minimap-update))
  
  (when (get-buffer-window sr-speedbar-buffer-name)
    (delete-window (get-buffer-window sr-speedbar-buffer-name)))

  (setq sidebox-should-be-on-p nil)

  (when (derived-mode-p 'prog-mode)
    (minimap-create)
    (select-window (minimap-get-window))
    (setq sidebox-lower-window (split-window-vertically sidebox-vertical-relation))
    (select-window sidebox-lower-window)
    (switch-to-buffer (get-buffer sr-speedbar-buffer-name))
    (goto-char (point-min))
    ;; (set-window-parameter nil 'no-other-window t)
    (set-window-dedicated-p nil t)
    (if (setq sidebox-current-window (get-buffer-window sidebox-current-buffer))
	(select-window sidebox-current-window)
      (select-window (next-window))
      (switch-to-buffer sidebox-current-buffer))
    (setq sidebox-should-be-on-p t)))

(defun sidebox-check-state ()
  "Check if the buffer has changed."
  (interactive)
  (setq sidebox-temporal-buffer	(current-buffer))
  
  (unless (member (format "%s" sidebox-temporal-buffer) sidebox-blacklist)
    (if (eq sidebox-temporal-buffer sidebox-current-buffer)
	(progn (when (and sidebox-should-be-on-p
			  (null (get-buffer-window minimap-buffer-name)))
		 (setq sidebox-current-buffer sidebox-temporal-buffer)
		 (if (null sidebox-never-close)
		     (cancel-function-timers 'minimap-update)
		   (sidebox-main)))
	       (when sidebox-first-time
		 (setq sidebox-first-time nil)
	    (sidebox-main)))
      (setq sidebox-current-buffer sidebox-temporal-buffer)
      (sidebox-main))))


(defun sidebox-off ()
    "Turn sidebox off."
  (interactive)
  (cancel-function-timers 'sidebox-check-state)
  (when (minimap-get-window)
    (minimap-kill)
    (cancel-function-timers 'minimap-update))
  
  (when (get-buffer-window sr-speedbar-buffer-name)
    (delete-window (get-buffer-window sr-speedbar-buffer-name))))

(defun sidebox-on ()
  "Turn sidebox on."
  (interactive)
  (setq sidebox-current-buffer (current-buffer))
  (setq sidebox-first-time t)
  (run-with-timer 0 0.1 'sidebox-check-state))

(run-with-timer 1 0.1 'sidebox-check-state)

;; (define-prefix-command 'sidebox-keymap)
;; (setq sidebox-keymap (kbd "C-ñ"))
(global-set-key (kbd "C-ñ C-p") 'sidebox-on)
(global-set-key (kbd "C-ñ C-o") 'sidebox-off)


(provide 'sidebox)

;;; sidebox_0.5.el ends here
