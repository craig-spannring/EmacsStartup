;;;; 
;;;; Emacs 24 changed the way the middle mouse button works.
;;;; 
;;;;    (mouse-yank-primary CLICK)   ; default for emacs24
;;;;    
;;;;       Insert the primary selection at the position clicked on.
;;;;       Move point to the end of the inserted text, and set mark at
;;;;       beginning.  If ‘mouse-yank-at-point’ is non-nil, insert at point
;;;;       regardless of where you click.
;;;;
;;;;    (mouse-yank-at-click CLICK ARG) ; default for emacs23
;;;;    
;;;;       Insert the last stretch of killed text at the position clicked
;;;;       on.  Also move point to one end of the text thus inserted
;;;;       (normally the end), and set mark at the beginning. 
;;;;       Prefix arguments are interpreted as with C-y.
;;;;       If ‘mouse-yank-at-point’ is non-nil, insert at point
;;;;       regardless of where you click.
;;;;
;;;; The new way is, in my opinion, pretty useless.
;;;; This should change it back. 
;;;;
;;;; You may also want to customize some other variables. 
;;;;      '(mouse-drag-copy-region t)
;;;;      '(x-select-enable-clipboard nil)
;;;;      '(x-select-enable-primary t)
;;;;

(when (= emacs-major-version 24)
  (global-set-key [mouse-2] 'mouse-yank-at-click))
