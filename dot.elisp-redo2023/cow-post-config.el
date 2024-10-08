;;; Settings that should be made after loading our packages.
;;;
;;; This is a good spot for user choices, e.g. should clock be displayed?

(defcustom cow-show-clock t "Show clock on mode line?"
  :type 'boolean
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (display-time-mode (if cow-show-clock 1 -1)))
  :group 'cow-emacs-conf)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)  ;; New buffers will use spaces by default
(save-place-mode 1)
(setq scroll-step 1)
(setq blink-matching-paren t)
(setq blink-matching-paren-distance (max 50000 blink-matching-paren-distance))
(setq next-line-add-newlines nil)

(defcustom cow-highlight-active-region nil
  "Should the active region be highlighted"
  :type 'boolean
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (transient-mark-mode (if cow-highlight-active-region 1 -1)))
  :group 'cow-emacs-conf)

(setenv "PAGER" "cat")

(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)

(server-start)
