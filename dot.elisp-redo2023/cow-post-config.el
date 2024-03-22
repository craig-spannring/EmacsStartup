;;; Settings that should be made after loading our packages.
;;;
;;; This is a good spot for user choices, e.g. should clock be displayed?

(defcustom cow-show-clock t "Show clock in mode"
  :type 'boolean
  :group 'cow-emacs-conf)
(display-time-mode (if cow-show-clock 1 -1))

(show-paren-mode 1)

(defcustom cow-highlight-active-region nil
  "Should the active region be highlighted") 
(transient-mark-mode (if cow-highlight-active-region 1 -1))

