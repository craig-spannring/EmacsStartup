;;; Settings that should be made after loading our packages.
;;;
;;; This is a good spot for user choices, e.g. should clock be displayed?

(defcustom cow-show-clock t "Show clock in mode"
  :type 'boolean
  :group 'cow-emacs-conf)
(display-time-mode (if cow-show-clock 1 -1))

