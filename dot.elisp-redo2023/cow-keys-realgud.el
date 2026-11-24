;; -*- lexical-binding: t; -*-

;;; Setup keys to make it easier to step in gdb
;;



;; (define-key c++-mode-map [kp-right] 'gud-next)
;; (define-key c++-mode-map [kp-down]  'gud-step)
;; (define-key c++-mode-map [kp-up]    'gud-finish)
;; (define-key c-mode-map   [kp-right] 'gud-next)
;; (define-key c-mode-map   [kp-down]  'gud-step)
;; (define-key c-mode-map   [kp-up]    'gud-finish)
;; (define-key gud-mode-map [kp-right] 'gud-next)
;; (define-key gud-mode-map [kp-down]  'gud-step)
;; (define-key gud-mode-map [kp-up]    'gud-finish)

(when (equal system-type 'darwin)
  ;; Special keyboard setup for a Mac.
  (define-key c++-mode-map [s-right] 'realgud:cmd-next)
  (define-key c++-mode-map [s-down]  'realgud:cmd-step)
  (define-key c++-mode-map [s-up]    'realgud:cmd-finish)
  (define-key c-mode-map   [s-right] 'realgud:cmd-next)
  (define-key c-mode-map   [s-down]  'realgud:cmd-step)
  (define-key c-mode-map   [s-up]    'realgud:cmd-finish)
  (define-key gud-mode-map [s-right] 'realgud:cmd-next)
  (define-key gud-mode-map [s-down]  'realgud:cmd-step)
  (define-key gud-mode-map [s-up]    'realgud:cmd-finish))


(provide 'cow-keys-realgud)
