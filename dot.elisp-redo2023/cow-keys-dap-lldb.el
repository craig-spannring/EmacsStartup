;;  -*- lexical-binding: t; -*-

;;; Setup keys to make it easier to step in gdb




;; (define-key gud-mode-map [kp-right] 'dap-next)
;; (define-key gud-mode-map [kp-down]  'dap-step)
;; (define-key gud-mode-map [kp-up]    'dap-finish)
(define-key c++-mode-map [kp-right] 'dap-next)    
(define-key c++-mode-map [kp-down]  'dap-step-in) 
(define-key c++-mode-map [kp-up]    'dap-step-out)
(define-key c-mode-map   [kp-right] 'dap-next)    
(define-key c-mode-map   [kp-down]  'dap-step-in) 
(define-key c-mode-map   [kp-up]    'dap-step-out)

(when (equal system-type 'darwin)
  ;; Special keyboard setup for a Mac.
  ;; 
  ;; (define-key gud-mode-map [s-right] 'dap-next)
  ;; (define-key gud-mode-map [s-down]  'dap-step)
  ;; (define-key gud-mode-map [s-up]    'dap-finish)  (define-key c++-mode-map [s-right] 'dap-next)    
  (define-key c++-mode-map [s-down]  'dap-step-in) 
  (define-key c++-mode-map [s-up]    'dap-step-out)
  (define-key c-mode-map   [s-right] 'dap-next)    
  (define-key c-mode-map   [s-down]  'dap-step-in) 
  (define-key c-mode-map   [s-up]    'dap-step-out))



(provide 'cow-keys-dap-lldb)
