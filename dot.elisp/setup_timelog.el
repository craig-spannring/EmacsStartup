;;; $Header: /home/cts/PersonalRepository.tmp/.elisp/setup_timelog.el,v 1.3 2006/06/21 13:57:09 cts Exp $




(require 'timelog)
(when (string-match "XEmacs" emacs-version) (require 'time-date))

(when (not (string-match "XEmacs" emacs-version))
  (load "~/.elisp/xemacs-compat/time-date"))

(define-key ctl-x-map "ti" 'timelog-in)
(define-key ctl-x-map "to" 'timelog-out)
(define-key ctl-x-map "tc" 'timelog-change)
(define-key ctl-x-map "ts" 'timelog-make-summary)
(define-key ctl-x-map "tr" 'timelog-reread-logfile)

(setq timelog-hourly-rate 50
      timelog-currency-string "DM"
      timelog-currency-separator-string ","
      timelog-summary-function 'timelog-my-summarize-project)


;(defconst midtech-payperiod_epoch '(15175 63260 0)
;  "Start of the pay period beginning on July 8, 2001")

(defconst midtech-payperiod_epoch (date-to-time "Sun Dec 18 13:00:00 2005 -0700")
  "Start of a pay period in the not too distant past.")



(defun midtech-current-pay-period ()
  (let ((diff (time-since midtech-payperiod_epoch))
        periods
        lo
        hi
        carry)

    (setq periods (/ (+ (* (car diff) 65536) (cadr diff))
                     (* 86400 14)))
    (setq lo (+ (* 86400 14 periods) (cadr midtech-payperiod_epoch)))
    (setq carry (/ lo 65536))
    (setq lo (- lo (* carry 65536)))
    (setq hi (+ (car midtech-payperiod_epoch) carry))
    (list hi lo 0)))


(defun mt-fill-out-timesheet-old (pay-period)
  "Need documentation"
  (interactive  (list (read-string
                       (format "Start of pay period: (default '%s') "
                               (format-time-string "%Y/%m/%d"
                                                   (midtech-current-pay-period))))))
  
  (save-some-buffers)
  (when (string= "" pay-period) 
    (setq pay-period (format-time-string "%Y/%m/%d" (midtech-current-pay-period))))
  
  (let ((template "z:\\TimeSheets\\Time_Sheet_template.doc z:\\bar.doc" ))
    
    (message "pay period is %s" pay-period) 
    (call-process "summ_log_old.cmd" "~/.timelog" "*bar*" nil 
                  "-r" 
                  template
                  "-s" 
                  pay-period)
    )
)


(defun mt-fill-out-timesheet (pay-period)
  "Need documentation"
  (interactive  (list (read-string
                       (format "Start of pay period: (default '%s') "
                               (format-time-string "%Y/%m/%d"
                                                   (midtech-current-pay-period))))))
  
  (save-some-buffers)
  (when (string= "" pay-period) 
    (setq pay-period (format-time-string "%Y/%m/%d" (midtech-current-pay-period))))
  
  (let ((template "z:\\TimeSheets\\Time_Sheet_template.xls z:\\bar.doc" ))
    (message "pay period is %s" pay-period) 
    (call-process "summ_log.cmd" "~/.timelog" "*bar*" nil 
                  "-r" 
                  template
                  "-s" 
                  pay-period)
    )
)