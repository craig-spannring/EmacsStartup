;;
;; Handle setting location for the moon phases, sunrise, and sunset.
;;




(defcustom cow-calendar-location 'Bozeman
  "Select geographical location for calendar functions."
  :type '(choice
	  (const Ålborg)    ;; Denmark
          (const Bozeman)   ;; Montana USA
	  (const Medford)   ;; Oregon USA
          (const Portland)) ;; Oregon USA
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (message "Setting calendar location to %s" cow-calendar-location)
	   (cond
	    ((equal value 'Ålborg)
	     (setq calendar-location-name "Ålborg")
	     (setq calendar-latitude 57.099998)
	     (setq calendar-longitude 9.85))
	    ((equal value 'Bozeman)
	     (setq calendar-location-name "Bozeman")
	     (setq calendar-latitude 45.679)
	     (setq calendar-longitude -111.037))
	    ((equal value 'Medford) 
	     (setq calendar-location-name "Medford")
	     (setq calendar-latitude 42.36)
	     (setq calendar-longitude -122.86))
	    ((equal value 'Portland) 
	     (setq calendar-location-name "Portland")
	     (setq calendar-latitude 45.5887222)
	     (setq calendar-longitude -122.5975000))))
  :group 'cow-emacs-conf)


(require 'calendar)

(provide 'cow-moon-configuration)
