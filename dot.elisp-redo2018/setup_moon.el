;;
;;  Enter our location for the moon phases, sunrise, and sunset.
;;
(cond
 (nil
  (setq calendar-location-name "Portland")
  (setq calendar-latitude 45.5887222)
  (setq calendar-longitude -122.5975000))
 (nil
  (setq calendar-location-name "Ålborg")
  (setq calendar-latitude 57.099998)
  (setq calendar-longitude 9.85))
 (t
  (setq calendar-location-name "Bozeman")
  (setq calendar-latitude 45.679)
  (setq calendar-longitude -111.037)))

;;; medford is 42-22-20.055N  122-52-21.296W
;(setq calendar-location-name "Medford")
;(setq calendar-latitude 42.36)
;(setq calendar-longitude -122.86)

(require 'calendar)

