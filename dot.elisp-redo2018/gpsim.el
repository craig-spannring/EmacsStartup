
(require 'latlong-utm)

(defun gpsim-create-obj (file style)
  "Create a new GPSIM object and open a file in which to store 
the simulated data.

file is the nma eof the file the should be used to hold the results.
style can be either 'raw-nmea, 'utm, 'gnuplot or 'c-code
  "
  ;;  A GPSim object is a vector consisting of 5 elements-
  ;;    0)  A buffer to write the simulated data into
  ;;    1)  The current bearing of the vehicle (radians, East=0, North=pi/2, West=pi, South=3pi/2)
  ;;    2)  The Easting position in the UTM tuple
  ;;    3)  The Northing position in the UTM tuple
  ;;    4)  The UTM zone number
  ;;    5)  Nothern hemisphere.  t for North, nil for South
  ;;    6)  The current time in milliseconds since midnight
  ;;    7)  Can be either 'raw-nmea, 'utm, 'gnuplot or 'c-code
  ;;    8)  Corrected GGA data, t if corrected, nil if uncorrected.
  
  
  (let ((result [nil nil nil nil nil nil nil nil t]))
    (find-file file)
    
    ;; (aset result 0 (get-buffer-create (format "%s" buffer)))
    (aset result 0 (current-buffer))
    (aset result 5 t)
    (aset result 6 (* 3600 1000))
    (aset result 7 style)
    (switch-to-buffer-other-window (aref result 0))
    result))

(defun gpsim-msec-to-gga-time (msec)
  (let
      ((hours   (% (/ (/ msec 1000) 3600) 24))
       (minutes (% (/ (/ msec 1000) 60) 60))
       (seconds (% (/ msec 1000) 60)))
    (format "%02d%02d%02d.%02d" hours minutes seconds (/ (% msec 1000) 10))))

(defun gpsim-to-gga-degrees-minutes (decimal-degrees)
  (let
      ((degrees (truncate (abs decimal-degrees)))
       (minutes (* 60.0 (mod (abs decimal-degrees) 1.0)))
       whole-minutes fraction)
    (setq whole-minutes (truncate minutes))
    (setq fraction (mod minutes 1.0))
    (+ (* degrees 100) whole-minutes fraction)))

(defun gpsim-checksum (str)
  (let ((start (cond ((equal (aref str 0) (string-to-char "$")) 1) (t 0)))
        (end (cond ((and (> (length str) 3) (string-match "\\*[0-9a-fA-F][0-9a-fA-F]$" str))
                    (- (length str) 4))
                   ((and (> (length str) 1) (string-match "\\*$" str))
                    (- (length str) 2))
                   (t (- (length str) 1))))
        tmpstr)
    (setq tmpstr (substring str start (+ 1 end)))
    ;(princ (format "tmpstr is '%s'\n" tmpstr))
    (reduce (lambda (sum ch) 
               ;(princ (format "working on '%s'\n" (char-to-string ch)))
               (logxor sum ch))
            (cons 0 (mapcar (lambda (x) x) tmpstr)))))

(defun gpsim-checksum-str (str)
  "Checksum for the given NMEA string"
  (format "%02X" (gpsim-checksum str)))

(defun gpsim-add-checksum (str)
  (format "%s*%02X" str (gpsim-checksum (concat str "*"))))


(defun gpsim-output-c-code (obj)
   (aset obj 7 'c-code))

(defun gpsim-output-raw-nmea (obj)
   (aset obj 7 'raw-nmea))

(defun gpsim-output-utm (obj)
   (aset obj 7 'utm))


(defun gpsim-output-gnuplot (obj)
   (aset obj 7 'gnuplot))

(defun gpsim-output-raw_nmea (obj)
  (aset obj 7 nil))

(defun gpsim-set-corrected (obj value)
  (aset obj 8 value))

(defun gpsim-gga-string (obj)
  (let ((latlon (utm2ll (aref obj 3) (aref obj 2) (aref obj 4) (aref obj 5)))
        (fix-quality (cond ((aref obj 8) 2) (t 1)))
        (altitude 1400.2)
        (horizontal-dilution 1.2)
        (geod-height -17.27)
        (dgps-age 3.0)
        (dgps-station-id "0100")
        (time (aref obj 6))
        (output-style (aref obj 7)))

    (cond 
     ((equal output-style 'c-code)
      (format "   CPosition::SLatLonPos(%s, %s, %s, 1001, %s),\n"
              (car latlon) (cadr latlon) altitude (gpsim-msec-to-gga-time time)))
     ((equal output-style 'raw-nmea)
      (concat
       (gpsim-add-checksum
        (concat
         (format "$GPGGA,")                                                ; NMEA prefix
         (format "%s," (gpsim-msec-to-gga-time time))                      ; Time
         (format "%s," (abs (gpsim-to-gga-degrees-minutes (car latlon))))  ; Lat
         (format "%s," (if (>= (car latlon) 0.0) "N" "S"))                 ; N or S
         (format "%s," (abs (gpsim-to-gga-degrees-minutes (cadr latlon)))) ; Lon
         (format "%s," (if (>= (cadr latlon) 0.0) "E" "W"))                ; E or W
         (format "%s," fix-quality)                                        ; Fix Quality
         (format "07,")                                                     ; Satellites in view
         (format "%s,"  horizontal-dilution)                               ; Horizontal dilution
         (format "%s,M," altitude)                                         ; Altitude
         (format "%s,M," geod-height)                                      ; Height of geoid of WGS84 ellipsoid
         (format "%s," dgps-age)                                           ; Age of DGPS update
         (format "%s" dgps-station-id)))                                   ; DGPS station ID #
       "\n"))
     ((equal output-style 'utm)
      (format "%s %s %s %s\n" (aref obj 2) (aref obj 3) (aref obj 4) (cond ((aref obj 5) 'N) (t 'S))))
     ((equal output-style 'gnuplot)
      (format "%s %s\n" (aref obj 2) (aref obj 3) (aref obj 4)))
)))
    

(defun gpsim-output-marker (obj str)
    (set-buffer (aref obj 0))
    (goto-char (point-max))
    (insert (format ";// %s \n" str)))

(defun gpsim-output-gga (obj)
  (set-buffer (aref obj 0))
  (goto-char (point-max))
  (insert (gpsim-gga-string obj))
)


(defun gpsim-set-origin (obj lat lon bearing)
  (aset obj 1 (mod (* (/ pi 180.0) (+ (- 360 bearing) 90)) (* 2 pi)))
  (let ((utm (ll2utm lat lon)))
    (aset obj 2 (cadr utm))
    (aset obj 3 (car utm))
    (aset obj 4 (caddr utm)))
  obj)


(defun gpsim-obj-time (obj)
  "Return the current simulation time of the object in milliseconds."
  (aref obj 6))

(defun gpsim-stop (obj seconds)
  (while (> seconds 0.0)
    ;; Print the new GGA string
    (gpsim-output-gga obj)
    
    ;; update the elapsed time and current object time 
    (aset obj 6 (+ (aref obj 6) 200))
    (setq seconds (- seconds 0.2))))

(defun gpsim-feet-to-meters (feet)
  (let ((f2m 0.3048006096))
    (* feet f2m)))

(defun gpsim-meters-to-feet (m)
  (let ((m2f (/ 1.0 0.3048006096)))
    (* m2f m)))

(defun gpsim-miles-to-meters (miles)
  (gpsim-feet-to-meters (* 5280.0 miles)))

(defun gpsim-meters-to-miles (meters)
  (/ (gpsim-meters-to-feet meters) 5280.0))
  

(defun gpsim-miles/hour-to-meters/second (miles/hour)
  (let ((miph-2-meph (/ 1 2.2369362912)))
    (* miles/hour miph-2-meph)))

(defun gpsim-meters/second-to-miles/hour (meters/second)
  (let ((factor 2.2369362912))
    (* meters/second factor)))

(defun gpsim-turn (obj direction new-bearing radius speed)
  "Simulate turning.
DIRECTION is either 'left or 'right
NEW-BREARING is the final bearing (in compass degrees) that you want to turn to
RADIUS is the radius of the turn. 
SPEED is the speed  in meters/second"

  (setq new-bearing (mod new-bearing 360.0))

  ;; Convert the bearing from compass degrees to radians.
  (setq new-bearing (mod (* (/ pi 180.0) (+ (- 360 new-bearing) 90)) (* 2 pi)))

  (let ((old-bearing   (aref obj 1)))
    (let (center-x 
          center-y theta dx dy
          (distance      0.0)
          (origin-x      (aref obj 2))
          (origin-y      (aref obj 3))
          new-x new-y
          (elapsed-time  0.0)
          distance-required
          delta-bearing
          (direction-to-center (if (equal direction 'right) (- (/ pi 2))
                                 (/ pi 2))))

      (setq delta-bearing 
            (cond ((>= new-bearing old-bearing)
                   (- new-bearing old-bearing))
                  (t
                   (- (+ (* 2 pi) new-bearing) old-bearing))))
      (setq  distance-required (* radius delta-bearing))
      ;;(print (format "Distance required to turn is %s" distance-required))
      (setq center-x (+ (aref obj 2) 
                        (* radius (cos (+ old-bearing direction-to-center)))))
      (setq center-y (+ (aref obj 3) 
                        (* radius (sin (+ old-bearing direction-to-center)))))
      ;(print (format "distance required is %s" distance-required))
      (while (< distance distance-required)
        (setq distance (* speed elapsed-time))

        ;; theta will represent angle of the angle on the unit circle
        ;; where the vehicle is heading
        (setq theta (mod (+ old-bearing 
                            (cond
                             ((equal direction 'left)
                              (- (/ distance radius) (/ pi 2)))
                             (t 
                              (- (/ pi 2) (/ distance radius)))))
                            (* 2 pi)))

        (setq dx (* (cos theta) radius))
        (setq dy (* (sin theta) radius))
        (setq new-x (+ center-x  dx))
        (setq new-y (+ center-y  dy))
        (aset obj 2 new-x)
        (aset obj 3 new-y)
      
        ;; Print the new GGA string
        (gpsim-output-gga obj)
      
        (setq elapsed-time (+ elapsed-time 0.2))
        (aset obj 6 (+ (aref obj 6) 200))
        )
      ))
  (aset obj 1 new-bearing)
  obj
  )

    
       
(defun gpsim-straight (obj distance speed)
  "Simulated driving in a straight line.
DISTANCE is the distance to drive in meters
SPEED is the speed in which to drive given in meters per second
"
  (let ((origin-x      (aref obj 2))
        (origin-y      (aref obj 3))
        (time-required (/ distance speed))
        (elapsed-time  0.0)
        (theta         (aref obj 1)))
    
    (while (< elapsed-time time-required)
      ;; determine the new position
      (let ((distance-driven  (* elapsed-time speed))
            dx dy)
        (setq dy (* distance-driven (sin theta)))
        (setq dx (* distance-driven (cos theta)))
        (aset obj 2 (+ origin-x dx))
        (aset obj 3 (+ origin-y dy)))

      ;(message "utm=%12f,%12f\n" (aref obj 3) (aref obj 2))
      ;(gpsim-output-marker obj (format "utm=%12f,%12f\n" (aref obj 3) (aref obj 2)))
      ;; Print the new GGA string
      (gpsim-output-gga obj)

      ;; update the elapsed time and current object time 
      (setq elapsed-time (+ elapsed-time 0.2))
      (aset obj 6 (+ (aref obj 6) 200)))
    obj))
  

(defun gpsim-distance (a b)
  "Return the distance between two points
A First point. 
B Second point. 

Each point is given by a list of numbers.

Example:
   (gpsim-distance '(0 0) '(3 4))
   25
"

  ;; Check for a couple of error conditions
  (when (or (not (listp a))
            (not (listp b))
            (not (= (length a) (length b))))
    (error "A and B must be points.  A point is a list made up of the X,Y,... coordinates"))

  (sqrt (reduce '+ (map 'list 
                        (lambda (c1 c2) (* (- c1 c2) (- c1 c2))) 
                        a b)))
)


(provide 'gpsim)
