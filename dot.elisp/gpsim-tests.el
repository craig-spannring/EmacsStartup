
(require 'cl)
(load "gpsim")


(defun gpsim-test1 ()
  (gpsim-test "bigfield.raw" 103 (gpsim-feet-to-meters 30.0)
              (gpsim-miles-to-meters 0.07)
              (gpsim-miles-to-meters 0.05)
              (gpsim-miles/hour-to-meters/second 10)))

(defun gpsim-test2 ()
  (gpsim-test "bigfield.raw" 103 (gpsim-feet-to-meters 30.0)
              (gpsim-miles-to-meters 0.2)
              (gpsim-miles-to-meters 0.1)
              (gpsim-miles/hour-to-meters/second 10)))

(defun gpsim-test-spiral (file-name bearing boom-width length width speed)
  (let ((turn-radius   (/ boom-width 2))
        (turn-diameter boom-width)
        obj count)
    
    (setq obj (gpsim-create-obj file-name 'gnuplot))

    (gpsim-set-origin obj 45.83 -111.05 bearing)

    (gpsim-stop obj 14.0)

    ;; Drive a short straight section that will allow the boundary to close
    (gpsim-straight obj turn-diameter speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    (while (and (> width 0) (> length 0))
      ;; side 1
      (message "Doing side 1, length %d, width %d" length width)
      (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
      (setq bearing (mod (- bearing 90) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)
      
      ;; side 2
      (message "Doing side 2, length %d, width %d" length width)
      (gpsim-straight obj (- width (* 2 turn-diameter)) speed)
      (setq bearing (mod (- bearing 90) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)
      
      ;; Side three 
      (message "Doing side 3, length %d, width %d" length width)
      (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
      (setq bearing (mod (- bearing 90) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)
      
      ;; Side four 
      (message "Doing side 4, length %d, width %d" length width)
      (gpsim-straight obj (- width (* 3 turn-diameter)) speed)
      (setq bearing (mod (- bearing 90) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)
      
      ;; Do just the start of side 1 in the next innermost spiral
      (gpsim-straight obj turn-diameter speed)
      
      ;; Adjust the length and width for the next pass
      (setq length (- length (* 2 turn-diameter)))
      (setq width (- width (* 2 turn-diameter)))
      )
    )
  )

(defun gpsim-test-expanding-box (file-name bearing boom-width passes speed)
  (let ((turn-radius   (/ boom-width 2))
        (turn-diameter boom-width)
        obj count)
    
    (setq obj (gpsim-create-obj file-name))

    (gpsim-set-origin obj 45.83 -111.05 bearing)

    (gpsim-stop obj 14.0)

    ;; Start by marking 2 turns
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)
    
    (let ((dist 0.0))
      (while (> 0 passes)
        (setq dist (+ dist turn-radius))
        (setq passes (1- passes))

        (setq bearing (mod (- bearing 90) 360.0))
        (gpsim-turn obj 'left bearing turn-radius speed)
        
        (gpsim-straight obj dist speed)

        (setq bearing (mod (- bearing 90) 360.0))
        (gpsim-turn obj 'left bearing turn-radius speed)
        
        (gpsim-straight obj dist speed)
        ))))
    
(defun gpsim-test (file-name bearing boom-width length width speed 
                             &optional lat lon)
  (let ((turn-radius   (/ boom-width 2))
        (turn-diameter boom-width)
        obj count)
    
    (setq obj (gpsim-create-obj file-name))

    (setq lat (cond (lat lat) (t 45.83)))
    (setq lon (cond (lon lon) (t -111.05)))

    (gpsim-set-origin obj lat lon bearing)

    (gpsim-output-marker obj 
                         (format "bearing=%s, boom-width=%sm  field=%sm by %sm speed=%sm/s lat/lon=%s/%s\n" 
                                 bearing boom-width length width speed lat lon))
    (gpsim-stop obj 14.0)

    ;; Drive a short straight section that will allow the boundary to close
    (gpsim-output-marker obj "Drive a short straight section that will allow the boundary to close")
    (gpsim-straight obj turn-diameter speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; first side of outer pass
    (gpsim-output-marker obj "first side of outer pass")
    (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)
  
    ;; Second side of outer pass
    (gpsim-output-marker obj "second side of outer pass")
    (gpsim-straight obj (- width (* 2 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Side three of outer pass
    (gpsim-output-marker obj "side three of outer pass")
    (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Side four of outer pass
    (gpsim-output-marker obj "side four of outer pass")
    (gpsim-straight obj (- width (* 3 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Finished with the outer most swath, now do one more pass around
    ;; First side of inner pass
    (gpsim-output-marker obj " Finished with the outer most swath, now do one more pass around")
    (gpsim-output-marker obj " First side of inner pass")
    (gpsim-straight obj (- length (* 3 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Second side of inner pass
    (gpsim-output-marker obj " Second side of inner pass")
    (gpsim-straight obj (- width (* 4 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Third side of inner pass
    (gpsim-output-marker obj " thirdside of inner pass")
    (gpsim-straight obj (- length (* 4 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Fourth side of inner pass
    (gpsim-output-marker obj " fourth side of inner pass")
    (gpsim-straight obj (- width (* 5 turn-diameter)) speed)
    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Finished with all of the outer passes.  Do first parallel pass
    (gpsim-output-marker obj " Finished with all of the outer passes.  Do first parallel pass")
    (gpsim-straight obj (- length (* 3 turn-diameter)) speed)
    (setq bearing (mod (- bearing 180) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    ;; Do second parallel pass
    (gpsim-output-marker obj " Do second parallel pass")
    (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
    (setq bearing (mod (+ bearing 180) 360.0))
    (gpsim-turn obj 'right bearing turn-radius speed)


    ;; Fill in the rest of the area
    (gpsim-output-marker obj " Fill in the rest of the area")
    (setq count (/ (/ (- width (* 6 turn-diameter)) turn-diameter) 2))
    ;; Stop when the area is filled or 35 hours have elapsed
    (gpsim-output-marker obj " Stop when the area is filled or 35 hours have elapsed")
    (while (and (> count 0.9) (< (gpsim-obj-time obj) (* 1000 60 60 36)))
      (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
      (setq bearing (mod (- bearing 180) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)

      (gpsim-straight obj (- length (* 2 turn-diameter)) speed)
      (setq bearing (mod (+ bearing 180) 360.0))
      (gpsim-turn obj 'right bearing turn-radius speed)

      (setq count (1- count)))


    obj))



(defun gpsim-test3 ()
  (let ((bearing 20.0)
        (turn-radius (/ 27.4 2))
        (speed (gpsim-miles/hour-to-meters/second 10))
        obj
        count)

    (setq obj (gpsim-create-obj "bar.raw"))

    (gpsim-set-origin obj 46.0 -110.0 bearing)

    ;;(gpsim-stop obj 14.0)

    (gpsim-straight obj 100 speed)
    (setq bearing (mod (- bearing 180) 360.0))
    (gpsim-turn obj 'right bearing turn-radius speed)
    
    (gpsim-straight obj 100 speed)
    (setq bearing (mod (+ bearing 180) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)
    
    (gpsim-straight obj 100 speed)
    
    ;;(gpsim-stop obj 14.0)

    obj))


(defun gpsim-test4 ()
  (let ((bearing 20.0)
        (turn-radius (/ 27.4 2))
        (speed (gpsim-miles/hour-to-meters/second 10))
        obj
        count)

    (setq obj (gpsim-create-obj "bar.raw"))

    (gpsim-set-origin obj 46.0 -110.0 bearing)

    (gpsim-stop obj 14.0)

    (gpsim-straight obj 100 speed)
    (setq bearing (mod (- bearing 180) 360.0))
    (gpsim-turn obj 'right bearing turn-radius speed)

    (gpsim-straight obj 100 speed)
    (setq bearing (mod (+ bearing 180) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)

    (gpsim-stop obj 14.0)

    obj))


(defun gpsim-test5 ()
  (let ((bearing 0.0)
        (turn-radius (/ 30.0 2))
        (speed 10) 
        obj
        count)

    (setq obj (gpsim-create-obj "bar.raw"))
    (gpsim-set-corrected obj nil)

    (gpsim-set-origin obj 46.0 -110.0 bearing)
    
    (gpsim-stop obj 1.0)

    (dotimes (i 7)
      (gpsim-straight obj 300 speed)
      (setq bearing (mod (- bearing 180) 360.0))
      (gpsim-turn obj 'right bearing turn-radius speed)
      
      (gpsim-straight obj 300 speed)
      (setq bearing (mod (- bearing 180) 360.0))
      (gpsim-turn obj 'left bearing turn-radius speed)
    )))

(defun gpsim-test08 ()
  (let ((obj (setq obj (gpsim-create-obj "bar.raw"))))
    (gpsim-output-c-code obj)
    (gpsim-test08-internal obj)
))

(defun gpsim-test08-internal (obj)
  (let ((bearing 87.0)
        (turn-radius (/ 30.0 2))
        (speed (gpsim-miles/hour-to-meters/second 10))
       
        count)



    (gpsim-set-origin obj 46.0 -108.001 bearing)
    (gpsim-stop obj 20.0)

    (gpsim-straight obj 200 speed)

    (setq bearing (mod (- bearing 90) 360.0))
    (gpsim-turn obj 'left bearing turn-radius speed)
    (gpsim-straight obj 100 speed)


))

    
    

(defun gpsim-test07-c-code ()
  (let ((bearing 0.0)
        (turn-radius (/ 30.0 2))
        (speed 10) 
        obj
        count)

    (setq obj (gpsim-create-obj "bar.raw"))
    (gpsim-output-c-code obj)

    (gpsim-set-origin obj 46.0 -110.0 bearing)
    
    (dotimes (count 2)
      (gpsim-set-origin obj 46.0 -110.0 bearing)
      (gpsim-output-marker obj (format "starting pass %s" count))
      (dotimes (i 1)
        (gpsim-output-marker obj (format "starting straight line"))
        (gpsim-straight obj 300 speed)
        (gpsim-output-marker obj (format "starting the turn"))
        (setq bearing (mod (- bearing 180) 360.0))
        (gpsim-turn obj 'right bearing turn-radius speed)
        (gpsim-output-marker obj (format "finished the turn"))
        
        (gpsim-straight obj 300 speed)
        (gpsim-output-marker obj (format "starting the 2nd turn"))
        (setq bearing (mod (- bearing 180) 360.0))
        (gpsim-turn obj 'left bearing turn-radius speed)
        (gpsim-output-marker obj (format "finished the 2nd turn"))
    ))))
    


(defun gpsim-test6 ()
  (let ((bearing 0.0)
        (turn-radius (/ 30.0 2))
        (speed 10) 
        obj
        count)

    (setq obj (gpsim-create-obj "bar.raw" 'raw-nmea))

    (gpsim-set-origin obj 46.0 -110.0 bearing)
    
    (gpsim-stop obj 20)

;    (dotimes (i 7)
      (gpsim-straight obj 150 speed)
      (setq bearing (mod (- bearing 180) 360.0))
      (gpsim-turn obj 'right bearing turn-radius speed)
      
      (gpsim-straight obj 300 speed)
      (setq bearing (mod (- bearing 180) 360.0))
      (gpsim-turn obj 'right bearing turn-radius speed)
 
      (gpsim-straight obj 160 speed)

;   )
))
    





;   (let (obj
;         tmp
;         (passes-remaining 1)
;         (bearing 20.0)
;         (turn-radius 100.0)
;         (speed (gpsim-miles/hour-to-meters/second 15)))
    
;     (setq obj (gpsim-create-obj "singlecircle.raw" 'gnuplot))

;     (gpsim-set-origin obj 46.0 -110.0 bearing)
;     (gpsim-stop obj 20.0)

;     (while (> passes-remaining 0)
;       (setq passes-remaining (- passes-remaining 1))
;       (setq tmp bearing)
;       (setq bearing (mod (+ bearing 10.0) 360.0))
;       (message "Turning from %s to %s" tmp bearing)
;       (gpsim-turn obj 'right 
;                   bearing 
;                   turn-radius speed)
;       )))

