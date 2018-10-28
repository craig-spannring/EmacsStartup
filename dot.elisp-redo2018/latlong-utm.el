(provide 'latlong-utm)

(defun nmea_latlon_to_decimal_latlon (value)
  "Convert a NMEA style latitude or longitude value (i.e. dddmm.frac_minutes) 
into a plain decimal number represting the degrees."
  (let ((sign (cond ((< value 0.0) -1.0) (t 1.0))))
    (setq value (abs value))
    (* sign (+ (truncate (/ (abs value) 100)) (/ (mod (abs value) 100.0) 60)))))

(defun ll2utm (Lat Long &optional use_nmea_drainbramage)
  "Convert a latitude/longitude into a UTM value return the list (northing easting zone)"
  
  (when use_nmea_drainbramage
    (setq Lat (nmea_latlon_to_decimal_latlon Lat))
    (setq Long (nmea_latlon_to_decimal_latlon Long)))

  (let
      ((LongTemp (- (+ Long 180.0)
                    (* (ftruncate (/ (+ Long 180.0) 360.0)) 360.0) 180.0))
       (deg2rad (/ pi 180.0)))
    (let (
          (a            6378137.0)      ; Equatorial radius
          (eccSquared   0.00669438)     ; eccentricity squared
          (k0           0.9996)         ;
          LongOrigin
          eccPrimeSquared 
          N T C A M
          (LatRad  (* Lat deg2rad))
          (LongRad  (* Long deg2rad))
          LongOriginRad
          (ZoneNumber (+ (truncate (/ (+ LongTemp 180) 6)) 1)))

      (when (and (>= Lat 56.0) (< Lat 64.0) (>= LongTemp 3.0) (< LongTemp 12))
        (setq ZoneNumber 32))

      ;; TODO handle zone numbers for Svalbard
      ;; if( Lat >= 72.0 && Lat < 84.0 ) 
      ;; {
      ;;    if(      LongTemp >= 0.0  && LongTemp <  9.0 ) ZoneNumber = 31;
      ;;    else if( LongTemp >= 9.0  && LongTemp < 21.0 ) ZoneNumber = 33;
      ;;    else if( LongTemp >= 21.0 && LongTemp < 33.0 ) ZoneNumber = 35;
      ;;    else if( LongTemp >= 33.0 && LongTemp < 42.0 ) ZoneNumber = 37;
      ;; }

      (setq LongOrigin (+ (- (* (- ZoneNumber 1) 6) 180) 3)) ;; +3 puts origin in middle of zone
      (setq LongOriginRad (* LongOrigin deg2rad))

      (setq eccPrimeSquared (/ eccSquared (- 1 eccSquared)))

      (setq N (/ a (sqrt (- 1 (* eccSquared (sin LatRad) (sin LatRad))))))
      (setq T (* (tan LatRad) (tan LatRad)))
      (setq C (* eccPrimeSquared (cos LatRad) (cos LatRad)))
      (setq A (* (cos LatRad) (- LongRad LongOriginRad)))
    
      (let
          ((term1 
            (+ 0.0 (* (- (- (- 1 (/ eccSquared 4)) 
                            (/ (* 3 eccSquared eccSquared) 64)) 
                         (/ (* 5 (expt eccSquared 3)) 256)) LatRad))
            )
           (term2 
            (- (* (+
                   (+ (/ (* 3 eccSquared) 8)  
                      (/ (* 3 eccSquared eccSquared) 32))
                   (/ (* 45 (expt eccSquared 3)) 1024))
                  (sin (* 2 LatRad))))
            )       
           (term3 
            (* (+ (/ (* 15 eccSquared eccSquared) 256)
                  (/ (* 45 (expt eccSquared 3.0)) 1024))
               (sin (* 4 LatRad)))
            )
           (term4
            (- 0.0 (* (/ (* 35 (expt eccSquared 3)) 3072) (sin (* 6 LatRad))))
            ))
        (setq M (* a (+ term1 term2 term3 term4)))
        )

      (list
       ;; Northing position
       (+ (* k0
             (+ M
                (* N
                   (tan LatRad)
                   (+ (/ (* A A) 2)
                      (/ (* (+ (+ (- 5 T) (* 9 C)) (* 4 C C)) (expt A 4)) 24)
                      (/ (* (- (+ (+ (- 61
                                        (* 58 T))
                                     (* T T))
                                  (* 600 C))
                               (* 330 eccPrimeSquared))
                            (expt A 6))
                         720)))))
          (cond ((< Lat 0.0) 10000000.0)
                (t 0.0)))       
       ;; Now the Easting value
       (+ (* k0 N (+ A
                                      (/ (* (+ (- 1 T) C) A A A) 6)
                                      (/ (* 
                                          (- (+ (- 5 (* 18 T))
                                                (* T T)
                                                (* 72 C))
                                             (* 58 eccPrimeSquared))
                                          A A A A A)
                                         120)))
                           500000.0)

       ;; Last,  the zone number 
       ZoneNumber
       
       )
      )
    )
  )


(defun utm2ll (UTMNorthing UTMEasting ZoneNumber NorthernHemisphere)
  "Compute the the latitude and longitude given a UTM coordinate.
UTMNorthing and UTMEasting are, as you should expect, meters.  A
non-nil NorthernHemisphere indicates that the position is north
of the equator, nil indicates location South of the equator.

Returns (Latitude Longitude)

  
  "

  (let ((rad2deg      (/ 180.0 pi))
        (k0           0.9996)         ;
        (a            6378137.0)      ; Equatorial radius
        (eccSquared   0.00669438)     ; eccentricity squared
        eccPrimeSquared
        e1
        N1  T1  C1  R1  D  M
        LongOrigin
        mu  phi1  phi1Rad
        x y
        Lat Long)
    (setq e1 (/ (- 1 (sqrt (- 1 eccSquared))) 
                (+ 1 (sqrt (- 1 eccSquared)))))
    (setq x (- UTMEasting 500000.0)) ; remove 500,000 meter offset for longitude
    (setq y (- UTMNorthing (cond (NorthernHemisphere 0.0) (t 10000000.0))))

    (setq LongOrigin (+ (* (- ZoneNumber 1) 6) (- 180.0) 3))

    (setq eccPrimeSquared (/ eccSquared (- 1 eccSquared)))

    (setq M (/ y k0))
    (setq mu (/ M (* a
                     (+ 1
                        (- (/ eccSquared 4))
                        (- (/ (* 3 eccSquared eccSquared) 64))
                        (- (/ (* 5 (expt eccSquared 3.0)) 256.0))))))
    (setq phi1Rad (+ mu
                     (* (- (* 3 (/ e1 2)) (/ (* 27 e1 e1 e1) 32))
                        (sin (* 2 mu)))
                     (* (- (/ (* 21 e1 e1) 16)
                           (/ (* 55 e1 e1 e1 e1) 32))
                        (sin (* 4 mu)))
                     (* (/ (* 151 e1 e1 e1) 96) (sin (* 6 mu)))))
    (setq phi1 (* phi1Rad rad2deg))

    (setq N1 (/ a (sqrt (- 1 (* eccSquared (sin phi1Rad) (sin phi1Rad))))))
    (setq T1 (* (tan phi1Rad) (tan phi1Rad)))
    (setq C1 (* eccPrimeSquared (cos phi1Rad) (cos phi1Rad)))
    (setq R1 (/ (* a (- 1 eccSquared)) 
                (expt (- 1 (* eccSquared (sin phi1Rad) (sin phi1Rad))) 1.5)))
    (setq D (/ x (* N1 k0)))

    (setq Lat (* rad2deg 
                (- phi1Rad
                   (* (/ (* N1 (tan phi1Rad)) R1)
                      (+ (+ (/ (* D D) 2.0))
                         (- (/ (* (+ 5 (* 3 T1) (* 10 C1) (- (* 4 C1 C1)) (- (* 9 eccPrimeSquared)))
                                  D D D D)
                               24.0))
                         (+ (/ (* (+ 61 
                                     (* 90 T1)
                                     (* 298 C1)
                                     (* 45 T1 T1)
                                     (- (* 252 eccPrimeSquared))
                                     (- (* 3 C1 C1)))
                                  D D D D D D)
                               720)))))))
    (let
        ((term1 D)
         (term2 (/ (* (+ 1 (* 2 T1) C1) D D D) 6))
         (term3 (/ (* (+ 5 (- (* 2 C1)) (* 28 T1) (- (* 3 C1 C1)) (* 8 eccPrimeSquared) (* 24 T1 T1))
                      D D D D D)
                   120)))
      (setq Long (+ LongOrigin (* rad2deg 
                                  (/ (+ term1 (- term2) term3)
                                     (cos phi1Rad))))))
          
    (list Lat Long)
    )
  )