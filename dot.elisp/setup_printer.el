
;;
;;  Setup the printer for the commands print-buffer and print-region.
;;
(setq lpr-command 
      (cond 
       ((string= (system-name) "bangkok.bzn.vlt.eds.com") 
        "~/.bin/buffer_print_helper")
       ((string= (system-name) "viper.bzn.vlt.eds.com") "a2ps")
       ((string= (system-name) "olympus.bzn.vlt.eds.com") "enscript")
       (t "lpr")))

(setq lpr-switches
      (cond 
       ((string= (system-name) "bangkok.bzn.vlt.eds.com") '("-P lp1"))
       ((string= (system-name) "viper.bzn.vlt.eds.com") '("-P lp1"))
       ((string= (system-name) "olympus.bzn.vlt.eds.com") nil)
       (t nil)))

