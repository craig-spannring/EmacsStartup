

(condition-case nil
    (progn 
      (require 'setup_w3)
      (require 'w3-auto)
      (load "weather"))
  (error nil))
