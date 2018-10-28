
(defun belgrade-weather (ARG)
  "Uses www.rainorshine.com to extract international Weather
information. If called with a prefix argument, directly prompts
your for a location.
Otherwise it uses a predefined location URL."
  (interactive "P")
  (if ARG
      (progn
        (w3-fetch "http://www.rainorshine.com/index.ssf?world_search.tp")
        (search-forward "Afghanistan" nil nil nil)
        (widget-button-press (point))
        (search-forward "[Go" nil nil nil)
        (widget-button-press (point)))
    (w3-fetch 
     "http://www.rainorshine.com/index.ssf?%24%24ZIPCITY=Belgrade%2C+MT"))
  (goto-char (point-min))
  (search-forward "BELGRADE, MT" nil nil nil)
  (forward-word -2)
  (let ((inhibit-read-only t)
        (start (point))
        (end (progn (search-forward "5-DAY" nil nil nil)
                    (end-of-line 11)
                    (point))))
    (kill-rectangle start end)
    (kill-buffer nil)
    (switch-to-buffer "*Weather*")
    (kill-all-local-variables)
    (kill-region (point-min) (point-max))
    (yank-rectangle)
    (goto-char (point-min))
    (text-mode)))



