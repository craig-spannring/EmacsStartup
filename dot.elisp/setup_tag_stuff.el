

(defun _find_next_tag ()
  "Find the next tag that matches the previous tag search"
  (interactive)
  (find-tag last-tag t))

(defun _find_prior_tag ()
  "Find the previous tag that matches the previous tag search"
  (interactive)
  (find-tag last-tag '-))

