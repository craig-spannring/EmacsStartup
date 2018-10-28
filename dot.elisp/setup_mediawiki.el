(cond 
 ((version< emacs-version "24.4")
  (load "teejet-mediawiki"))
 (t 
  (load "teejet-mediawiki-emacs24_4")))
