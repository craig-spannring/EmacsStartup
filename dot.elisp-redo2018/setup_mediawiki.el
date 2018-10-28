(require 'mediawiki)

(defcustom teejet-mediawiki-url "http://bznwiki.spray.com/" "Top level URL for TeeJet's wiki")
(defcustom teejet-mediawiki-wikiname "TeeJet" "Name of wiki in mediawiki-site-alist")
(defcustom teejet-mediawiki-firstpage "Main Page" "Initial page to open")



(defun teejet-mediawiki-login (username password)
  (interactive (list (read-string (format "User name (%s): " (user-login-name))
				  nil nil (user-login-name))
		     (read-passwd "Password: ")))
  (when (not (assoc teejet-mediawiki-wikiname mediawiki-site-alist))
    (push (list teejet-mediawiki-wikiname teejet-mediawiki-url username password
				      teejet-mediawiki-firstpage)
	  mediawiki-site-alist))
  (mediawiki-site teejet-mediawiki-wikiname))

    
