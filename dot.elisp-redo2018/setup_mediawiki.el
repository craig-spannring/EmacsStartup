
(install-and-require-packages '(mediawiki))


(when nil
  (defcustom mi-mediawiki-url "http://bznwiki.spray.com/" "Top level URL for MI's wiki")
  (defcustom mi-mediawiki-wikiname "MI" "Name of wiki in mediawiki-site-alist")
  (defcustom mi-mediawiki-firstpage "Main Page" "Initial page to open")
  
  
  
  (defun mi-mediawiki-login (username password)
    (interactive (list (read-string (format "User name (%s): " (user-login-name))
                                    nil nil (user-login-name))
                       (read-passwd "Password: ")))
    (when (not (assoc mi-mediawiki-wikiname mediawiki-site-alist))
      (push (list mi-mediawiki-wikiname mi-mediawiki-url username password
                  mi-mediawiki-firstpage)
            mediawiki-site-alist))
    (mediawiki-site mi-mediawiki-wikiname))
  )
    
