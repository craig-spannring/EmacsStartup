;; -*- lexical-binding: t; -*-

;;; Perform any pre-initialization that is host-specific

(when (string= (system-name) "nomad.local")
  (setenv "ANT_HOME" "/Users/cts/apache-ant-1.10.15")
  (setenv "JAVA_HOME"
          (string-trim-right 
           (shell-command-to-string "/usr/libexec/java_home")))

 (setenv "PATH"
         (mapconcat 'identity
                    (append
       	      (list "/Library/Frameworks/Python.framework/Versions/Current/bin")
                     (split-string (getenv "PATH") path-separator)
                     (cons
                      (format "%s/bin" (getenv "ANT_HOME"))
                      '(
			"/usr/bin"
			"/bin"
			"/usr/sbin"
			"/usr/local/bin"
                        "/Library/TeX/texbin"
                        "/Users/cts/bin"
                        "/opt/local/bin"
                        "/opt/spannring/bin"
                        "/opt/gradle/gradle-8.13/bin")))
                    ":"))
 (setq exec-path (split-string (getenv "PATH") path-separator)))

(provide 'cow-host-specific-pre)
