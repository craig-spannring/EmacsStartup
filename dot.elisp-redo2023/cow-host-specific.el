(when (string= (system-name) "nomad.local")
  (setenv "ANT_HOME" "/Users/cts/apache-ant-1.10.15")
  (setenv "JAVA_HOME"
          (string-trim-right 
           (shell-command-to-string "/usr/libexec/java_home")))

  (setenv "PATH"
          (mapconcat 'identity
                     (append
                      (split-string (getenv "PATH") path-separator)
                      (cons
                       (format "%s/bin" (getenv "ANT_HOME"))
                       '(
                         "/usr/local/bin"
                         "/Library/TeX/texbin"
                         "/Users/cts/bin"
                         "/opt/spannring/bin"
                         "/opt/gradle/gradle-8.13/bin")))
                     ":")))
  
(provide 'cow-host-specific)
