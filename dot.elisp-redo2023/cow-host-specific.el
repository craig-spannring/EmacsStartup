(when (string= (system-name) "nomad.local")
  (setenv "PATH"
          (mapconcat 'identity
                     (append
                      (split-string (getenv "PATH") path-separator)
                      '(
                        ;; "/Library/Frameworks/Python.framework/Versions/3.9/bin"
                        ;; "/Library/Frameworks/Python.framework/Versions/3.12/bin"
                        ;; "/Users/cts/.cargo/bin"
                        ;; "/opt/local/bin"
                        ;; "/opt/local/sbin"
                        ;; "/Library/Frameworks/Python.framework/Versions/Current/bin/"
                        ;; "/Library/PostgreSQL/16/bin"
                        ;; "/usr/bin"
                        ;; "/bin"
                        ;; "/usr/sbin"
                        ;; "/sbin"
                        "/usr/local/bin"
                        ;; "/opt/local/bin"
                        ;; "/opt/local/sbin"
                        ;; "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
                        ;; "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
                        ;; "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
                        ;; "/opt/X11/bin"
                        ;; "/Library/Apple/usr/bin"
                        ;; "/Library/TeX/texbin"
                        "/Users/cts/bin"
                        ;; "/Library/PostgreSQL/12/bin"
                        "/opt/spannring/bin"
                        ))
                     ":")))
  

(provide 'cow-host-specific)
