;;
;; Set up stuff for latex
;;
(setq tex-dvi-view-command
          (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))
(setq auto-mode-alist 
      (append 
       (list '("\\.latex\\'" . latex-mode)) auto-mode-alist))

