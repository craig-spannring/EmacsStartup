;; -*- lexical-binding: t; -*-

;; (defcustom plantuml-jar-path nil
;;   "Path to the PlantUML jar file."
;;   :type 'file
;;   :group 'cow-uml)


;; (if (not (boundp 'plantuml-jar-path))
;;     (message "Warning: plantuml-jar-path is not set. Please customize to path of the PlantUML jar file.")
  
;;   (install-and-require-packages '(plantuml-mode))
  
;;   ;; Enable plantuml-mode for PlantUML files
;;   (add-to-list 'auto-mode-alist '("\\.puml\\'"     . plantuml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(provide 'cow-uml)
