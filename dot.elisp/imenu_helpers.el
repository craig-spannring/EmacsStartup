;;;
;;; routines to help imenu work better with the TeeJet coding standard.
;;;



(defun _imenu_helper-backward-sexp ()
  "Try to go back one sexpr.  

  Return t on success, nil on fail"

  (let ((orig-pos (point))
        (next-pos (condition-case nil
                      (progn (backward-sexp) (point))
                    (error
                     nil))))
    (cond 
     ((null next-pos) nil)
     (t               (not (= orig-pos next-pos))))))
    

(defun _imenu_helper-forward-sexp ()
  "Try to go back one sexpr.

   Return t on success, nil on fail"

  (let ((orig-pos (point))
        (next-pos (condition-case nil
                      (progn (forward-sexp) (point))
                    (error
                     nil))))
    (cond 
     ((null next-pos) nil)
     (t               (not (= orig-pos next-pos))))))


    
(defun _imenu-helpers-block-interior ()
  "Return the block starting at the point

  Precondition-
    The point must be on the opening brace 
    of a block. 

  Return 
     Starting and ending positions of the block. 
  "
  
  (save-excursion 
    (cons (+ 1 (point)) (progn (forward-sexp) (- (point) 1)))))

(defun _imenu-helpers-get-namespace-name (pos)
  "Return the name of the namespace at location pos

   Return 
     The namespace name as a string, nil if it is 
     an anonymous namespace.  
  "

  (save-excursion
    (goto-char pos)

    ;; Make sure position is on the namespace keyword
    (if (not (string= "namespace" (thing-at-point 'word)))
        (error "Not at a namespace"))

    ;; Look for the opening brace, then go backwards 
    ;; one sexp.  That will leave the cursor on the 
    ;; name, assuming the namespace has a name. 
    (search-forward "{" nil t)
    (goto-char (- (point) 1))
    (backward-sexp)
    
    ;;
    ;; Get the word underneath the current position. 
    ;; If it is "namespace" then we are looking at an 
    ;; unnamed namespace. 
    ;; 
    (let ((raw-name (thing-at-point 'word)))
      (let ((name (if (null raw-name) 
                      nil 
                    (substring-no-properties raw-name))))
            (if (string= "namespace" name) nil name)))))



(defun imenu-helpers-locate-namespaces (start-pos end-pos)
  "Locate top level namespaces in region start-pos to end-pos

  Return 
    List of namespaces.  Each namespace item is 
      ((interior-start . interior-end) name start-pos)
    where interior-start: location right after opening brace
          interior-end;   location right before closing brace
          name:           name of the namespace, nil if anonymous
          start-pos:      Location of the namespace keyword

  Bug
     This does not recognize namespaces with a K&R brace convention.  
  "
  (let (result)
    (save-excursion

      ;;
      ;; Work our way backward through the region
      ;; 
      (goto-char end-pos)
        
      ;;
      ;; work our way to the beginning of the region.  Exit the loop
      ;; when we are before the start-pos or can not go back any more
      ;; sexpressions. 
      ;; 
      (while (and (< start-pos (point)) (_imenu_helper-backward-sexp))
        
        (let ((current-syntax (car (c-guess-basic-syntax))))
          
          (cond 
           ((> start-pos (point)) 
            ;; the backward-sexp may have moved us before the start
            ;; location.  If that's the case then don't try to look
            ;; for a namespace before start-pos. 
            nil) 
           ((eq 'namespace-open (car current-syntax))
            ;; interior-region is the first and last positions in the
            ;;                 namespace block. 
            ;; name            is the name of the namespace, nil for
            ;;                 anonymous namespaces. 
            ;; start           position of the namespace keyword. 
            (let ((interior-region (_imenu-helpers-block-interior)) 
                  (name            (_imenu-helpers-get-namespace-name
                                    (second current-syntax)))
                  (start           (second current-syntax)))
              
              (setq result (cons (list interior-region name start) result))))))))
    result
    )
  )

(defun _imenu_helper-pos-marker (pos)
  "Create a marker in the current buffer at location pos"
  (save-excursion
    (goto-char pos)
    (point-marker)))


(defun _imenu-prev-function (start-of-region)
  "Search backwards for nearest function

   Return the name and location of the function"

  ;;
  ;; Go back to previous function definition, if there is a previous
  ;; definition.  Make sure  
  ;; 
  (while (and (< start-of-region (point))
              (_imenu_helper-backward-sexp) 
              (not (eq 'defun-open (caar (c-guess-basic-syntax)))))
    nil)

  ;; 
  ;; If we found a function definition then the point is located at
  ;; the opening brace of the function. 
  ;; 
  (cond
   ((eq 'defun-open (caar (c-guess-basic-syntax)))
    ;;
    ;; start-syntax will be the position where the function 
    ;; definition starts.  (e.g. for "int foo(char ch)" start-syntax
    ;; would point to the first character in "int"). 
    ;;
    (let ((start-syntax  
           (_imenu_helper-pos-marker (cadr (first (c-guess-basic-syntax))))))
      
      (message "Moving to function name")

      ;; Move point back to the first character of the function name. 
      (goto-char start-syntax)
      (search-forward "(" nil t)
      (goto-char (- (point) 1))
      (message "about to call backward-sexp")
      (backward-sexp)
      
      (message "should be at function name")

      ;;(if (string= "const" (thing-at-point 'word)) (backward-sexp))
      ;;(backward-sexp)

      (let ((start-funname (point))
            (foo           (forward-sexp)) ; foo used for side-effect only
            (end-funname   (point)))
        ;; Result is dotted pair (name . position)
        (let ((result   (cons (buffer-substring-no-properties
                               start-funname end-funname)
                              start-syntax)))
          (goto-char start-funname)
          result
          )
        )
      )
    )
   ((eq 'namespace-open (caar (c-guess-basic-syntax)))
    ;; todo - We don't do anything here anymore. We should think about
    ;; turning this 'cond' into a simple 'if'.
    nil))
)


(defun imenu-tjcs-indexer-internal (start-pos end-pos)
  "Create an index alist of the current C++ buffer

   This function creates an index for all functions 
   defined between  start-pos and end-pos. 

   Bug
      This function doesn't recognize functions 
      with a K&R brace style. 

   Return 
      index-alist of the functions suitable for 
      use with imenu.
  "

  (save-excursion
    ;; 
    ;; We start at the end of the region and work 
    ;; backwards. 
    ;; 
    (goto-char end-pos)

    
    (let (tmp index-alist)
      ;; 
      ;; Grab the (name . position) for each top-level function from
      ;; end-pos to start-pos. 
      ;; 
      (while (and (< 1000 (length index-alist)) (setq tmp (_imenu-prev-function start-pos)))
        ;; put the (name . position) into the results
        (message "Adding %s to index " (car tmp))
        (push tmp index-alist))

      ;;
      ;; 
      (setq nested-indexes 
            (mapcar 
             (lambda (item)
               ;;
               ;; (Recursively) Run our indexer on the region in the
               ;; namespaceblock. 
               ;; 
               (imenu-tjcs-indexer-internal 
                (car (first item))
                (cdr (first item))))
             ;; 
             ;; We want to run mapcar on the list of all the
             ;; namespaces  in this block.
             ;;
             (imenu-helpers-locate-namespaces start-pos end-pos)))

      ;;
      ;; Add the index from each namespace to the results
      ;; 
      (dolist (item nested-indexes) 
        (setq index-alist (append item index-alist)))

      index-alist)))
  
(defun imenu-helpers-join-indexes (a b)
  (let ((result nil))
    ;; 
    ;; Copy list 'a' into result
    ;; 
    (dolist (item a) (setq result (cons item result)))

    ;; 
    ;; Go through each element in list 'b'.  Add any element that
    ;; isn't already in the result list to result.
    ;; 
    (dolist (item b) 
      (if (not (member item result)) 
          (setq result (cons item result))))

    result))
          

(defun imenu-helpers-index-sorter (a b)
  (< (marker-position (cdr a)) (marker-position (cdr b))))

(defun imenu-tjcs-indexer ()
  "Create an index alist of the current buffer"

  ;;
  ;; The default indexer function in imenu.el doesn't handle functions
  ;; nested in a namespace block very well.
  ;; imenu-tjcs-indexer-internal doesn't handle K&R brace placement
  ;; very well. 
  ;; 
  ;; This will run imenu-tjcs-indexer-internal to index functions
  ;; inside the namespace black (and incidently, non-K&R formatted
  ;; functions) and place that index in 'mine'.  It then runs the
  ;; index from imenu.el placing that index in 'orig'.
  ;; 
  ;; It then combines and sorts those two indexes. 
  ;; 
  (cond 
   (nil 
    (let ((mine (imenu-tjcs-indexer-internal (point-min) (point-max)))
          (orig (imenu-default-create-index-function)))
      (sort (imenu-helpers-join-indexes mine orig)
            'imenu-helpers-index-sorter)))
   (t 
    (imenu-default-create-index-function))))
   
