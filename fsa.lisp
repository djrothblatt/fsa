(defmacro split (list then &optional else)
  (let ((xs (gensym)))
    `(let ((,xs ,list))
       (if (consp ,xs)
           (let ((hd (car ,xs))
                 (tl (cdr ,xs)))
             ,then)
           ,else))))

(defun random-elt (list)
  (nth (random (length list)) list))

(defun state/acc (name accept transitions)
  `(,name (symbols)
          (split symbols
                 (case hd
                   ,@(append (loop for (symbol state) in transitions
                                collect `(,symbol (,state tl)))
                             '((t nil))))
                 ,accept)))

(defun state/gen (name accept transitions)
  `(,name (symbols)
          (let ((next-transition (random-elt ',(append transitions
                                                       (if accept '(accept) nil)))))
            (if (eq next-transition 'accept)
                nil
                (destructuring-bind (symbol state) next-transition
                  (case state
                    ,@(loop for (symbol state) in transitions
                         collect `(,state (cons symbol
                                                (,state symbols))))))))))


(defmacro fsa (state-fn &body states)
  `(lambda (&optional symbols)
    (labels ,(loop for state in states
                 collect (split state
                                (funcall state-fn hd
                                         (not (not (member 'accept tl)))
                                         (remove-if (lambda (x) (member x '(start accept))) tl))
                                nil))
       (,(or (car (find 'start states))
            (caar states))
         symbols))))

(defmacro define-fsa (name state-fn &body states)
  `(setf (symbol-function ',name)
         (fsa ,state-fn ,@states)))

(defmacro define-acceptor (name &body states)
  `(define-fsa ,name state/acc ,@states))

(defmacro define-generator (name &body states)
  `(define-fsa ,name state/gen ,@states))

(defmacro define-fsas/acceptor-generator (name &body states)
  (flet ((symbol-extend (symbol suffix)
           (intern (concatenate 'string
                                (symbol-name symbol)
                                (string-upcase suffix)))))
    `(progn
       (define-acceptor ,(symbol-extend name "/acceptor") ,@states)
       (define-generator ,(symbol-extend name "/generator") ,@states))))
