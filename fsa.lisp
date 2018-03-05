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

(defmacro fsa (name state-fn &body states)
  `(defun ,name (&optional symbols)
     (labels ,(loop for state in states
                 collect (split state
                                (funcall state-fn hd
                                       (not (not (member 'accept tl)))
                                       (remove-if (lambda (x) (member x '(start accept))) tl))
                                nil))
       (,(or (car (find 'start states))
            (caar states))
         symbols))))

(defmacro fsa/acceptor (name &body states)
  `(fsa ,name state/acc ,@states))

(defmacro fsa/generator (name &body states)
  `(fsa ,name state/gen ,@states))

(defmacro fsas (name &body states)
  (flet ((symbol-extend (symbol suffix)
           (intern (concatenate 'string
                                (symbol-name symbol)
                                (string-upcase suffix)))))
    `(progn
       (fsa/acceptor ,(symbol-extend name "-acceptor") ,@states)
       (fsa/generator ,(symbol-extend name "-generator") ,@states))))

