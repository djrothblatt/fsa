(defmacro split (list then &optional else)
  (let ((xs (gensym)))
    `(let ((,xs ,list))
       (if (consp ,xs)
           (let ((hd (car ,xs))
                 (tl (cdr ,xs)))
             ,then)
           ,else))))

(defun state (name accept transitions)
  `(,name (symbols)
          (split symbols
                 (case hd
                   ,@(append (loop for (symbol state) in transitions
                                collect `(,symbol (,state tl)))
                             '((t nil))))
                 ,accept)))

(defmacro fsa (name &body states)
  `(defun ,name (symbols)
     (labels ,(loop for state in states
                 collect (split state
                                (state hd
                                       (not (not (member 'accept tl)))
                                       (remove-if (lambda (x) (member x '(start accept))) tl))
                                nil))
       (,(or (car (find 'start states))
            (caar states))
         symbols))))
