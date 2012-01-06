(defun foo (x y)
  (+ x y))

(foo 5 6)

(defun cursor-fun (x)
  (cond 
   ((= x 5) (set-cursor-color "red"))
   ((= x 6) (set-cursor-color "blue"))))

(defun cursor-fun-factory (init)
  (let ((sym (gensym)))
    (set sym init)
    `(lambda nil
       (cond
        ((= ,sym 1) (progn (set-cursor-color "red")
                        (setq ,sym 2)))
        ((= ,sym 2) (progn (set-cursor-color "blue")
                        (setq ,sym 3)))
        ((= ,sym 3) (progn (set-cursor-color "green")
                        (setq ,sym 1))))))))

(setf j (cursor-fun-factory))

(j 6)




(defun make-adder (init)
  (let ((sym (gensym)))
    (set sym init)
    `(lambda (&optional val)
       (if val
           (setq ,sym (+ ,sym val))
         ,sym))))



(setq y (make-adder 1))


(funcall y 5)

 (funcall x)
 (funcall x 2)
 (funcall x) => 3





