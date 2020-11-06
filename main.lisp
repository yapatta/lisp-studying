(defun add-up (lis) 
  (if (null lis)
      0
      (+ (first lis) (add-up (rest lis)))
      ))

(defun laugh (n)
  (if (zerop n)
      nil
      (cons 'HA (laugh (- n 1)))
      )
  )

(defun count-down (n)
  (if (zerop n)
      nil
      (cons n (count-down (- n 1)))
      )
  )

(defun sq-list (lis)
  ;(if (zerop (length lis))
  (if (null lis)
      nil
      (cons (* (first lis) (first lis)) (sq-list (rest lis)))
      )
  )

(defun anyoddp (lis)
  (if (null lis)
      nil
      (or (oddp (first lis)) (anyoddp (rest lis)))
      )
  )

(defun ex-num (lis)
  (if (null lis)
      nil
      (if (numberp (first lis))
          (cons (first lis) (ex-num (rest lis)))
          (ex-num (rest lis))
          ) 
      )
  )

(defun count-odd (lis)
  (if (null lis)
      0
      (if (evenp (first lis))
          (+ 1 (count-odd (rest lis)))
          (count-odd (rest lis))
          ) 
      )
  )

(defun my-nth (n lis)
  (if (null lis)
      nil
      (if (zerop n)
          (first lis)
          (my-nth (- n 1) (rest lis))
          )
      )
  )

(defun my-append (a b)
  (if (null a)
      b
      (cons (first a) (my-append (rest a) b))
      )
  )

(defun rev (lis)
  (if (null lis) 
      nil
      (append (rev (rest lis)) (list (first lis)))
      )
  )

(defun rm (v lis)
  (if (null lis)
      nil
      (if (equal v (first lis))
          (rm v (rest lis))
          (cons (first lis) (rm v (rest lis)))
          )
      )
  )

(defun mem (v lis)
  (if (null lis)
      nil
      (if (equal v (first lis))
          lis
          (mem v (rest lis))
          ) 
      )
  )


;; Teacher Answer
(defun beforep (x y l)
  (mem y (mem x l))
  )

;; My Answer
(defun beforep (x y lis)
  (if (equal x (first lis))
      t
      (if (equal y (first lis))
          nil
          (beforep x y (rest lis))
          )
      )
  )

(defun exist (v lis)
  (if (null lis)
      nil
      (if (equal v (first lis))
          t
          (exist v (rest lis))
          ) 
      )
  )

;; Teacher Answer
(defun rmdup (x)
  (cond ((null x) nil)
        ((mem (first x) (rest x))
         (cons (first x)
               (rmdup (rm (first x) (rest x)))
               )
         (t (cons (first x) (rmdup (rest x))))
         )
        )
  )

(defun rmdup (x)
  (if x (cons (first x)
              (rmdup (rm (first x) (rest x)))
              ))
  )

;; My Answer
(defun rmdup (lis)
  (if (null lis)
      nil
      (if (exist (first lis) (rest lis))
          (rmdup (rest lis))
          (cons (first lis) (rmdup (rest lis)))
          ) 
      )
  )

(defun wa (a b)
  (rmdup (append a b))
  )

(defun seki (a b)
  (if (null a)
      nil
      (if (exist (first a) (rest a))
          (seki (rest a) b)
          (if (exist (first a) b)
              (cons (first a) (seki (rest a) b))
              (seki (rest a) b)
              )  
          )
      )
  )

(defun sa (a b)
  (if (null a)
      nil
      (if (exist (first a) b)
          (sa (rest a) b)
          (cons (first a) (sa (rest a) b))
          ) 
      )

  )

(defun lookup (key lis)
  (if (null lis)
      nil
      (if (equal key (first (first lis)))
          (first lis)
          (lookup key (rest lis))
          )
      )
  )

(defun anyoddp (x)
  (if (null x)
      nil
      (if (oddp (first x))
          T
          (anyoddp (rest x))
          )
      )
  )

(defun tr-fact (n)
  (tr-fact-body n 1)
  )

(defun tr-fact-body (n result)
  (if (= n 1)
      result
      (tr-fact-body (- n 1) (* n result))
      )
  )


(defun tr-rev (lis)
  (tr-rev-body (lis nil))
  )

(defun tr-rev-body (before next)
  (if (null before)
      next
      (tr-rev-body (rest before) (append (list (first before)) next))
      )
  )

(defun tr-count-down-body (n lis)
  (if (= n 0)
      lis
      (tr-count-down-body (- n 1) (append lis (list n)))
      )
  )

(defun tr-count-down (n)
  (tr-count-down-body n nil)  
  )

(defun sum-tree (x)
  (cond ((null x) 0)
        ((numberp x) x)
        (t (+ (sum-tree (first x))
              (sum-tree (rest x))
              ))
        )
  )

(defun flat (tree)
  (cond ((null tree) nil)
        ((symbolp tree) (list tree))
        ((listp tree) (append (flat (first tree)) (flat (rest tree))))
        (t nil)
        )
  )

(defun average (x y)
  (let ((sum (+ x y)))
    (list x y 'average 'is (/ sum 2.0))
    )
  )

(defun price-change (old new)
  (let* ((diff (- new old))
         (proportion (/ diff old))
         (percentage (* proportion 100.0))
         )
    (list 'change 'by percentage 'percent)
    )
  )

(defun countdown (n)
  (cond ((= n 0) nil)
        (t (format t "~S " n)
         (countdown (- n 1))
         )
        )
  )

(defun foo (n)
  (let nil
    (format t "~S is given~%" n)
    (countdown n)
    )
  )

(defun sum-tree (tree)
  (cond ((null tree) 0)
        ((numberp tree) tree)
        ((stringp tree) 0)
        ((symbolp tree) 0)
        (t (+ (sum-tree (first tree)) (sum-tree (rest tree))))
        )
  )

(defun sleepy (tree)
  (cond ((null tree) nil)
        ((symbolp tree) 'z)
        ((listp tree) (cons (sleepy (first tree)) (sleepy (rest tree))))
        (t nil)
        )
  )

(defun drawline (n)
  (cond ((= n 0) (format t "~%"))
        (t (format t "~S" '*)
         (drawline (- n 1))
         )
        )
  )

(defun drawbox (w h)
  (cond ((= h 0) nil)
        (t
         (drawline w)
         (drawbox w (- h 1))
         )
        )
  )

