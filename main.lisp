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
