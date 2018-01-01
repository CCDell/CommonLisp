;;;;Noah Sanci
;;;;Simulates rolling die with the rules of the board game "Risk" deciding the victor when the maximum
;;;;number of die are used

(defparameter wins (vector 0 0 0)); 0: defender wins both, 1: attacker and defender both win once,
                                  ; 2: attacker wins both.

(defun roll (&key (rolls 50))
  (dotimes (i rolls)
    (determineVictories (rollDie 3) (rollDie 2)))
  (printWins)
  (cleanWins))

(defun rollDie(die)
  (let ((rolls (make-array die)))
    (dotimes (i die)
      (setf (elt rolls i) (1+ (random 6))))
    rolls))

(defun determineVictories (attackerRolls defenderRolls)
  (let* ((defMax (integerListMax defenderRolls)) (sDefMax (integerListMax (remove defMax defenderRolls :count 1)))
	 (attMax (integerListMax attackerRolls)) (sAttMax (integerListMax (remove attMax attackerRolls :count 1))))
    (cond
      ;;If the defender wins both
      ((and (>= defMax attMax) (>= sDefMax sAttMax))
       (setf (elt wins 0) (1+ (elt wins 0))))
      ;;If the attacker wins both
      ((and (> attMax defMax) (> sAttMax sDefMax))
       (setf (elt wins 2) (1+ (elt wins 2))))
      ;;If they both win once
      ((or (and (>= defMax attMax) (> sAttMax sDefMax)) (and (> attMax defMax) (>= sDefMax sAttMax)))
       (setf (elt wins 1) (1+ (elt wins 1)))))))

(defun integerListMax (intList)  
  (let ((max 0))
    (dotimes (i (length intList)) 
      (when (> (elt intList i) max)
	(setf max (elt intList i))))
    max))

(defun cleanWins () (setf wins (vector 0 0 0)))
(defun printWins ()
  (format t "~%Attacker wins both rolls: ~a~%" (elt wins 2))
  (format t "~%Defender and attacker win once: ~a~%" (elt wins 1))
  (format t "~%Defender wins both rolls: ~a~%" (elt wins 0)))
