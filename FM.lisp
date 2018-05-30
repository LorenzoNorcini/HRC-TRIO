(asdf:operate 'asdf:load-op 'ae2sbvzot)
(use-package :trio-utils)



(defvar pos `(pos_0 pos_1 pos_2 pos_3 pos_4 pos_5 pos_6 pos_7 pos_8 pos_9 pos_10 pos_11 pos_12 pos_13 pos_14 pos_15 pos_16 pos_17 pos_18 pos_19 pos_20 pos_21 pos_22 pos_23 pos_24 pos_25))



(defvar at_least_one_robot_position
  (Alw 
		(eval
			(append `(||)
				(loop for loc in pos collect
					`(-P- ,(read-from-string (format nil "robot_in_~A" loc)))
		  		)
		  	)
	  	)
  	)
)


(defvar one_robot_postion_at_a_time
  (Alw
	   (eval
		  	(append `(&&)
  				(loop for loc1 in pos
        			append (loop for loc2 in pos
                                  when (not(eq loc1 loc2))
                                  					collect 
                                    					`(-> (-P- ,(read-from-string (format nil "robot_in_~A" loc1)))
															(!!
																(-P- ,(read-from-string (format nil "robot_in_~A" loc2)))
							)
						)
					)
				)	
			)
		)
	)
)

(defvar moving_robot
	(Alw 
		(eval
			(append `(&&)
				(loop for loc in pos collect
                	`(-> 
                		(-P- ,(read-from-string (format nil "robot_in_~A" loc)))
					(Futr
						(!! (-P- ,(read-from-string (format nil "robot_in_~A" loc))) )
					1))
				)
		  	)
	  	)
  	)
)


(defvar at_least_one_operator_position
  (Alw 
		(eval
			(append `(||)
				(loop for loc in pos collect
					`(-P- ,(read-from-string (format nil "operator_in_~A" loc)))
		  		)
		  	)
	  	)
  	)
)


(defvar one_operator_postion_at_a_time
  (Alw
	   (eval
		  	(append `(&&)
  				(loop for loc1 in pos
        			append (loop for loc2 in pos
                                  when (not(eq loc1 loc2))
                                  					collect 
                                    					`(-> (-P- ,(read-from-string (format nil "operator_in_~A" loc1)))
															(!!
																(-P- ,(read-from-string (format nil "operator_in_~A" loc2)))
							)
						)
					)
				)	
			)
		)
	)
)

(defvar moving_operator
	(Alw 
		(eval
			(append `(&&)
				(loop for loc in pos collect
                	`(-> 
                		(-P- ,(read-from-string (format nil "operator_in_~A" loc)))
					(Futr
						(!! (-P- ,(read-from-string (format nil "operator_in_~A" loc))) )
					1))
				)
		  	)
	  	)
  	)
)




(ae2sbvzot:zot 10
		(yesterday(&&
					at_least_one_robot_position
					one_robot_postion_at_a_time
					one_operator_postion_at_a_time
					at_least_one_operator_position
					moving_operator
					moving_robot
				))
)
