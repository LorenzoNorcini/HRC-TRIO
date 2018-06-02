(asdf:operate 'asdf:load-op 'ae2sbvzot)
(use-package :trio-utils)

(defvar pos `(pos_0 pos_1 pos_2 pos_3 pos_4 pos_5 pos_6 pos_7 pos_8 pos_9 pos_10 pos_11 pos_12 pos_13 pos_14 pos_15 pos_16 pos_17 pos_18 pos_19 pos_20 pos_21 pos_22 pos_23 pos_24))

(setf adjacency (make-array '(25 25) 
   :initial-contents '(

(0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(1 1 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
(1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0)
(0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0)
(0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0)
(0 0 0 1 1 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0)
(0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0)
(0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0 0)
(0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0 0)
(0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0 0 0 0)
(0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0)
(0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 1 0 0 0)
(0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0)
(0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1 0)
(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 1)
(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 1 1)
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0)
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0 0)
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1 0)
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 1)
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0)

   		)
	)
)

(defun array-slice (arr row)
 (make-array (array-dimension arr 1) 
   :displaced-to arr 
    :displaced-index-offset (* row (array-dimension arr 1))
 )
)

(defun 2d-array-to-list (array)
  (map 'list #'identity array)
)

(defun get_variable_name (prefix suffix) (read-from-string (format nil prefix suffix)))

(defvar at_least_one_robot_position
  (Alw 
		(eval
			(append `(||)
				(loop for loc in pos collect
					`(-P- , (get_variable_name "robot_in_~A" loc))
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
         						`(-> (-P- , (get_variable_name "robot_in_~A" loc1))
									(!!
										(-P- ,(get_variable_name "robot_in_~A" loc2))
							)
						)
					)
				)	
			)
		)
	)
)

(defvar robot_has_moving_state
  (Alw 
  	(&&
		(|| (-P- robot_still) (-P- robot_moving))
		(<-> (-P- robot_still) (!!(-P- robot_moving)))
  	))
)

(defvar robot_has_direction
  (Alw 
  	(&&
		(|| (-P- direction_to_bin) (-P- direction_to_ws) (-P- no_direction))
		(<-> (-P- direction_to_bin) (!!( || (-P- direction_to_ws) (-P-  no_direction) )))
		(<-> (-P- direction_to_ws) (!!( || (-P- direction_to_bin) (-P-  no_direction) )))
		(<-> (-P-  no_direction) (!!( || (-P- direction_to_ws) (-P- direction_to_bin) )))
  	))
)

(defvar robot_movement_moving
  (Alw
  	(<-> (-P- robot_moving)
	   (eval
		  	(append `(||)
  				(loop for i upto (- (list-length pos) 1)
  					append (loop for j upto (- (list-length pos) 1)
  				  		when (not(eq (nth j(2d-array-to-list (array-slice adjacency i))) 0))
      						collect 
     						`(&& (-P- , (get_variable_name "robot_in_pos_~A" i))
											(Futr
												(-P- ,(get_variable_name "robot_in_~A" (nth j pos))) 1
							)
						)
					)
				)	
			)
		)
	))
)

(defvar robot_movement_still
  (Alw 
  	(-> (-P- robot_still)
		(eval
			(append `(||)
				(loop for loc in pos collect
     					` (&& (-P- , (get_variable_name "robot_in_~A" loc))
											(Futr
												(-P- ,(get_variable_name "robot_in_~A" loc)) 1
											)
							)
				)
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
					`(-P- , (get_variable_name "operator_in_~A" loc))
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
         						`(-> (-P- , (get_variable_name "operator_in_~A" loc1))
									(!!
										(-P- , (get_variable_name "operator_in_~A" loc2))
							)
						)
					)
				)
			)
		)
	)
)

(defvar operator_movement
  (Alw
	   (eval
		  	(append `(||)
  				(loop for i upto (- (list-length pos) 1)
  					append (loop for j upto (- (list-length pos) 1)
  				  		when (not(eq (nth j (2d-array-to-list (array-slice adjacency i))) 0))
      						collect 
     							`(&& (-P- , (get_variable_name "operator_in_pos_~A" i))
									(Futr
										(-P- ,(get_variable_name "operator_in_~A" (nth j pos))) 1
							)
						)
					)
				)	
			)
		)
	)
)

(defvar no_op_robot_same_tile
  (Alw 
		(eval
			(append `(&&)
				(loop for loc in pos collect
					`(->
						(-P- , (get_variable_name "robot_in_~A" loc))
							(!! (-P- , (get_variable_name "operator_in_~A" loc)) )
					)
		  		)
		  	)
	  	)
  	)
)

(defvar still_if_operator_adjacent_or_working
  (Alw
  	(<- (-P- robot_still)
	   (eval
		  	(append `(||)
  				(loop for i upto (- (list-length pos) 1)
  					append (loop for j upto (- (list-length pos) 1)
  				  		when (not(eq (nth j(2d-array-to-list (array-slice adjacency i))) 0))
      						collect 
      							`(&& 
      								(-P- , (get_variable_name "robot_in_pos_~A" i))
									(-P- , (get_variable_name "operator_in_~A" (nth j pos)))
								)
							)	
						)	
					)
			)	
	   )
	)
)

(defvar when_arrived_in_ws_work
	(Alw
		(<->
			(&&
				(-P- direction_to_ws)
				(-P- robot_in_pos_15)
			)
			(&&
				(Lasts_ei(-P- working) 6)
			)
		)
	)
)

(defvar working_procedure
	(Alw
		(&&
			(<->
				(&&
					(!!(Past(-P- working) 1))
					(-P- working)
				)
				(-P- started_working)
			)
			(<->
				(&&
					(Past(-P- working) 1)
					(!!(Past(-P- working) 2))
					(-P- working)
				)
				(-P- picked_piece_from_local_bin)
			)
			(<->
				(&&
					(Past(-P- working) 2)
					(!!(Past(-P- working) 3))
					(-P- working)
				)
				(-P- extended_arm_in_ws)
			)
			(<->
				(&&
					(Past(-P- working) 3)
					(!!(Past(-P- working) 4))
					(-P- working)
				)
				(-P- piece_elaborated)
			)
			(<->
				(&&
					(Past(-P- working) 4)
					(!!(Past(-P- working) 5))
					(-P- working)
				)
				(-P- arm_retracted_from_ws)
			)
			(<->
				(&&
					(Past(-P- working) 5)
					(!!(Past(-P- working) 6))
					(-P- working)
				)
				(-P- finished_working)
			)
		)
	)
)

(defvar when_arrived_in_bin_load
	(Alw
		(<->
			(&&
				(-P- direction_to_bin)
				(-P- robot_in_pos_9)
			)
			(&&
				(Lasts_ei(-P- loading) 6)
			)
		)
	)
)

(defvar work_only_in_assigned_zone
	(Alw
		(->
			(-P- working)
			(&&
				(-P- robot_in_pos_15)
			)
		)
	)
)

(defvar load_only_in_assigned_zone
	(Alw
		(->
			(-P- loading)
			(&&
				(-P- robot_in_pos_9)
			)
		)
	)
)

(defvar no_direction_while_operating
	(Alw
		(<->
			(-P- no_direction)
			(||
				(-P- working)
				(-P- loading)
			)
		)
	)
)

(defvar direction_change
	(Alw
		(&&
			(->
				(&&
					(Past(-P- working) 1)
					(!!(-P- working))
				)
				(&&
					(until_ii (-P- direction_to_bin) (-P- robot_in_pos_9))
					(SomF (-P- robot_in_pos_9))
				)
			)
			(->
				(&&
					(Past(-P- loading) 1)
					(!!(-P- loading))
				)
				(&&
					(until_ii (-P- direction_to_ws) (-P- robot_in_pos_15))
					(SomF (-P- robot_in_pos_15))
				)
			)
		)
	)
)

(defvar loading_procedure
	(Alw
		(&&
			(<->
				(&&
					(!!(Past(-P- loading) 1))
					(-P- loading)
				)
				(-P- started_loading)
			)
			(<->
				(&&
					(Past(-P- loading) 1)
					(!!(Past(-P- loading) 2))
					(-P- loading)
				)
				(-P- arm_extended_in_global_bin)
			)
			(<->
				(&&
					(Past(-P- loading) 2)
					(!!(Past(-P- loading) 3))
					(-P- loading)
				)
				(-P- picked_piece_from_global_bin)
			)
			(<->
				(&&
					(Past(-P- loading) 3)
					(!!(Past(-P- loading) 4))
					(-P- loading)
				)
				(-P- arm_retracted_from_global_bin)
			)
			(<->
				(&&
					(Past(-P- loading) 4)
					(!!(Past(-P- loading) 5))
					(-P- loading)
				)
				(-P- placed_piece_in_local_bin)
			)
			(<->
				(&&
					(Past(-P- loading) 5)
					(!!(Past(-P- loading) 6))
					(-P- loading)
				)
				(-P- finished_loading)
			)
		)
	)
)


(ae2sbvzot:zot 50
		(yesterday(&&

					at_least_one_robot_position
					at_least_one_operator_position

					one_robot_postion_at_a_time
					one_operator_postion_at_a_time
					
					robot_movement_moving
					robot_movement_still
					robot_has_moving_state
					operator_movement

					still_if_operator_adjacent_or_working
					no_op_robot_same_tile

					robot_has_direction
					no_direction_while_operating
					direction_change

					load_only_in_assigned_zone
					work_only_in_assigned_zone
					when_arrived_in_ws_work
					when_arrived_in_bin_load
					working_procedure
					loading_procedure

					(-P- robot_in_pos_15)
					(-P- direction_to_ws)
				))
)
