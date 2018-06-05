(asdf:operate 'asdf:load-op 'ae2sbvzot)
(use-package :trio-utils)


; List of avaialble positions in the Map. 
; Such list of positions can be seen as a 5x5 grid
; due to the definition of the adjacency
;
;	0,	1,	2,	3,	4
;	5,	6,	7,	8,	9
;	10,	11,	12,	13, 	14
;	15,	16, 	17, 	18, 	19
;	20,	21,	22,	23,	24
;

(defvar pos `(pos_0    pos_1    pos_2    pos_3    pos_4
	      pos_5    pos_6    pos_7    pos_8    pos_9 
	      pos_10   pos_11   pos_12   pos_13   pos_14 
	      pos_15   pos_16   pos_17   pos_18   pos_19
	      pos_20   pos_21   pos_22   pos_23   pos_24)
)

; The adjacency matrix of our 5x5 grid.
; Such matrix is used as a definition for the concept adjacency,
; in order to impose constraints on the movement of both Robot and Operator

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

; An utility function to 

(defun array-slice (arr row)
 (make-array (array-dimension arr 1) 
   :displaced-to arr 
    :displaced-index-offset (* row (array-dimension arr 1))
 )
)

; Converts an array to list

(defun 2d-array-to-list (array)
  (map 'list #'identity array)
)

; wrapper for generating variable name

(defun get_variable_name (prefix suffix) (
	read-from-string (format nil prefix suffix))
)

; The Work Station tile and the Global Bin tile,
; respectively pos_20 and pos_4, are not walkable.
; This means that such tiles cannot be the position
; of either the Operator or the Robot

(defvar non_walkable_tiles
	(Alw 
		(&&
			(!! (-P- operator_in_pos_20))
			(!! (-P- operator_in_pos_4))
			(!! (-P- robot_in_pos_20))
			(!! (-P- robot_in_pos_4))
		)
	)
)

; Robot must have at least one position at each time.
; This means that robot_in_pos_i is True for at least one value of i

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

; Robot must have one single position at each time.
; This means that robot_in_pos_i is True for no more than one value of i

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

; The robot is either moving o still

(defvar robot_has_moving_state
  (Alw 
  	(&&
		(|| (-P- robot_still) (-P- robot_moving))
		(<-> (-P- robot_still) (!!(-P- robot_moving)))
  	))
)

; The robot has one and only one of the following direction state:
; - To Global Bin
; - To Work Station
; - No Direction

(defvar robot_has_direction
  (Alw 
  	(&&
		(|| (-P- direction_to_bin) (-P- direction_to_ws) (-P- no_direction))
		(<-> (-P- direction_to_bin) (!!( || (-P- direction_to_ws) (-P-  no_direction) )))
		(<-> (-P- direction_to_ws) (!!( || (-P- direction_to_bin) (-P-  no_direction) )))
		(<-> (-P-  no_direction) (!!( || (-P- direction_to_ws) (-P- direction_to_bin) )))
  	))
)

; The Robot is moving if and only if it will be in an adjecent tile at the next time instant

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

; The Robot is still if and only if it will be in the same tile at the next time instant

(defvar robot_movement_still
  (Alw 
  	(<-> (-P- robot_still)
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

; Operator must have at least one position at each time.
; This means that operator_in_pos_i is True for at least one value of i

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

; Operator must have one single position at each time.
; This means that operator_in_pos_i is True for no more than one value of i

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

; At each time, the operator can either be in the same tile as the previus time instant
; or it can be in an adjecent tile

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
								(||
									(-P- ,(get_variable_name "operator_in_~A" (nth j pos)))
									(-P- ,(get_variable_name "operator_in_pos_~A" i))
								) 1

							)
						)
					)
				)	
			)
		)
	)
)

; Operator and robot cannot be in the same tile.
; This means that, if i=j, robot_in_pos_i and operator_in_pos_j 
; cannot be both true

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

; If the Robot is still if the operator is Adjacent

(defvar still_if_operator_adjacent_or_working
  (Alw
  	(<-> (-P- robot_still)
	   (eval
		  	(append `(||)
  				(loop for i upto (- (list-length pos) 1)
  					append (loop for j upto (- (list-length pos) 1)
  				  		when (not(eq (nth j(2d-array-to-list (array-slice adjacency i))) 0))
      						collect 
							`(||
								(&& 
									(-P- , (get_variable_name "robot_in_pos_~A" i))
									(-P- , (get_variable_name "operator_in_~A" (nth j pos)))
								)
								(||
									(&&
										(-P- robot_in_pos_15)
										(-P- direction_to_ws)
									)
									(&&
										(-P- robot_in_pos_9)
										(-P- direction_to_bin)
									)
								)
							)
						)	
					)	
				)
			)	
	   	)
	)
)

; When the direction is towards the Work Station and 
; the robot is in the tile 15 i.e. the Working Zone, 
; at the next time instant the working procedure starts

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

; Simply the varius phases of working

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

; When the direction is towards the Global Bin and 
; the robot is in the tile 9 i.e. the Loading Zone, 
; at the next time instant the loading procedure starts

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

; Simply the varius phases of loading

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

; The working state is active only in pos_15, i.e. the Working Zone
; The loading state is active only in pos_9, i.e. the Loading Zone

(defvar work_and_load_only_in_assigned_zone
	(Alw
		(&&
			(->
				(-P- working)
				(&&
					(-P- robot_in_pos_15)
				)
			)
			(->
				(-P- loading)
				(&&
					(-P- robot_in_pos_9)
				)
			)
		)
	)
)

; During working or loading the direction state is no_direction
; and the direction state is no_direction only while working or loading

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

; After working the direction state changes to direction_to_bin
; until the destination is reached which will happen at some point in the future
; After loading the direction state changes to direction_to_ws
; until the destination is reached which will happen at some point in the future

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



(ae2sbvzot:zot 50
		(yesterday(&&
					non_walkable_tiles

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

				work_and_load_only_in_assigned_zone
					when_arrived_in_ws_work
					when_arrived_in_bin_load
					working_procedure
					loading_procedure

					; Starting Conditions

					(-P- robot_in_pos_15)
					(-P- direction_to_ws)
				))
)
