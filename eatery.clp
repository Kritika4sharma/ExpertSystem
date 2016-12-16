;***************************************************************************************
; CLASSES FOR EATERY RECOMMENDATION SYSTEM
;***************************************************************************************


(defclass CUSTOMER      ; CLASS FOR CUSTOMER
	(is-a USER)
	(role concrete)
	(slot area)
	(slot budget))

	
	
(defclass EATERY        ; CLASS FOR EATERY
	(is-a USER)
	(role concrete)
	(slot cuisine)
	(slot suggested_eatery)
	(slot type))

	
	
;***************************************************************************************
; DEFINING  INSTANCES FOR EATERY RECOMMENDATION SYSTEM
;***************************************************************************************



(definstances CUSTOMER-INSTANCES     ; INSTANCE FOR CUSTOMER
	(client of CUSTOMER))

(definstances EATERY-INSTANCES       ; INSTANCE FOR EATERY
	(which_eatery of EATERY))

	
	
;***************************************************************************************
; INPUT TO BE TAKEN FROM CUSTOMER 
;***************************************************************************************



(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
   
   
; RULE TO GET THE INPUT AREA FROM THE CUSTOMER
(defrule GetArea(declare (salience 10))          
    =>
    (printout t crlf)
    (printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)
    (printout t "---------------------WELCOME TO THE EATERY RECOMMENDATION SYSTEM --------------------" crlf)
    (printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)
    (printout t crlf)    
    (send [client] put-area
    (user-input-validation "Which area would you like to go? (civil-lines/jhalwa/naini):  "
   		civil-lines jhalwa naini)))

		
		
;***************************************************************************************
;RULES OF THE EXPERT SYSTEM TO SELECT THE EATERY
;***************************************************************************************


;----------------------------------CIVIL-LINES------------------------------------------



; RULE TO VISIT EATERY IN CIVIL-LINES 
(defrule eat_in_civil_lines
	?ins <- (object (is-a CUSTOMER) (area civil-lines))
	=> 
	(printout t crlf)
	(printout t "Let me select an EATERY suitable in civil-lines you want to go..." crlf crlf)
   	(send [which_eatery] put-type
    (user-input-validation "Choose the type of eatery : Restaurant/Cafet/Fast-Food/Bakery  "
    		restaurant cafet fast-food bakery)))


			
; RULE TO SELECT CAFET IN CIVIL-LINES
(defrule cafet_in_civil_lines
	(and ?ins <- (object (is-a EATERY) (type cafet))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(printout t crlf)
	(printout t "Let me select a cafet suitable in civil-lines you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500/Rs.1000  "
         200 500 1000)))


		 
; RULE TO SELECT CAFET IN CIVIL-LINES FOR 200
(defrule cafet_in_civil_lines_200
	(and ?ins <- (object (is-a EATERY) (type cafet))
	(object (is-a CUSTOMER)(area civil-lines)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Indian Coffee House")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 


	
; RULE TO SELECT CAFET IN CIVIL-LINES FOR 500
(defrule cafet_in_civil_lines_500
	(and ?ins <- (object (is-a EATERY) (type cafet))
	(object (is-a CUSTOMER)(area civil-lines)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Well Bean Cafe")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 


	
; RULE TO SELECT CAFET IN CIVIL-LINES FOR 1000
(defrule cafet_in_civil_lines_1000
	(and ?ins <- (object (is-a EATERY)(type cafet))
	(object (is-a CUSTOMER)(area civil-lines)(budget 1000)))
	=> 
	(send ?ins put-suggested_eatery "Cafe Coffee Day")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 


	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES
(defrule restaurant_in_civil_lines
	(and ?ins <- (object (is-a EATERY)(type restaurant))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(printout t crlf)
	(printout t "Let me select a restaurant suitable in civil-lines you want to go..." crlf crlf)
	(send [which_eatery] put-cuisine
    (user-input-validation "Enter the cuisine of your choice : Chinese/North/South  "
         chinese north south)))
	

	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES FOR CHINESE
(defrule restaurant_in_civil_lines_chinese
	(and ?ins <- (object (is-a EATERY)(type restaurant)(cuisine chinese))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(send ?ins put-suggested_eatery "China Bowl")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 

	
	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES FOR NORTH
(defrule restaurant_in_civil_lines_north
	(and ?ins <- (object (is-a EATERY)(type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(printout t crlf)
	(printout t "Let me select a North Indian restaurant suitable in civil-lines you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500/Rs.1000  "
         200 500 1000)))

		 
		 
; RULE TO SELECT RESTAURANT IN CIVIL-LINES NORTH FOR 200
(defrule restaurant_in_civil_lines_north_200
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area civil-lines)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Celebrations")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 
	

	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES NORTH FOR 500
(defrule restaurant_in_civil_lines_north_500
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area civil-lines)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Moti Mahal")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))
	
	
	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES NORTH FOR 1000
(defrule restaurant_in_civil_lines_north_1000
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area civil-lines)(budget 1000)))
	=> 
	(send ?ins put-suggested_eatery "Kanha Shyam")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
; RULE TO SELECT RESTAURANT IN CIVIL-LINES SOUTH
(defrule restaurant_in_civil_lines_south
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine south))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(send ?ins put-suggested_eatery "Sagar Ratna OR Tamarind Tree")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT FAST FOOD IN CIVIL-LINES
(defrule fast_food_in_civil_lines
	(and ?ins <- (object (is-a EATERY) (type fast-food))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(printout t crlf)
	(printout t "Let me select a fast-food center suitable in civil-lines you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500  "
         200 500 )))
	

	
; RULE TO SELECT FAST FOOD IN CIVIL-LINES FOR 200
(defrule fast_food_in_civil_lines_200
	(and ?ins <- (object (is-a EATERY) (type fast-food))
	(object (is-a CUSTOMER)(area civil-lines)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Haldiram")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
; RULE TO SELECT FAST FOOD IN CIVIL-LINES FOR 500
(defrule fast_food_in_civil_lines_500
	(and ?ins <- (object (is-a EATERY) (type fast-food))
	(object (is-a CUSTOMER)(area civil-lines)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Bikanerwala")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT BAKERY IN CIVIL-LINES
(defrule bakery_in_civil_lines
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area civil-lines)))
	=> 
	(printout t crlf)
	(printout t "Let me select a bakery suitable in civil-lines you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500/Rs.1000  "
         200 500 1000)))
	

	
; RULE TO SELECT BAKERY IN CIVIL-LINES FOR 200
(defrule bakery_in_civil_lines_200
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area civil-lines)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Kamdhenu")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
; RULE TO SELECT BAKERY IN CIVIL-LINES FOR 500
(defrule bakery_in_civil_lines_500
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area civil-lines)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Paradise")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
; RULE TO SELECT BAKERY IN CIVIL-LINES FOR 1000
(defrule bakery_in_civil_lines_1000
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area civil-lines)(budget 1000)))
	=> 
	(send ?ins put-suggested_eatery "Elchico")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
;--------------------------------JHALWA------------------------------------



; RULE TO VISIT EATERY IN JHALWA
(defrule eat_in_jhalwa
	?ins <- (object (is-a CUSTOMER) (area jhalwa))
	=> 
	(printout t crlf)
	(printout t "Let me select an EATERY suitable in jhalwa you want to go..." crlf crlf)
    (send [which_eatery] put-type
  	(user-input-validation "Choose the type of eatery : Restaurant/Dhaba/Bakery  "
  		restaurant dhaba bakery)))

		
		
; RULE TO SELECT RESTAURANT IN JHALWA
(defrule restaurant_in_jhalwa
	(and ?ins <- (object (is-a EATERY) (type restaurant))
	(object (is-a CUSTOMER)(area jhalwa)))
	=> 
	(send ?ins put-suggested_eatery "By The Way")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT BAKERY IN JHALWA
(defrule bakery_in_jhalwa
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area jhalwa)))
	=> 
	(send ?ins put-suggested_eatery "Krishna Bakery")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT DHABA IN JHALWA
(defrule dhaba_in_jhalwa
	(and ?ins <- (object (is-a EATERY) (type dhaba))
	(object (is-a CUSTOMER)(area jhalwa)))
	=> 
	(printout t crlf)
	(printout t "Let me select a dhaba suitable in jhalwa you want to go..." crlf crlf)
	(send [which_eatery] put-cuisine
    (user-input-validation "Choose the cuisine of your choice : Punjabi/North =  "
         punjabi north)))


		 
; RULE TO SELECT DHABA IN JHALWA FOR PUNJABI
(defrule dhaba_in_jhalwa_punjabi
	(and ?ins <- (object (is-a EATERY) (type dhaba)(cuisine punjabi))
	(object (is-a CUSTOMER)(area jhalwa)))
	=> 
	(send ?ins put-suggested_eatery "Punjabi Dhaba")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	


	
; RULE TO SELECT DHABA IN JHALWA FOR NORTH
(defrule dhaba_in_jhalwa_north
	(and ?ins <- (object (is-a EATERY) (type dhaba)(cuisine north))
	(object (is-a CUSTOMER)(area jhalwa)))
	=> 
	(send ?ins put-suggested_eatery "Sanskar OR Malik")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	
	
	
	
;----------------------------------NAINI------------------------------------------



; RULE TO VISIT EATERY IN NAINI
(defrule eat_in_naini
	?ins <- (object (is-a CUSTOMER) (area naini))
	=> 
	(printout t crlf)
	(printout t "Let me select an EATERY suitable in naini you want to go..." crlf crlf)
   	(send [which_eatery] put-type
    (user-input-validation "Choose the type of eatery : Restaurant/Cafet/Fast-Food/Bakery  "
    		restaurant cafet fast-food bakery)))


			
; RULE TO SELECT RESTAURANT IN NAINI
(defrule restaurant_in_naini
	(and ?ins <- (object (is-a EATERY)(type restaurant))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(printout t crlf)
	(printout t "Let me select a restaurant suitable in naini you want to go..." crlf crlf)
	(send [which_eatery] put-cuisine
    (user-input-validation "Enter the cuisine of your choice : North/South/Punjabi/Chinese  "
         north south punjabi chinese)))


		 
; RULE TO SELECT RESTAURANT IN NAINI FOR NORTH
(defrule restaurant_in_naini_north
	(and ?ins <- (object (is-a EATERY)(type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(printout t crlf)
	(printout t "Let me select a North Indian restaurant suitable in naini you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500/Rs.1000  "
         200 500 1000)))


		 
; RULE TO SELECT RESTAURANT IN NAINI NORTH FOR 200
(defrule restaurant_in_naini_north_200
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area naini)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Meherban Restro")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 


	
; RULE TO SELECT RESTAURANT IN NAINI NORTH FOR 500
(defrule restaurant_in_naini_north_500
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area naini)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Bliss Restro")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT RESTAURANT IN NAINI NORTH FOR 1000
(defrule restaurant_in_naini_north_1000
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine north))
	(object (is-a CUSTOMER)(area naini)(budget 1000)))
	=> 
	(send ?ins put-suggested_eatery "Higgins")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))


	
; RULE TO SELECT RESTAURANT IN NAINI FOR SOUTH
(defrule restaurant_in_naini_south
	(and ?ins <- (object (is-a EATERY)(type restaurant)(cuisine south))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(printout t crlf)
	(printout t "Let me select a South Indian restaurant suitable in naini you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.200/Rs.500 "
         200 500)))


		 
; RULE TO SELECT RESTAURANT IN NAINI NORTH FOR 200
(defrule restaurant_in_naini_south_200
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine south))
	(object (is-a CUSTOMER)(area naini)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Sagar Restro")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 
	

	
; RULE TO SELECT RESTAURANT IN NAINI SOUTH FOR 500
(defrule restaurant_in_naini_south_500
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine south))
	(object (is-a CUSTOMER)(area naini)(budget 500)))
	=> 
	(send ?ins put-suggested_eatery "Shri Sai Family Restro")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))

	
	
; RULE TO SELECT RESTAURANT IN NAINI FOR PUNJABI
(defrule restaurant_in_naini_punjabi
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine punjabi))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(send ?ins put-suggested_eatery "Urban Tadka")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	


	
; RULE TO SELECT RESTAURANT IN NAINI FOR CHINESE
(defrule restaurant_in_naini_chinese
	(and ?ins <- (object (is-a EATERY) (type restaurant)(cuisine chinese))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(send ?ins put-suggested_eatery "Chinese Food Villa")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	


	
; RULE TO SELECT BAKERY IN NAINI 
(defrule bakery_in_naini
	(and ?ins <- (object (is-a EATERY) (type bakery))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(send ?ins put-suggested_eatery "Ganga Sweets")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	


	
; RULE TO SELECT CAFET IN NAINI 
(defrule cafet_in_naini
	(and ?ins <- (object (is-a EATERY) (type cafet))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(send ?ins put-suggested_eatery "Aryan Coffee House")
	(printout t crlf)
	(printout t "Processing your Request..."crlf))	


	
; RULE TO SELECT FAST-FOOD IN NAINI
(defrule fast_food_in_naini
	(and ?ins <- (object (is-a EATERY)(type fast-food))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(printout t crlf)
	(printout t "Let me select a restaurant suitable in naini you want to go..." crlf crlf)
	(send [which_eatery] put-cuisine
    (user-input-validation "Enter the cuisine of your choice : North/Chaat  "
         north chaat)))


		 
; RULE TO SELECT FAST-FOOD IN NAINI FOR NORTH
(defrule fast_food_in_naini_north
	(and ?ins <- (object (is-a EATERY)(type fast-food)(cuisine north))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(printout t crlf)
	(printout t "Let me select a North Indian fast food eatery suitable in naini you want to go..." crlf crlf)
	(send [client] put-budget
    (user-input-validation "Enter your budget: Rs.100/Rs.200  "
         100 200)))
	

	
; RULE TO SELECT FAST-FOOD IN NAINI NORTH FOR 100
(defrule fast_food_in_naini_north_100
	(and ?ins <- (object (is-a EATERY) (type fast-food)(cuisine north))
	(object (is-a CUSTOMER)(area naini)(budget 100)))
	=> 
	(send ?ins put-suggested_eatery "Fried Candi")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 
	
	
	
; RULE TO SELECT FAST-FOOD IN NAINI NORTH FOR 200
(defrule fast_food_in_naini_north_200
	(and ?ins <- (object (is-a EATERY) (type fast-food)(cuisine north))
	(object (is-a CUSTOMER)(area naini)(budget 200)))
	=> 
	(send ?ins put-suggested_eatery "Fast Food Plaza")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 
	
	
	
; RULE TO SELECT FAST-FOOD IN NAINI CHAAT
(defrule fast_food_in_naini_chaat
	(and ?ins <- (object (is-a EATERY) (type fast-food)(cuisine chaat))
	(object (is-a CUSTOMER)(area naini)))
	=> 
	(send ?ins put-suggested_eatery "Gokul Chaat")
	(printout t crlf)
	(printout t "Processing your Request..."crlf)) 	
	
	
	
;***************************************************************************************
;PRINTS THE FINAL RECOMMENDATION FOR EATERY
;***************************************************************************************



; RULE TO PRINT THE SUGGESTED EATERY
;(defrule choose_eatery (declare (salience -1))
;	(object (is-a EATERY) (suggested_eatery ?mov))
;	=>
	;(printout t crlf)
	;(printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)
    ;(printout t "----------The recommended EATERY which best suits your needs is : " ?mov crlf)
    ;(printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)
	;(printout t crlf)
	;(printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)
    ;(printout t "----------------THANK YOU FOR USING OUR EATERY RECOMMENDATION SYSTEM-----------------"crlf)
    ;(printout t "$$$$$$$$$$***************************************************************$$$$$$$$$$$$" crlf)


	
; RULE TO PRINT THE SUGGESTED EATERY
(defrule choose_EATERY (declare (salience -1))
	(object (is-a EATERY) (suggested_eatery ?mov))
	=>
	(printout t crlf)
	(printout t "-----------------------------------------------------------------------------" crlf)
    (printout t "The recommended EATERY which best suits your needs is: " ?mov crlf)
    (printout t "-----------------------------------------------------------------------------" crlf))