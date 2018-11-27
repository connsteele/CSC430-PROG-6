program assgn6
implicit none

! Interpreter Input Data Type
TYPE myExprc
    REAL :: num
    CHARACTER :: char
    LOGICAL :: bool
    INTEGER :: typeKey !Would be 1 (for Reals), 2 (for Characters), and 3 (for Bools)
END TYPE myExprc

TYPE myValue
    REAL :: num
    CHARACTER :: char
    LOGICAL :: bool
    INTEGER :: typeKey !Would be 1 (for Reals), 2 (for Characters), and 3 (for Bools)
END TYPE myValue

! Declare the array size for the input
INTEGER :: arrSize = 3
! Vars to hold output of interp
TYPE(myValue) :: returnVal1
TYPE(myValue) :: returnVal2
! Input Arrays to Interp
TYPE(myExprc),DIMENSION(:),allocatable :: inputArray1
TYPE(myExprc),DIMENSION(:),allocatable :: inputArray2

allocate(inputArray1(arrSize))
inputArray1(1)%char = '+'
inputArray1(1)%typeKey = 2
inputArray1(2)%num = 3
inputArray1(2)%typeKey = 1
inputArray1(3)%num = 1
inputArray1(3)%typeKey = 1

allocate(inputArray2(arrSize))
inputArray2(1)%char = '-'
inputArray2(1)%typeKey = 2
inputArray2(2)%num = 3
inputArray2(2)%typeKey = 1
inputArray2(3)%num = 1
inputArray2(3)%typeKey = 1

returnVal1 = interpret(inputArray1, arrSize)
returnVal2 = interpret(inputArray2, arrSize)

print *, returnVal1%num
print *, returnVal2%num

deallocate(inputArray1)
deallocate(inputArray2)

!----- Functions -----!
! Interpreter: Turn myExprc type into myValue type
<<<<<<< HEAD
do i = 1, arrSize
    ! Check for Arithmatic Operators
    if (i == 1 .AND. inputArray(i)%typeKey == 2 ) then
        ! Check for +
        if (inputArray(1)%char == '+' .AND. arrSize >= 3) then
            returnVal = inputArray(2)%num + inputArray(3)%num
        else if (inputArray(1)%char == '-' .AND. arrSize >= 3) then
            returnVal = inputArray(2)%num - inputArray(3)%num
        else if (inputArray(1)%char == '*' .AND. arrSize >= 3) then
            returnVal = inputArray(2)%num * inputArray(3)%num
        else if (inputArray(1)%char == '/' .AND. arrSize >= 3 .AND. inputArray(3)%num .NE. 0) then
            returnVal = inputArray(2)%num / inputArray(3)%num
        else
            print *, "Error: No Arithmatic Operator in Input"
        endif
        print *, returnVal ! Print out the return value, change this so it returns instead
    else
        ! Throw an error here, add else if statements to look for other things
    end if
end do

deallocate(inputArray)



words = word("{ + 2 3 }")

print *, words

CONTAINS
	! String to Array function
	function word (str)
		CHARACTER(9) :: str
		CHARACTER(10) :: word(5)
		
		INTEGER :: pos1 = 1, pos2, n = 0, i
		DO
			pos2 = INDEX(str(pos1:), " ")
			IF (pos2 == 0) THEN
			   n = n + 1
			   word(n) = str(pos1:)
			   EXIT
			END IF
			n = n + 1
			word(n) = str(pos1:pos1+pos2-2)
			pos1 = pos2+pos1
		END DO
		DO i = 1, n
			WRITE(*,"(2A)", ADVANCE="NO") TRIM(word(i)), "."
		END DO
		RETURN 
			

	end function word
	
	function exprC (inp, inpSize)
		CHARACTER(10) :: inp(5)
		INTEGER :: inpSize
		TYPE(myExprc),DIMENSION(:),allocatable :: exprC
		INTEGER :: i
		allocate(exprC(inpSize - 2))
		do i = 2, inpSize - 1
			if (inp(i) == 'F') then
				exprC(i-1)%logical = .FALSE.
				exprC(i-1)%typeKey = 3
			else if (inp(i) == 'T') then
				exprC(i-1)%logical = .TRUE.
				exprC(i-1)%typeKey = 3
			else if (inp(i) > '0' .AND. inp(i) < '9') then
				read(str , *) exprC(i-1)%num 
				exprC(i-1)%typeKey = 1
			else
				exprC(i-1)%char = inp(i)
				exprC(i-1)%typeKey = 2
			end if
		end do
		RETURN
	end function exprC
	
    recursive function interpret(inputArray, arrSize)
        implicit none
        TYPE(myExprc),DIMENSION(:),allocatable :: inputArray
        INTEGER :: arrSize 
        TYPE(myValue) :: interpret ! what gets returned from the function and its type
        INTEGER :: i
        REAL :: returnNum
        CHARACTER :: returnChar
        LOGICAL :: returnBool

        do i = 1, arrSize
            if (arrSize == 1) then
                  if (inputArray(1)%typeKey == 1) then
                     returnNum = inputArray(1)%num
                     interpret%typeKey = 1
                  else if (inputArray(1)%typeKey == 2) then
                     returnChar = inputArray(1)%char
                     interpret%typeKey = 2
                  else if (inputArray(1)%typeKey == 3) then
                     returnBool = inputArray(1)%bool
                     interpret%typeKey = 3
                  endif
            ! Check for Arithmatic Operators
         else if (i == 1 .AND. inputArray(i)%typeKey == 2 ) then
                ! Assign the type key to the return value
                interpret%typeKey = 1
                if (inputArray(1)%char == '+' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num + inputArray(3)%num
                else if (inputArray(1)%char == '-' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num - inputArray(3)%num
                else if (inputArray(1)%char == '*' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num * inputArray(3)%num
                else if (inputArray(1)%char == '/' .AND. arrSize >= 3 .AND. inputArray(3)%num .NE. 0) then
                        returnNum = inputArray(2)%num / inputArray(3)%num
                else
                        print *, "Error: No Arithmatic Operator in Input"
                exit ! exit the do loop once the input has been evaluated
                endif
            else
            ! Throw an error here, add else if statements to look for other things
            end if
        end do
        interpret%num = returnNum
        interpret%char = returnChar
        interpret%bool = returnBool
    end function interpret
end program
