program assgn6
implicit none

! Interpreter Input Data Type
TYPE myExprc
    REAL :: num
    CHARACTER :: char
    LOGICAL :: bool
    INTEGER :: typeKey !Would be 1 (for Reals), 2 (for Characters), and 3 (for Bools)
END TYPE myExprc


! Declare the array size for the input
CHARACTER(10) :: words(5)
INTEGER :: arrSize = 3
REAL :: returnVal !Interp output value
TYPE(myExprc),DIMENSION(:),allocatable :: inputArray

INTEGER :: i
allocate(inputArray(arrSize))
inputArray(1)%char = '+'
inputArray(1)%typeKey = 2
inputArray(2)%num = 3
inputArray(2)%typeKey = 1
inputArray(3)%num = 1
inputArray(3)%typeKey = 1

! Interpreter: Turn myExprc type into myValue type
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



words = word("{ + 2 3")

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

end program




