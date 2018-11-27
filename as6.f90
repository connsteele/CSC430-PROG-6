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
INTEGER :: arrSize = 3
TYPE(myExprc),DIMENSION(:),allocatable :: inputArray

INTEGER :: i
allocate(inputArray(arrSize))
inputArray(1)%char = '+'
inputArray(1)%typeKey = 2
inputArray(2)%num = 3
inputArray(1)%typeKey = 1
inputArray(3)%num = 1
inputArray(1)%typeKey = 1

print *, inputArray(1)%char

! Loop that reads input array

do i = 0, arrSize
    if (i == 0 .AND. inputArray(i)%typeKey == 2) then
        !check for operators here
    end if 
end do

deallocate(inputArray)



end program