program assgn6
implicit none

! Interpreter Input Data Type
TYPE mySexp
    REAL :: num
    CHARACTER :: char
END TYPE mySexp


INTEGER :: arrSize = 10 ! Size of array created from input
TYPE(mySexp),DIMENSION(:),allocatable :: inputArray

! Read through the input to find the length of the array

! Allocate memory based on the length and set the elements of the array
INTEGER :: i
allocate(inputArray(arrSize))
do i = 1, arrSize !Loop from 1 to array size
    inputArray(i)%num = i
    inputArray(i)%char = 'c'
end do
print *, inputArray

deallocate(inputArray)

end program