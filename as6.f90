program assgn6
implicit none

!---- Data Types ----!
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

! Vars to hold input
CHARACTER(1) :: words(5)
CHARACTER(2) :: logicWord(5)
CHARACTER(1) :: single(3)
! Var to hold output of serialize
CHARACTER(len = 10) :: serialVal
! Input Arrays to Interp
TYPE(myExprc),DIMENSION(:),allocatable :: inputArray


!---- Test Cases ----!
words = (/'{','+', '2', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: 5.00000"
print *,""

words = (/'{','-', '2', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: -1.00000"
print *,""

words = (/'{','/', '6', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: 2.00000"
print *,""

words = (/'{','*', '2', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: 6.00000"
print *,""

words = (/'{','<', '2', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: T"
print *,""

words = (/'{','>', '2', '3','}'/)
inputArray = exprC(words,5)
serialVal = serialize(interpret(inputArray, 3))
print *,"ExprC Input to interpret: ",words
print *,"Actual Output: ",serialVal
print *,"Exptected Output: F"
print *,""

single = (/'{','4','}'/)
inputArray = exprC(single,3)
serialVal = serialize(interpret(inputArray, 1))
print *,"ExprC Input to interpret: ",single
print *,"Actual Output: ",serialVal
print *,"Exptected Output: 4.00000"
print *,""

single = (/'{','a','}'/)
inputArray = exprC(single,3)
serialVal = serialize(interpret(inputArray, 1))
print *,"ExprC Input to interpret: ",single
print *,"Actual Output: ",serialVal
print *,"Exptected Output: a"
print *,""

!--- Test Cases that won't work, but are have some code in interp ---!
! logicWord = (/'{','>=', '2', '3','}'/)
! inputArray = exprC(logicWord,5)
! serialVal = serialize(interpret(inputArray, 3))
! print *,"ExprC Input to interpret: ",logicWord
! print *,"Actual Output: ",serialVal
! print *,"Exptected Output: F"

! logicWord = (/'{','<=', '2', '3','}'/)
! inputArray = exprC(logicWord,5)
! serialVal = serialize(interpret(inputArray, 3))
! print *,"ExprC Input to interpret: ",logicWord
! print *,"Actual Output: ",serialVal
! print *,"Exptected Output: T"

! logicWord = (/'{','==', '2', '3','}'/)
! inputArray = exprC(logicWord,5)
! serialVal = serialize(interpret(inputArray, 3))
! print *,"ExprC Input to interpret: ",logicWord
! print *,"Actual Output: ",serialVal
! print *,"Exptected Output: F"

! logicWord = (/'{','/=', '2', '3','}'/) ! /= is not equal in fortran
! inputArray = exprC(logicWord,5)
! serialVal = serialize(interpret(inputArray, 3))
! print *,"ExprC Input to interpret: ",logicWord
! print *,"Actual Output: ",serialVal
! print *,"Exptected Output: T"


!---- Functions ----!
CONTAINS
    ! Transform input
    function exprC (inp, inpSize)
        CHARACTER(1) :: inp(3)
        INTEGER :: inpSize
        TYPE(myExprc),DIMENSION(:),allocatable :: exprC
        INTEGER :: i
        allocate(exprC(inpSize - 2))
        do i = 2, inpSize - 1
            if (inp(i) == 'F') then
                exprC(i-1)%bool = .FALSE.
                exprC(i-1)%typeKey = 3
            else if (inp(i) == 'T') then
                exprC(i-1)%bool = .TRUE.
                exprC(i-1)%typeKey = 3
            else if (inp(i) > '0' .AND. inp(i) < '9') then
                read(inp(i) , *) exprC(i-1)%num 
                exprC(i-1)%typeKey = 1
            else
                exprC(i-1)%char = inp(i)
                exprC(i-1)%typeKey = 2
            end if
        end do
        RETURN
    end function exprC
    
    ! TUrn Exprc input into Value
    recursive function interpret(inputArray, arrSize)
        implicit none
        TYPE(myExprc),DIMENSION(:),allocatable :: inputArray
        INTEGER :: arrSize 
        TYPE(myValue) :: interpret
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
            ! Check for Operators
            else if (i == 1 .AND. inputArray(i)%typeKey == 2 ) then
                ! Assign the type key to the return value
                ! Check for Arithmatic Operators
                if (inputArray(1)%char == '+' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num + inputArray(3)%num
                        interpret%typeKey = 1
                else if (inputArray(1)%char == '-' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num - inputArray(3)%num
                        interpret%typeKey = 1
                else if (inputArray(1)%char == '*' .AND. arrSize >= 3) then
                        returnNum = inputArray(2)%num * inputArray(3)%num
                        interpret%typeKey = 1
                else if (inputArray(1)%char == '/' .AND. arrSize >= 3 .AND. inputArray(3)%num .NE. 0) then
                        returnNum = inputArray(2)%num / inputArray(3)%num
                        interpret%typeKey = 1
                ! Check for Logical Operators
                else if (inputArray(1)%char == '>' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num > inputArray(3)%num
                else if (inputArray(1)%char == '>=' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num >= inputArray(3)%num
                else if (inputArray(1)%char == '<' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num < inputArray(3)%num
                else if (inputArray(1)%char == '<=' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num <= inputArray(3)%num
                else if (inputArray(1)%char == '==' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num == inputArray(3)%num
                else if (inputArray(1)%char == '/=' .AND. arrSize >= 3) then
                    interpret%typeKey = 3
                    returnBool = inputArray(2)%num /= inputArray(3)%num
                else
                        print *, "Error: No Arithmatic Operator in Input"
                exit ! exit the do loop once the input has been evaluated
                endif
            else
                ! nothing
            end if
        end do
        interpret%num = returnNum
        interpret%char = returnChar
        interpret%bool = returnBool
    end function interpret

    ! Turn the input into a string
    function serialize(inp)
        implicit none
        TYPE(myValue) :: inp
        CHARACTER (len = 1024) :: outstr
        CHARACTER (len = 1024) :: serialize

        if (inp%typeKey == 1) then
            write(outstr, *) inp%num
        else if (inp%typeKey == 2) then
            write(outstr, *) inp%char
        else if (inp%typeKey == 3) then
            write(outstr, *) inp%bool
        else
            serialize = "Error"
            print *, "Error! Type Key doesn't match existing Type"
        endif

        serialize = outstr
    end function serialize
end program