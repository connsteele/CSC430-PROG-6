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
    CHARACTER(1) :: words(5)
    !Var to hold output of serialize
    CHARACTER(len = 1024) :: serialVal
    ! Input Arrays to Interp
    TYPE(myExprc),DIMENSION(:),allocatable :: inputArray
    
    
    !---- Test Cases ----!
    !words = word("{ + 2 3 }")
    words = (/'{','+', '2', '3','}'/)
    inputArray = exprC(words,5)
    serialVal = serialize(interpret(inputArray, 3))
    print *,words,serialVal

    !words = word("{ - 2 3 }")
    words = (/'{','-', '2', '3','}'/)
    inputArray = exprC(words,5)
    serialVal = serialize(interpret(inputArray, 3))
    print *,words,serialVal
    
    words = (/'{','/', '2', '3','}'/)
    inputArray = exprC(words,5)
    serialVal = serialize(interpret(inputArray, 3))
    print *,words,serialVal
    
    words = (/'{','*', '2', '3','}'/)
    inputArray = exprC(words,5)
    serialVal = serialize(interpret(inputArray, 3))
    print *,words,serialVal


    
    CONTAINS
        ! String to Array function
        !function word (str)
        !    CHARACTER(9) :: str
        !    CHARACTER, dimension(:,:),allocatable :: word
        !    
        !    INTEGER :: pos1 = 1, pos2, n = 0, i, j
        !    allocate (word(3,5))
        !    DO
        !        pos2 = INDEX(str(pos1:), " ")
        !        IF (pos2 == 0) THEN
        !           n = n + 1
        !           DO j = 1, pos2
	!					word(n,1) = str(pos1)
	!			   END DO
     !              EXIT
      !          END IF
       !         n = n + 1
        !        word(n,1) = str(pos1:)
         !       pos1 = pos2+pos1
          !  END DO
            !DO i = 1, n
            !    WRITE(*,"(2A)", ADVANCE="NO") TRIM(word(i)), "."
            !END DO
           ! RETURN 
                
    
       ! end function word
        
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

        ! Turn the input into a string and print it to the console
        function serialize(inp)
            implicit none
            TYPE(myValue) :: inp
            CHARACTER (len = 1024) :: outstr
            CHARACTER (len = 1024) :: serialize !return 0 for no error and 1 for error
    
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
