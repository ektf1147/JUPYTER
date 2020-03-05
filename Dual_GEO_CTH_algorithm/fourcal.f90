MODULE fourcal

CONTAINS

    SUBROUTINE add(a,b,c)

    INTEGER, INTENT(in)  :: a, b
    INTEGER, INTENT(out) :: c 
    c = a + b

    END SUBROUTINE add
    
    SUBROUTINE sub(a,b,c)

    INTEGER, INTENT(in)  :: a, b
    INTEGER, INTENT(out) :: c 
    c = a - b

    END SUBROUTINE sub

END MODULE