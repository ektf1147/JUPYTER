MODULE NMSC_Mod_Print_Message

   CONTAINS

! ------------------------------------------------------------------------------
! Printing error message
! ------------------------------------------------------------------------------
   SUBROUTINE NMSC_print_ERR(string)
  
   IMPLICIT NONE

   CHARACTER(len=*), INTENT(IN) :: string

   WRITE(*,*) string
   STOP '    Program terminated by error....'

   END SUBROUTINE NMSC_print_ERR


! ------------------------------------------------------------------------------
! Printing state message
! ------------------------------------------------------------------------------
   SUBROUTINE NMSC_print_msg(string)

   IMPLICIT NONE   

   CHARACTER(len=*), INTENT(IN) :: string

   WRITE(*,*) string

   END SUBROUTINE NMSC_print_msg

END MODULE NMSC_Mod_Print_Message
