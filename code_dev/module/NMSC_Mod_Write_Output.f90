! Module: NMSC_Mod_Write_Output
!
! Description:
!! @ Brief
!!   To write various output file
!!     1. NMSC_Write_Output : Writing GEO albedo images
!      2. NMSC_Write_Remark : Writing Cloud Top Height (CTH) images
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Write_Output

   USE NMSC_Mod_Const
   USE NMSC_Mod_Variable
   USE NMSC_Mod_Print_Message

   CONTAINS


! -----------------------------------------------------------------------------
! 1. Write CTH and lat/lon output
! ----------------------------------------------------------------------------

SUBROUTINE NMSC_Write_Output(iunit, cths, cths_lat, cths_lon, min_dist, &
                             max_ncc1, max_ncc2) !in


IMPLICIT NONE


! Input variables
INTEGER, INTENT(IN) :: iunit
REAL, INTENT(IN) :: cths
REAL, INTENT(IN) :: cths_lat, cths_lon
REAL, INTENT(IN) :: min_dist
REAL, INTENT(IN) :: max_ncc1, max_ncc2


WRITE(iunit, '(6f10.4)') cths, cths_lat, cths_lon, min_dist, max_ncc1, max_ncc2


END SUBROUTINE NMSC_Write_Output


! ------------------------------------------------------------------------------
! 2. Write remark in output file
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Write_Remark(iunit) ! in


IMPLICIT NONE

! Input variables
INTEGER, INTENT(IN) :: iunit   


WRITE(iunit, *) '--------------------------------------------------------------'
WRITE(iunit, *) '     CTH       Lat      Lon      Min dist     ncc1     ncc2'
WRITE(iunit, *) '--------------------------------------------------------------'


END SUBROUTINE NMSC_Write_Remark


END MODULE NMSC_Mod_Write_Output
