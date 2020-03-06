MODULE NMSC_Mod_FileIO

  USE NMSC_Mod_Const
  USE NMSC_Mod_Variable
  USE NMSC_Mod_Print_Message

   CONTAINS

! ----------------------------------------------------------------------------
! Open file
! ----------------------------------------------------------------------------
   SUBROUTINE NMSC_Open_File(iunit,       & !in
                             filename,    & !in
                             fileform,    & !in (optional)
                             filestatus)    !in (optional)

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: iunit
   CHARACTER(len=*), INTENT(IN) :: filename
   CHARACTER(len=*), INTENT(IN), OPTIONAL :: fileform
   CHARACTER(len=*), INTENT(IN), OPTIONAL :: filestatus


! Himawari-8 DN, lat/lon file, FY-2E lat/lon file
   IF ( PRESENT(fileform) .and. PRESENT(filestatus) ) THEN
      OPEN(iunit, FILE = filename, FORM   = fileform,   & 
                                   STATUS = filestatus, &
                                   IOSTAT = openstatus)
! Other files
   ELSE
      OPEN(iunit, FILE = filename, STATUS = 'unknown', &
                                   IOSTAT = openstatus)
   ENDIF

! Open test
   IF (openstatus /= 0) &
      CALL NMSC_Print_ERR('   Fail to open Output file....')
        
   END SUBROUTINE NMSC_Open_File

   
! ----------------------------------------------------------------------------
! Close file
! ----------------------------------------------------------------------------
   SUBROUTINE NMSC_Close_File(iunit)

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: iunit

   CLOSE(iunit)
  
   END SUBROUTINE NMSC_Close_File

END MODULE NMSC_Mod_FileIO

