! Module: NMSC_Mod_Read_Images
!
! Description:
!! @ Brief
!!   To Read two GEO data
!!     1. NMSC_Read_NML : Reading GEO filepath and filedate
!!     2. NMSC_Read_GEO : Reading GEO Digital Number and DN to reflectance
!!     3. NMSC_Read_latlon : Reading GEO latitude/longitude information
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!


MODULE NMSC_Mod_Read_Images

USE NMSC_Mod_Kinds
USE NMSC_Mod_Const
USE NMSC_Mod_Variable
USE NMSC_Mod_FileIO
USE NMSC_Mod_Print_Message
USE HDF5

NAMELIST /input_files/ hima_path,   &
                       hima_date1,  &
                       hima_date2,  &
                       fy2e_path,   &
                       fy2e_date 

CONTAINS


! ------------------------------------------------------------------------------
! 1. Printing GEO combinations
! ------------------------------------------------------------------------------
SUBROUTINE NMSC_Print_GEO


! Which two combinations ?

IF ( use_himawari8 .and. use_fy2e ) THEN
   PRINT*, "    Himawari-8 and FY-2E are Used..."
ELSE IF ( use_himawari8 .and. use_fy4a ) THEN
   PRINT*, "    Himawari-8 and FY-4A are Used..."
ELSE IF ( use_himawari8 .and. use_gk2a ) THEN
   PRINT*, "    Himawari-8 and GK-2A are Used..."
ELSE IF ( use_fy2e .and. use_fy4a ) THEN
   PRINT*, "    FY-2E and FY-4A are Used..."
ELSE IF ( use_fy2e .and. use_gk2a ) THEN
   PRINT*, "    FY-2E and GK-2A are Used..."
ELSE IF ( use_fy4a .and. use_gk2a ) THEN
   PRINT*, "    FY-4A and GK-2A are Used..."
ELSE
   PRINT*, "    Don't know satellite combinations and stop the program!"
   STOP
END IF


END SUBROUTINE NMSC_Print_GEO



! ------------------------------------------------------------------------------
! 2. Perform time interpolation ?
! ------------------------------------------------------------------------------
SUBROUTINE NMSC_Time_Interp_Switch (time_interp_switch)


! input and output variable
LOGICAL, INTENT(INOUT) :: time_interp_switch


! use the fy2e ?
IF (use_fy2e) time_interp_switch = .true.


IF (time_interp_switch) THEN
   PRINT*, "    Because FY-2E is used, Time Interpolation option is Used!"
ELSE
   PRINT*, "    Because FY-2E is not used, Time Interpolation option is NOT used!"
END IF


END SUBROUTINE NMSC_Time_Interp_Switch



! ------------------------------------------------------------------------------
! 1. Reading GEO filepath and filedate
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Read_NML(hima_path,hima_date1,hima_date2,fy2e_path,fy2e_date)

IMPLICIT NONE

! Input variables
CHARACTER(len=*), INTENT(IN) :: hima_path
CHARACTER(len=*), INTENT(IN) :: hima_date1
CHARACTER(len=*), INTENT(IN) :: hima_date2
CHARACTER(len=*), INTENT(IN) :: fy2e_path
CHARACTER(len=*), INTENT(IN) :: fy2e_date

   
OPEN(iunit, FILE = 'NMSC_filename_namelist.nml')
READ(iunit, NML = input_files)
CLOSE(iunit)

END SUBROUTINE NMSC_Read_NML


! ------------------------------------------------------------------------------
! 2. Reading GEO Digital Number (DN)
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Read_GEO(hima_path,hima_date1,hima_date2,fy2e_path,fy2e_date, &
                         hima_DN1, hima_DN2 ,fy2e_DN)


IMPLICIT NONE


! Input variables
CHARACTER(len=*), INTENT(IN) :: hima_path
CHARACTER(len=*), INTENT(IN) :: hima_date1
CHARACTER(len=*), INTENT(IN) :: hima_date2
CHARACTER(len=*), INTENT(IN) :: fy2e_path
CHARACTER(len=*), INTENT(IN) :: fy2e_date



! Output variables
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN1
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN2
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_DN



! Local variables
INTEGER (HID_T)                          :: file_id   !< file id
INTEGER (HID_T)                          :: data_id   !< data id
INTEGER                                  :: error     !< error flag   
REAL, DIMENSION(fy2e_nlines, fy2e_ncols) :: data_out  
INTEGER(HSIZE_T), DIMENSION(2)           :: data_dims
INTEGER(KIND=hima_int_kinds), DIMENSION(:,:), ALLOCATABLE :: hima_DN1_int
INTEGER(KIND=hima_int_kinds), DIMENSION(:,:), ALLOCATABLE :: hima_DN2_int



! Himawari-8 channel 3 (11 bit), -32768 : error pixels
IF (use_himawari8) THEN

   ALLOCATE (hima_DN1_int(hima_nlines, hima_ncols) )
   ALLOCATE (hima_DN2_int(hima_nlines, hima_ncols) )

   fname = TRIM(hima_path)//'himawari8_ahi_le1b_ch03_fd005ge_'//&
           TRIM(hima_date1)//'.bin'

   CALL NMSC_Open_File(iunit, fname, 'binary', 'old')      
   READ(iunit)  hima_DN1_int
   CALL NMSC_Close_File(iunit)


   fname = TRIM(hima_path)//'himawari8_ahi_le1b_ch03_fd005ge_'//&
           TRIM(hima_date2)//'.bin'
 

   CALL NMSC_Open_File(iunit, fname, 'binary', 'old')
   READ(iunit)  hima_DN2_int
   CALL NMSC_Close_File(iunit)


   ! Transpose array
   hima_DN1_int = TRANSPOSE(hima_DN1_int)
   hima_DN2_int = TRANSPOSE(hima_DN2_int)


   ! Digital number is -999 if dn < 20 (for R<0)
   WHERE(hima_DN1_int < 20) hima_DN1_int = int_unavail
   WHERE(hima_DN2_int < 20) hima_DN2_int = int_unavail


   ! Convert type from integer to real 
   hima_DN1 = hima_dn1_int
   hima_DN2 = hima_dn2_int


   DEALLOCATE(hima_DN1_int)
   DEALLOCATE(hima_DN2_int)
ENDIF


! FY-2E visible channel (6 bit)
IF (use_fy2e) THEN
   fname = TRIM(fy2e_path)//'FY2E_FDI_ALL_NOM_'//TRIM(fy2e_date)//'.hdf'


   ! Initialize FORTRAN interface
   ! Returns non-negative if sucessful and nagative if fail
   CALL h5open_f (error)

   ! Open an existing HDF5 file
   ! Returns a file identifiler if sucessful; otherwise a negative value.
   CALL h5fopen_f (fname, H5F_ACC_RDONLY_F, file_id, error)

   ! Open an existing dataset
   ! Returns a data identifiler if sucessful; otherwise a negative value.
   CALL h5dopen_f (file_id, dname, data_id, error)
 
 
   ! Read the dataset
   data_dims(1) = fy2e_nlines
   data_dims(2) = fy2e_ncols
   CALL h5dread_f (data_id, H5T_NATIVE_REAL, data_out, data_dims, error)


   ! Close the dataset
   CALL h5dclose_f (data_id, error)


   ! Close the file
   CALL h5fclose_f (file_id, error)


   ! Close FORTRAN interface
   CALL h5close_f (error) 


   ! Transpose digital number array
   fy2e_DN = data_out
   fy2e_DN = TRANSPOSE(fy2e_DN)   
ENDIF


! FY-4A channel 2 (12 bit ?) ; 65535 : error pixels
IF (use_fy4a) THEN
   fname = TRIM(fy4a_path)//'FY4A-_AGRI--_N_DISK_1047E_L1-FDI-MULT_NOM_'//TRIM(fy4a_sttime)//&
           '_'//TRIM(fy4a_edtime)//'_0500M_V0001.HDF'


   ! Initialize FORTRAN interface
   ! Returns non-negative if sucessful and nagative if fail
   CALL h5open_f (error)


   ! Open an existing HDF5 file
   ! Returns a file identifiler if sucessful; otherwise a negative value.
   CALL h5fopen_f (fname, H5F_ACC_RDONLY_F, file_id, error)


   ! Open an existing dataset
   ! Returns a data identifiler if sucessful; otherwise a negative value.
   CALL h5dopen_f (file_id, dname_f4, data_id, error)


   ! Read the dataset
   data_dims(1) = fy4a_nlines
   data_dims(2) = fy4a_ncols
   CALL h5dread_f (data_id, H5T_NATIVE_REAL, data_out, data_dims, error)


   ! Close the dataset
   CALL h5dclose_f (data_id, error)


   ! Close the file
   CALL h5fclose_f (file_id, error)


   ! Close FORTRAN interface
   CALL h5close_f (error)


   ! Transpose digital number array
   fy4a_DN = data_out
   fy4a_DN = TRANSPOSE(fy4a_DN)
ENDIF
!

! GK-2A channel 2 
IF (use_gk2a) THEN
   ! TBD 
ENDIF

END SUBROUTINE NMSC_Read_GEO


! ------------------------------------------------------------------------------
! 3. Reading GEO latitude/longitude information
! ------------------------------------------------------------------------------  

SUBROUTINE NMSC_Read_latlon(hima_path, fy2e_path, hima_lon, hima_lat, &
                            fy2e_lon, fy2e_lat)

IMPLICIT NONE

! Input variables
CHARACTER(len=*),     INTENT(IN)    :: hima_path
CHARACTER(len=*),     INTENT(IN)    :: fy2e_path

! Output variables
REAL, DIMENSION(:,:), INTENT(OUT)   :: hima_lon
REAL, DIMENSION(:,:), INTENT(OUT)   :: hima_lat
REAL, DIMENSION(:,:), INTENT(OUT)   :: fy2e_lon
REAL, DIMENSION(:,:), INTENT(OUT)   :: fy2e_lat

! Local variable
!REAL, DIMENSION(:,:,:), ALLOCATABLE :: fy2e_latlon


! 3.1. Himawari-8 
! ---------------

! Latitude
fname = TRIM(hima_path)//'Lat_500m.bin' 

CALL NMSC_Open_File(iunit,fname,'binary','old') 
READ(iunit)  hima_lat
CALL NMSC_Close_File(iunit)


! Longitude
fname = TRIM(hima_path)//'Lon_500m.bin'
CALL NMSC_Open_File(iunit,fname,'binary','old')
READ(iunit)  hima_lon
CALL NMSC_Close_File(iunit)


! Transpose lat/lon arrays
hima_lon = TRANSPOSE(hima_lon)
hima_lat = TRANSPOSE(hima_lat)


! 3.2. FY-4A
! ----------

ALLOCATE(fy2e_latlon(fy2e_nlines, fy2e_ncols, 2)) ! Allocate 

! Latitude
fname = TRIM(fy2e_path)//'fy4a_lat_500M.bin'

CALL NMSC_Open_File(iunit,fname,'binary','old')
READ(iunit)  fy2e_lat
CALL NMSC_Close_File(iunit)


! Longitude
fname = TRIM(fy2e_path)//'fy4a_lon_500M.bin'

CALL NMSC_Open_File(iunit,fname,'binary','old')
READ(iunit)  fy2e_lon
CALL NMSC_Close_File(iunit)


! Split Latitude and longitude 
! The first 9152*9152 numbers are longitude and 
! the second 9152*9152 numbers are latitude array. 
! 300 is the fill value
! (Xiam Di, Personal Communication)
!fy2e_lon(:,:) = fy2e_latlon(:,:,1) + fy2elon ! Add FY-2E longitude
!fy2e_lat(:,:) = fy2e_latlon(:,:,2)


! Transpose lat/lon arrays
fy2e_lon = TRANSPOSE(fy2e_lon)
fy2e_lat = TRANSPOSE(fy2e_lat)

DEALLOCATE(fy2e_latlon) ! deallocate 

END SUBROUTINE NMSC_Read_latlon


!------------------------------------------------------------------------------
! 4. Reading FY-2E Cloud Classification (CLC) information
!------------------------------------------------------------------------------

SUBROUTINE NMSC_Read_CLC(fy2e_path, fy2e_date, fy2e_clc)

IMPLICIT NONE

! Input variable
CHARACTER(len=*), INTENT(IN) :: fy2e_path
CHARACTER(len=*), INTENT(IN) :: fy2e_date

! Output variable
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), INTENT(OUT) :: fy2e_clc

! Local variables
INTEGER (HID_T)                                  :: file_id    !< file id
INTEGER (HID_T)                                  :: data_id    !< data id
INTEGER                                          :: error      !< error flag   
REAL, DIMENSION(fy2e_nlines_low, fy2e_ncols_low) :: data_out
INTEGER(HSIZE_T), DIMENSION(2)                   :: data_dims
INTEGER                                          :: i, j, k, l !< do loop element


! 4.1. FY-2E Cloud Classification (CLC) information
!      Valid Data Range : 0 - 100 (255 ; fill value)
! -------------------------------------------------------
fname = TRIM(fy2e_path)//'FY2E_FDI_ALL_NOM_'//TRIM(fy2e_date)//'.hdf'

! Initialize FORTRAN interface
! Returns non-negative if sucessful and nagative if fail
CALL h5open_f (error)
 
! Open an existing HDF5 file
! Returns a file identifiler if sucessful; otherwise a negative value.
CALL h5fopen_f (fname, H5F_ACC_RDONLY_F, file_id, error)
 
! Open an existing dataset
! Returns a data identifiler if sucessful; otherwise a negative value.
CALL h5dopen_f (file_id, dname2, data_id, error)
 
! Read the dataset
data_dims(1) = fy2e_nlines_low
data_dims(2) = fy2e_ncols_low
CALL h5dread_f (data_id, H5T_NATIVE_REAL, data_out, data_dims, error)
 
! Close the dataset
CALL h5dclose_f (data_id, error)
 
! Close the file
CALL h5fclose_f (file_id, error)
 
! Close FORTRAN interface
CALL h5close_f (error)
 
! Transpose clc array
data_out = TRANSPOSE(data_out)


! 4.2 Congrid arrays ( [2288, 2288] -> [9152, 9152] )
! ---------------------------------------------------

DO j = 1, fy2e_ncols_low
DO i = 1, fy2e_nlines_low
   fy2e_clc(4*i-3, 4*j-3) = data_out(i,j)
   DO l = 0, 3
   DO k = 0, 3
      fy2e_clc(4*i-3+k, 4*j-3+l) = data_out(i,j)
   ENDDO
   ENDDO
ENDDO
ENDDO


! 4.3. Fill value (255 or -1)
! -255 : fill value
! -1 : outside area
! ---------------------------
WHERE( fy2e_clc == 255) fy2e_clc = -1
!WHERE( fy2e_clc == -1)  fy2e_clc = int_unavail


END SUBROUTINE NMSC_Read_CLC


END MODULE NMSC_Mod_Read_Images
