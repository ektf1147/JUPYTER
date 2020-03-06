! Module: NMSC_Mod_Variable
!
! Description:
!! @ Brief
!!   To define various variables used in the program
!
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Variable

USE NMSC_Mod_Kinds

IMPLICIT NONE


! Total Run Time
REAL :: start
REAL :: finish


! Open file Status
INTEGER :: openstatus     ! file open status    


! GEO filepath, date
CHARACTER(len=80) :: hima_path  ! Himawari-8 path
CHARACTER(len=80) :: hima_date1 ! Himawari-8 date before FY-2E 
CHARACTER(len=80) :: hima_date2 ! Himawari-8 date after FY-2E
CHARACTER(len=80) :: fy2e_path  ! FY-2E path
CHARACTER(len=80) :: fy2e_date  ! FY-2E date


! filename
CHARACTER(len=90) :: fname
CHARACTER(len=90) :: fname2

! GEO Digital Number (level0)
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN1
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN2
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_DN
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN1_d
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN2_d
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_DN_d
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN1_int_d
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN2_int_d

real, dimension(:,:), allocatable :: outputs
real, dimension(:,:), allocatable :: hima_linenumber

! Pyramid Image processing (level1, level2)
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_DN_d_L1
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_DN_d_L2
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN1_int_d_L1
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN1_int_d_L2
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN2_int_d_L1
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_DN2_int_d_L2

! FY-2E Cloud classification
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), ALLOCATABLE :: fy2e_clc
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), ALLOCATABLE :: fy2e_clc_d

! GEO latitude/longitude
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_lon
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_lat
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_lon
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_lat

! Target GEO area
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_lon_d
REAL, DIMENSION(:,:), ALLOCATABLE :: hima_lat_d
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_lon_d
REAL, DIMENSION(:,:), ALLOCATABLE :: fy2e_lat_d

! Normalized Cross correlation
INTEGER :: center_line
INTEGER :: center_col
REAL    :: max_ncc1      ! Himawari-8 before and FY-2E
REAL    :: max_ncc2      ! Himawari-8 after and FY-2E
INTEGER :: max_loc_line1 ! Himawari-8 before and FY-2E
INTEGER :: max_loc_line2 ! Himawari-8 after and FY-2E
INTEGER :: max_loc_col1  ! Himawari-8 before and FY-2E
INTEGER :: max_loc_col2  ! Himawari-8 after and FY-2E

REAL    :: std_ncc1
REAL    :: std_ncc2


! Gaussian fitting for sub-pixel accuracy in NCC
REAL :: dx1 ! Himawari-8 before and FY-2E
REAL :: dy1 ! Himawari-8 before and FY-2E
REAL :: dx2 ! Himawari-8 after and FY-2E
REAL :: dy2 ! Himawari-8 after and FY-2E


! Do loop
INTEGER :: i
 
! Parallax correction variable
REAL :: height
REAL :: proj_himalat1
REAL :: proj_himalon1
REAL :: proj_himalat2
REAL :: proj_himalon2
REAL :: proj_himalat
REAL :: proj_himalon
REAL :: proj_fy2elat
REAL :: proj_fy2elon
REAL :: corr_himalat
REAL :: corr_himalon
REAL :: corr_fy2elat 
REAL :: corr_fy2elon
REAL :: CTHs_lat
REAL :: CTHs_lon
REAL :: dist, dist2
REAL :: min_dist
REAL :: dist_before
REAL :: corr_himalat_before
REAL :: corr_himalon_before
REAL :: corr_fy2elat_before
REAL :: corr_fy2elon_before

! Calculated Cloud Top Height (CTH)
REAL :: CTHs, CTHs2

! angle
REAL :: sza_hima, sza_fy2e, azi_hima, azi_fy2e
 

END MODULE NMSC_Mod_Variable
