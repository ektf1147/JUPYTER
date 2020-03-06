! Module: NMSC_Mod_Const
!
! Description:
!! @ Brief
!!   To define various constants used in the program
!
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Const

IMPLICIT NONE

! -------------------
! General Constant
! -------------------
   
! Open file 
INTEGER, PARAMETER :: iunit  = 10
INTEGER, PARAMETER :: iunit2 = 11

! For calculating cross-correlation
INTEGER, PARAMETER :: nline_ref  = 35 
INTEGER, PARAMETER :: ncol_ref   = nline_ref
INTEGER, PARAMETER :: nline_src  = ((nline_ref-1)/2)*4+1
INTEGER, PARAMETER :: ncol_src   = ((ncol_ref-1)/2)*4+1

INTEGER, PARAMETER :: max_line   = (nline_src-nline_ref)/2
INTEGER, PARAMETER :: max_col    = (ncol_src-ncol_ref)/2
INTEGER, PARAMETER :: num_pixels = nline_ref * ncol_ref
 
! Earth radius information
REAL, PARAMETER    :: Re = 6371.0 ! km
REAL, PARAMETER    :: Radius_eq   = 6378.1370 
REAL, PARAMETER    :: Radius_pole = 6356.7523     
 
! For calculating satellite zenith angle
REAL, PARAMETER    :: pi = 3.141592
REAL, PARAMETER    :: deg2rad = pi / 180. 
REAL, PARAMETER    :: rad2deg = 180. / pi   
 
! NaN value 
REAL, PARAMETER    :: real_unavail      = -999.0 
REAL, PARAMETER    :: real_unavail_dist = 999.0
INTEGER, PARAMETER :: int_unavail       = -999
   
! Minimum distance threshold for cth postprocessing
REAL, PARAMETER    :: mindist_threshold = 2.0 ! km (pixel size in study area)


! NCC threshold for qc
REAL, PARAMETER    :: ncc_threshold = 0.5

! GEO height
REAL, PARAMETER    :: satheight = 42164.0 ! km

! Number of Iteration for calculating CTHs
INTEGER, PARAMETER :: Niter = 401


! Logical variable for using GEOs
! Default combinations : Himawari-8 and FY-4A
! however, any two combinations are possible.
! .true. : use, .false. : no use
LOGICAL, PARAMETER :: use_himawari8 = .True.
LOGICAL, PARAMETER :: use_fy2e = .False.
LOGICAL, PARAMETER :: use_fy4a = .False. 
LOGICAL, PARAMETER :: use_gk2a = .True.



! -------------------
! Himawari-8 Constant
! ------------------- 
   
INTEGER, PARAMETER :: hima_nlines = 22000      ! Himawari-8 y-dimension 
INTEGER, PARAMETER :: hima_ncols  = 22000      ! himawari-8 x-dimension
     
! Case 1 area (2017.11.28. 03:30 UTC, MODIS)
!INTEGER, PARAMETER :: stline_h = 3900
!INTEGER, PARAMETER :: edline_h = 5500
!INTEGER, PARAMETER :: stcol_h  = 6200
!INTEGER, PARAMETER :: edcol_h  = 8720

! Case 2 area 
! 1) 2017.11.03. 05:30 UTC (deep convective cloud)
! 2) 2017.11.19. 05:30 UTC (multi-layer cloud)
!INTEGER, PARAMETER :: stline_h = 3300 
!INTEGER, PARAMETER :: edline_h = 7600
!INTEGER, PARAMETER :: stcol_h  = 5300
!INTEGER, PARAMETER :: edcol_h  = 10700

! Case 3 area (2017.11.08. 05:30 UTC, CALIOP)
!INTEGER, PARAMETER :: stline_h = 4800
!INTEGER, PARAMETER :: edline_h = 7500
!INTEGER, PARAMETER :: stcol_h  = 5200
!INTEGER, PARAMETER :: edcol_h  = 7700


! Case 4 area (Korea)
!INTEGER, PARAMETER :: stline_h = 3051
!INTEGER, PARAMETER :: edline_h = 4647
!INTEGER, PARAMETER :: stcol_h  = 8121
!INTEGER, PARAMETER :: edcol_h  = 9872


! Korea Target area (2019.11.22)
INTEGER, PARAMETER :: stline_h = 800!1700!2500
INTEGER, PARAMETER :: edline_h = 5700!5200!4647
INTEGER, PARAMETER :: stcol_h  = 5000!7000
INTEGER, PARAMETER :: edcol_h  = 10872!9872


INTEGER, PARAMETER :: hima_coldim_d  = edcol_h - stcol_h + 1
INTEGER, PARAMETER :: hima_linedim_d = edline_h - stline_h + 1

REAL, PARAMETER    :: himalat     = 0.0
REAL, PARAMETER    :: himalon     = 140.7
  
REAL, PARAMETER    :: Hima_TimeResol = 600. !< seconds ( 10 min) 
 


! -------------------
! FY-2E Constant
! -------------------

CHARACTER(LEN=16), PARAMETER :: dname  = 'NOMChannelVIS1KM'    
CHARACTER(LEN=22), PARAMETER :: dname2 = 'NOMCloudClassification'

INTEGER, PARAMETER :: fy2e_nlines   = 9152   ! y-direction (south-north)
INTEGER, PARAMETER :: fy2e_ncols    = 9152   ! x-direction (west-east)

INTEGER, PARAMETER :: fy2e_nlines_low = 2288 ! y-direction (5 km resol.) 
INTEGER, PARAMETER :: fy2e_ncols_low  = 2288 ! x-direction (5 km resol.)

! Case 1 area (2017.11.28.03:30 UTC, MODIS)
!INTEGER, PARAMETER :: stline_f = 1820 
!INTEGER, PARAMETER :: edline_f = 2400 
!INTEGER, PARAMETER :: stcol_f  = 6500
!INTEGER, PARAMETER :: edcol_f  = 7150

! 5 km resol. (same as above region)
!INTEGER, PARAMETER :: stline_f = 450
!INTEGER, PARAMETER :: edline_f = 575
!INTEGER, PARAMETER :: stcol_f  = 1650 
!INTEGER, PARAMETER :: edcol_f  = 1785 

! Case 2 area 
! 1) 2017.11.03. 05:30 UTC (deep convective cloud)
! 2) 2017.11.19. 05:30 UTC (multi-layer cloud)
!INTEGER, PARAMETER :: stline_f = 2000
!INTEGER, PARAMETER :: edline_f = 3000
!INTEGER, PARAMETER :: stcol_f  = 6800
!INTEGER, PARAMETER :: edcol_f  = 7600

! Case 3 area 
! 2017.11.08.05:30 UTC, 2017.09.21, CALIOP)
!INTEGER, PARAMETER :: stline_f = 2000
!INTEGER, PARAMETER :: edline_f = 3205
!INTEGER, PARAMETER :: stcol_f  = 6450
!INTEGER, PARAMETER :: edcol_f  = 7250


! Case 4 area (Korea)
!INTEGER, PARAMETER :: stline_f = 1500
!INTEGER, PARAMETER :: edline_f = 2000
!INTEGER, PARAMETER :: stcol_f  = 6900
!INTEGER, PARAMETER :: edcol_f  = 7200



! Korea Target area (2019.11.21)
INTEGER, PARAMETER :: stline_f = 1000
INTEGER, PARAMETER :: edline_f = 2500
INTEGER, PARAMETER :: stcol_f  = 6600
INTEGER, PARAMETER :: edcol_f  = 7450


INTEGER, PARAMETER :: fy2e_coldim_d  = edcol_f - stcol_f + 1
INTEGER, PARAMETER :: fy2e_linedim_d = edline_f - stline_f + 1

REAL, PARAMETER    :: fy2elat   = 0.0
REAL, PARAMETER    :: fy2elon   = 86.5



! --------------------------------
! Mean acquition time differences
! --------------------------------
REAL, PARAMETER :: fy2e_tottime = 1500. ! 25 min
REAL, PARAMETER :: hima_tottime =  560. ! 9 min 20s

REAL, PARAMETER :: fy2e_linetime = fy2e_tottime/fy2e_nlines
REAL, PARAMETER :: hima_linetime = hima_tottime/hima_nlines


! 09.10 by jhlee
!REAL, PARAMETER :: fy2e_meantime = fy2e_linetime * 2394.
!REAL, PARAMETER :: hima_meantime = hima_linetime * 5494.


END MODULE NMSC_Mod_Const
