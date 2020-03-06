! Module: NMSC_Mod_Remap_Images
!
! Description:
!! @ Brief
!!   To remap one GEO data onto other GEO data
!      1. NMSC_Target_area : Slicing target area
!      2. NMSC_Remap_GEO   : Remapping one GEO onto other GEO 
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Remap_Images

USE NMSC_Mod_Const
USE NMSC_Mod_Variable
USE NMSC_Mod_Print_Message

CONTAINS


! ------------------------------------------------------------------------------
! 1. Slicing target area
! ------------------------------------------------------------------------------
SUBROUTINE NMSC_Target_area(hima_lon,hima_lat,hima_DN1,hima_DN2,fy2e_lon,     &
                            fy2e_lat,fy2e_DN,fy2e_clc,hima_lon_d,hima_lat_d,  &
                            hima_DN1_d,hima_DN2_d,fy2e_lon_d,fy2e_lat_d,      &
                            fy2e_DN_d, fy2e_clc_d)

IMPLICIT NONE

! Input variables
REAL, DIMENSION(:,:), INTENT(IN) :: hima_lon
REAL, DIMENSION(:,:), INTENT(IN) :: hima_lat
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN1
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN2
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lon
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lat
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_DN
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), INTENT(IN) :: fy2e_clc

! Output variables
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_lon_d
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_lat_d
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN1_d
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN2_d
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_lon_d
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_lat_d
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_DN_d
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), INTENT(OUT) :: fy2e_clc_d


! 1.1. Himawari-8 Target area
! ---------------------------
hima_lon_d(:,:) = hima_lon(stline_h:edline_h, stcol_h:edcol_h)
hima_lat_d(:,:) = hima_lat(stline_h:edline_h, stcol_h:edcol_h)
hima_DN1_d(:,:) = hima_DN1(stline_h:edline_h, stcol_h:edcol_h)
hima_DN2_d(:,:) = hima_DN2(stline_h:edline_h, stcol_h:edcol_h)


! 1.2. FY-2E Target area
! ----------------------
fy2e_lon_d(:,:) = fy2e_lon(stline_f:edline_f, stcol_f:edcol_f)
fy2e_lat_d(:,:) = fy2e_lat(stline_f:edline_f, stcol_f:edcol_f)
fy2e_DN_d(:,:)  = fy2e_DN(stline_f:edline_f, stcol_f:edcol_f)


! 1.3. FY-2E CLC Target area
! --------------------------
fy2e_clc_d(:,:) = fy2e_clc(stline_f:edline_f, stcol_f:edcol_f)


END SUBROUTINE NMSC_Target_area


! ------------------------------------------------------------------------------
! 2. Remapping images
! ------------------------------------------------------------------------------
SUBROUTINE NMSC_remap_GEO(hima_DN1_d, hima_DN2_d, fy2e_lon_d, fy2e_lat_d, &
                          hima_lon_d, hima_lat_d, hima_DN1_int_d, hima_DN2_int_d)


IMPLICIT NONE 


! Input variables (use lut)
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN1_d
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN2_d
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lon_d
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lat_d
REAL, DIMENSION(:,:), INTENT(IN) :: hima_lon_d
REAL, DIMENSION(:,:), INTENT(IN) :: hima_lat_d


! Output variables (use lut)
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN1_int_d
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN2_int_d


! Local variables (use lut)
REAL, DIMENSION(:,:), ALLOCATABLE :: output
INTEGER :: x, y, k
REAL :: d1, d2, d3, d4
REAL :: nom1, nom2, denom

!ALLOCATE( dist(hima_linedim_d, hima_coldim_d) )
!ALLOCATE( min_loc(2) )
!allocate( diff_lat(hima_linedim_d, hima_coldim_d) )
!allocate( diff_lon(hima_linedim_d, hima_coldim_d) )
!ALLOCATE( a(hima_linedim_d, hima_coldim_d) )
!ALLOCATE( c(hima_linedim_d, hima_coldim_d) )



! use lut! -------------------------------------------------------------------
ALLOCATE ( output( fy2e_coldim_d*fy2e_linedim_d,10 ) )


! Open Remapping LUT
!OPEN(10, file = 'Remap_LUT_2330N_112123E_Himawari_to_FY2E_4points_test.txt', &
!         status = 'unknown', &
!         action = 'read',    &
!         form = 'formatted')

OPEN(10, file = 'Remap_LUT.txt', &
         status = 'unknown', &
         action = 'read',    &
         form = 'formatted')


READ(10, *) ! blank line


! Read LUT
DO y = 1, fy2e_coldim_d*fy2e_linedim_d
   READ(10, *) output(y,:)
ENDDO


! Remapping (Inverse Distance Weighting)
k = 1

DO x = 1, fy2e_coldim_d
DO y = 1, fy2e_linedim_d
   d1 = (fy2e_lat_d( INT(output(k,1)),INT(output(k,2))) - hima_lat_d(INT(output(k,3)), INT(output(k,4))))**2 + &
        (fy2e_lon_d( INT(output(k,1)),INT(output(k,2))) - hima_lon_d(INT(output(k,3)), INT(output(k,4))))**2

   d2 = (fy2e_lat_d( INT(output(k,1)),INT(output(k,2))) - hima_lat_d(INT(output(k,5)), INT(output(k,6))))**2 + &
        (fy2e_lon_d( INT(output(k,1)),INT(output(k,2))) - hima_lon_d(INT(output(k,5)), INT(output(k,6))))**2

   d3 = (fy2e_lat_d( INT(output(k,1)),INT(output(k,2))) - hima_lat_d(INT(output(k,7)), INT(output(k,8))))**2 + &
        (fy2e_lon_d( INT(output(k,1)),INT(output(k,2))) - hima_lon_d(INT(output(k,7)), INT(output(k,8))))**2

   d4 = (fy2e_lat_d( INT(output(k,1)),INT(output(k,2))) - hima_lat_d(INT(output(k,9)), INT(output(k,10))))**2 + &
        (fy2e_lon_d( INT(output(k,1)),INT(output(k,2))) - hima_lon_d(INT(output(k,9)), INT(output(k,10))))**2


   d1 = SQRT(d1) 
   d2 = SQRT(d2)
   d3 = SQRT(d3)
   d4 = SQRT(d4)

  
   denom = 1./(d1*d1) + 1./(d2*d2) + 1./(d3*d3) + 1./(d4*d4)
   nom1 = 1./(d1*d1) * hima_DN1_d(INT(output(k,3)), INT(output(k,4))) + &
          1./(d2*d2) * hima_DN1_d(INT(output(k,5)), INT(output(k,6))) + &
          1./(d3*d3) * hima_DN1_d(INT(output(k,7)), INT(output(k,8))) + &
          1./(d4*d4) * hima_DN1_d(INT(output(k,9)), INT(output(k,10)))

   nom2 = 1./(d1*d1) * hima_DN2_d(INT(output(k,3)), INT(output(k,4))) + &
          1./(d2*d2) * hima_DN2_d(INT(output(k,5)), INT(output(k,6))) + &
          1./(d3*d3) * hima_DN2_d(INT(output(k,7)), INT(output(k,8))) + &
          1./(d4*d4) * hima_DN2_d(INT(output(k,9)), INT(output(k,10)))


   hima_DN1_int_d (y,x) = nom1/denom
   hima_DN2_int_d (y,x) = nom2/denom
   k = k+1
ENDDO
ENDDO


! Close LUT
CLOSE(10)


END SUBROUTINE NMSC_remap_GEO


END MODULE NMSC_Mod_Remap_Images
