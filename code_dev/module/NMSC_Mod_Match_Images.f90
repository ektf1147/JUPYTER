! Module: NMSC_Mod_Match_Images
!
! Description:
!> @brief
!!   Contains subroutines used for image matching
!!     1. NMSC_Image_Pyramid : to construct image pyramid
!!     2. NMSC_Find_Shift  : to match two GEO images (cross-correlation)
   
!> @author
!!   Jonghyuk Lee (Yonsei Univ.)

!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !                           
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!     Version 1   2019.04.24  Image pyramid by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Match_Images

USE NMSC_Mod_Const
USE NMSC_Mod_Variable
USE NMSC_Mod_Print_Message

CONTAINS 


! ------------------------------------------------------------------------------
! 1. Pyramid Processing (both template and input image)
! ------------------------------------------------------------------------------
SUBROUTINE NMSC_Image_Pyramid(fy2e_DN_d, hima_DN1_int_d, hima_DN2_int_d, &
                              fy2e_DN_d_L1,hima_DN1_int_d_L1,hima_DN2_int_d_L1,&
                              fy2e_DN_d_L2,hima_DN1_int_d_L2,hima_DN2_int_d_L2)

IMPLICIT NONE

! Input variable 
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_DN_d
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN1_int_d
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN2_int_d

! Output variable
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_DN_d_L1
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN1_int_d_L1
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN2_int_d_L1
REAL, DIMENSION(:,:), INTENT(OUT) :: fy2e_DN_d_L2
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN1_int_d_L2
REAL, DIMENSION(:,:), INTENT(OUT) :: hima_DN2_int_d_L2

! Local variable
INTEGER :: i, j ! do loop


! Pyramid processing
! level 0 : original input and template
! level 1 : 1/2 or 1/3 of level 0
! level 2 : 1/2 or 1/3 of level 1


! level 1 processing (1/3 of level 0)
DO j = 1, fy2e_coldim_d/3
DO i = 1, fy2e_linedim_d/3
   fy2e_DN_d_L1(i,j) = SUM( fy2e_DN_d(3*i-2:3*i, 3*j-2:3*j) ) / 9.
   hima_DN1_int_d_L1(i,j) = SUM( hima_DN1_int_d(3*i-2:3*i, 3*j-2:3*j) ) / 9.
   hima_DN2_int_d_L1(i,j) = SUM( hima_DN2_int_d(3*i-2:3*i, 3*j-2:3*j) ) / 9.
ENDDO
ENDDO

! level 2 processing (1/3 of level 1)
DO j = 1, fy2e_coldim_d/9
DO i = 1, fy2e_linedim_d/9
   fy2e_DN_d_L2(i,j) = SUM( fy2e_DN_d_L1(3*i-2:3*i, 3*j-2:3*j) ) / 9.
   hima_DN1_int_d_L2(i,j) = SUM( hima_DN1_int_d_L1(3*i-2:3*i, 3*j-2:3*j) ) / 9.
   hima_DN2_int_d_L2(i,j) = SUM( hima_DN2_int_d_L1(3*i-2:3*i, 3*j-2:3*j) ) / 9.
ENDDO
ENDDO


END SUBROUTINE NMSC_Image_Pyramid

! ------------------------------------------------------------------------------
! 2. To match two GEO images (using Normalized Cross Correlation, NCC)
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Image_Matching(hima_DN_int_d, fy2e_DN_d, center_line, center_col, & 
                               max_ncc, max_loc_line, max_loc_col, std_ncc)

                                                         
IMPLICIT NONE 
 
! Input variable
REAL, DIMENSION(:,:), INTENT(IN) :: hima_DN_int_d
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_DN_d
INTEGER, INTENT(IN)  :: center_line
INTEGER, INTENT(IN)  :: center_col

! Output variable
REAL, INTENT(OUT)    :: max_ncc
INTEGER, INTENT(OUT) :: max_loc_line
INTEGER, INTENT(OUT) :: max_loc_col
REAL, INTENT(OUT)    :: std_ncc

! Local variable
INTEGER :: left
INTEGER :: right
INTEGER :: up
INTEGER :: down
INTEGER :: x
INTEGER :: y
REAL    :: mean_tmp
REAL    :: mean_sub
REAL    :: std_tmp
REAL    :: std_sub
REAL    :: ncc
REAL    :: mean_ncc


REAL, DIMENSION(:), ALLOCATABLE :: arr_tmp
REAL, DIMENSION(:), ALLOCATABLE :: arr_sub
REAL, DIMENSION(:), ALLOCATABLE :: NCC_matrix_1d
REAL, DIMENSION(:,:), ALLOCATABLE :: NCC_matrix_2d


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Template size : 7 by 7                                               !
! Search area size : 13 by 13                                          !
! The center of two windows are same.                                  !
! Maximum column/line shifts are +- 3 ( (13-7)/2)                      !
! (Zaksek et al., 2013)                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Assign initial values (-999 or -999.)
max_ncc      = real_unavail 
max_loc_line = int_unavail 
max_loc_col  = int_unavail


! Define size parameter of Template image
! column : x-direction (east-west), line : y-direction (north-south)
! max_col = max_line = 3
left  = center_col  - max_col  ! x
right = center_col  + max_col  ! x
up    = center_line - max_line ! y
down  = center_line + max_line ! y


! Allocate Template image (fy2e) and Subimage of input image (himawari-8)
ALLOCATE( arr_tmp(num_pixels) ) !< fy2e 
ALLOCATE( arr_sub(num_pixels) ) !< himawari-8

    
! Select template image (FY-2E)
arr_tmp  = RESHAPE( fy2e_DN_d(up:down, left:right), (/num_pixels/) ) 
mean_tmp = SUM(arr_tmp)/num_pixels
std_tmp  = SQRT( ABS( SUM(arr_tmp*arr_tmp)/num_pixels - (mean_tmp * mean_tmp) ))           


! Allocate correlation coefficient matrix
ALLOCATE( NCC_matrix_2d(-max_line:max_line, -max_col:max_col) )
ALLOCATE( NCC_matrix_1d(num_pixels) )



! Calculate NCC value
DO x = -max_col, max_col   ! negative j : left, positive j : right
DO y = -max_line, max_line ! negative i : up,   positive i : down
     
   ! Select subimage of search image (Himawari-8) 
   arr_sub  = RESHAPE(hima_DN_int_d(up+y:down+y, left+x:right+x),(/num_pixels/))
   mean_sub = SUM(arr_sub)/num_pixels   
   std_sub  = SQRT( ABS( SUM(arr_sub*arr_sub)/num_pixels -(mean_sub*mean_sub)))
    
   ncc = (SUM(arr_tmp*arr_sub)/num_pixels - mean_tmp*mean_sub)/(std_tmp*std_sub)
   

   ! Find maximum NCC and its locations
   IF (ncc > max_ncc) THEN
      max_ncc = ncc
      max_loc_line = y
      max_loc_col  = x              
   ENDIF


   ! To calculate the standard deviation of NCC in correlation matrix
   NCC_matrix_2d(y,x) = ncc
      
ENDDO
ENDDO


! Calculate the standard deviation of NCC in correlation matrix
   NCC_matrix_1d = RESHAPE(NCC_matrix_2d, (/num_pixels/))
   mean_ncc = SUM(NCC_matrix_1d) / num_pixels
   std_ncc  = SQRT( ABS( SUM(NCC_matrix_1d*NCC_matrix_1d)/num_pixels - &
                    (mean_ncc * mean_ncc) ) )

  
!  print*, 'std_ncc:', std_ncc
!  print*, 'max_ncc:', max_ncc
!  print*, 'max_ncc:', maxval(ncc_matrix_2d)

!  PRINT*, 'max_loc_line:', max_loc_line
!  print*, 'max_loc_line:', maxloc(ncc_matrix_2d) 

!  print*, 'max_loc_col:', max_loc_col
!  print*, 'max_loc_col:', maxloc(ncc_matrix_2d)


! Deallocate Template image and Subimage of source image 
DEALLOCATE(arr_tmp)  
DEALLOCATE(arr_sub)  
DEALLOCATE(NCC_matrix_1d)
DEALLOCATE(NCC_matrix_2d)


END SUBROUTINE NMSC_Image_Matching


END MODULE NMSC_Mod_Match_Images
