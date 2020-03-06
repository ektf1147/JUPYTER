! Module: NMSC_Mod_Alloc_Arrays
!
! Description:
!! @ Brief
!!   To Allocate/Deallocate arrays used in the program
!
!> @ Author
!>   Jonghyuk Lee (Yonsei Univ.)
!
!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Alloc_Arrays
 
USE NMSC_Mod_Const
USE NMSC_Mod_Variable

CONTAINS

! ------------------------------------------------------------------------------
! Allocating arrays
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Alloc_arrays
     
ALLOCATE(hima_DN1(hima_nlines, hima_ncols))
ALLOCATE(hima_DN2(hima_nlines, hima_ncols))
ALLOCATE(fy2e_DN(fy2e_nlines, fy2e_ncols))

ALLOCATE(hima_lon(hima_nlines, hima_ncols))
ALLOCATE(hima_lat(hima_nlines, hima_ncols))
ALLOCATE(fy2e_lon(fy2e_nlines, fy2e_ncols))
ALLOCATE(fy2e_lat(fy2e_nlines, fy2e_ncols))
ALLOCATE(fy2e_clc(fy2e_nlines, fy2e_ncols))

ALLOCATE(hima_lon_d(hima_linedim_d, hima_coldim_d))
ALLOCATE(hima_lat_d(hima_linedim_d, hima_coldim_d))
ALLOCATE(hima_DN1_d(hima_linedim_d, hima_coldim_d))
ALLOCATE(hima_DN2_d(hima_linedim_d, hima_coldim_d))
ALLOCATE(fy2e_lon_d(fy2e_linedim_d, fy2e_coldim_d))
ALLOCATE(fy2e_lat_d(fy2e_linedim_d, fy2e_coldim_d))
ALLOCATE(fy2e_DN_d(fy2e_linedim_d, fy2e_coldim_d))

ALLOCATE(fy2e_clc_d(fy2e_linedim_d, fy2e_coldim_d))

ALLOCATE(hima_DN1_int_d(fy2e_linedim_d, fy2e_coldim_d)) 
ALLOCATE(hima_DN2_int_d(fy2e_linedim_d, fy2e_coldim_d))

ALLOCATE(fy2e_DN_d_L1(fy2e_linedim_d/3, fy2e_coldim_d/3))
ALLOCATE(fy2e_DN_d_L2(fy2e_linedim_d/9, fy2e_coldim_d/9))
ALLOCATE(hima_DN1_int_d_L1(fy2e_linedim_d/3, fy2e_coldim_d/3))
ALLOCATE(hima_DN1_int_d_L2(fy2e_linedim_d/9, fy2e_coldim_d/9))
ALLOCATE(hima_DN2_int_d_L1(fy2e_linedim_d/3, fy2e_coldim_d/3))
ALLOCATE(hima_DN2_int_d_L2(fy2e_linedim_d/9, fy2e_coldim_d/9))

allocate( outputs( fy2e_coldim_d*fy2e_linedim_d,10 ) )
allocate(hima_linenumber(fy2e_linedim_d, fy2e_coldim_d))
END SUBROUTINE NMSC_Alloc_arrays


! ------------------------------------------------------------------------------
! Deallocating arrays
! ------------------------------------------------------------------------------

SUBROUTINE NMSC_Dealloc_arrays

IF (ALLOCATED(hima_DN1))          DEALLOCATE(hima_DN1)
IF (ALLOCATED(hima_DN2))          DEALLOCATE(hima_DN2)
IF (ALLOCATED(fy2e_DN))           DEALLOCATE(fy2e_DN)

IF (ALLOCATED(hima_lon))          DEALLOCATE(hima_lon)
IF (ALLOCATED(hima_lat))          DEALLOCATE(hima_lat)
IF (ALLOCATED(fy2e_lon))          DEALLOCATE(fy2e_lon)
IF (ALLOCATED(fy2e_lat))          DEALLOCATE(fy2e_lat)

IF (ALLOCATED(fy2e_clc))          DEALLOCATE(fy2e_clc)

IF (ALLOCATED(hima_lon_d))        DEALLOCATE(hima_lon_d)
IF (ALLOCATED(hima_lat_d))        DEALLOCATE(hima_lat_d)
IF (ALLOCATED(hima_DN1_d))        DEALLOCATE(hima_DN1_d)
IF (ALLOCATED(hima_DN2_d))        DEALLOCATE(hima_DN2_d)
IF (ALLOCATED(fy2e_lon_d))        DEALLOCATE(fy2e_lon_d)
IF (ALLOCATED(fy2e_lat_d))        DEALLOCATE(fy2e_lat_d)
IF (ALLOCATED(fy2e_DN_d))         DEALLOCATE(fy2e_DN_d)

IF (ALLOCATED(fy2e_clc_d))        DEALLOCATE(fy2e_clc_d)

IF (ALLOCATED(hima_DN1_int_d))    DEALLOCATE(hima_DN1_int_d)
IF (ALLOCATED(hima_DN2_int_d))    DEALLOCATE(hima_DN2_int_d)

IF (ALLOCATED(fy2e_DN_d_L1))      DEALLOCATE(fy2e_DN_d_L1) 
IF (ALLOCATED(fy2e_DN_d_L2))      DEALLOCATE(fy2e_DN_d_L2)
IF (ALLOCATED(hima_DN1_int_d_L1)) DEALLOCATE(hima_DN1_int_d_L1)
IF (ALLOCATED(hima_DN1_int_d_L2)) DEALLOCATE(hima_DN1_int_d_L2)
IF (ALLOCATED(hima_DN2_int_d_L1)) DEALLOCATE(hima_DN2_int_d_L1)
IF (ALLOCATED(hima_DN2_int_d_L2)) DEALLOCATE(hima_DN2_int_d_L2)
IF (ALLOCATED(outputs)) DEALLOCATE(outputs)
IF (ALLOCATED(hima_linenumber)) DEALLOCATE(hima_linenumber)
END SUBROUTINE NMSC_Dealloc_arrays


END MODULE NMSC_Mod_Alloc_Arrays
