! Module: NMSC_Mod_Calc_Parallax
!
! Description:
!> @brief
!!   Contains subroutines used for calculating parallax
!!     1. NMSC_Parallax_Corr   : to calculate parallax distance
!!     2. NMSC_Calc_distance   : to calculate SZA
!!     3. NMSC_Calc_CTH_latlon : to calculate CTH
!> @author
!!   Jonghyuk Lee (Yonsei Univ.)

!-------------------[Code history]---------------------------------------------!
!     Version     Date        Comment                                          !     
!     ----------  ----------  --------------------                             !                           
!     PROTOTYPE   2018.05.28  Original code by Jonghyuk Lee (Yonsei Univ.)     !
!------------------------------------------------------------------------------!

MODULE NMSC_Mod_Calc_Parallax

   USE NMSC_Mod_Const
   USE NMSC_Mod_Variable
   USE NMSC_MOD_Print_Message

   CONTAINS 


! --------------------------------------------------------------------------
! 1. Initialize variables
! ---------------------------------------------------------------------------
   SUBROUTINE NMSC_Init_Var (var1, & !in
                             var2, & !in (optional)
                             var3, & !in (optional)
                             var4, & !in (optional)
                             var5)

   IMPLICIT NONE

   REAL, INTENT(INOUT) :: var1
   REAL, INTENT(INOUT), OPTIONAL :: var2, var3, var4, var5

   var1 = real_unavail_dist
   IF ( present(var2) ) var2 = real_unavail
   IF ( present(var3) ) var3 = real_unavail
   IF ( present(var4) ) var4 = real_unavail
   IF ( present(var5) ) var5 = real_unavail


   END SUBROUTINE NMSC_Init_Var


!------------------------------------------------------------------------------
! 2. To find three projected coordinates (lat, lon)
!------------------------------------------------------------------------------
 
SUBROUTINE NMSC_Find_latlon(fy2e_lat_d, fy2e_lon_d, center_line, center_col, &
                            max_loc_line1, max_loc_col1, max_loc_line2,      &
                            max_loc_col2, proj_himalat1, proj_himalon1,      &
                            proj_himalat2, proj_himalon2, proj_fy2elat,      &
                            proj_fy2elon)
 
IMPLICIT NONE

! Input variable
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lat_d
REAL, DIMENSION(:,:), INTENT(IN) :: fy2e_lon_d
INTEGER, INTENT(IN)              :: center_line
INTEGER, INTENT(IN)              :: center_col
INTEGER, INTENT(IN)              :: max_loc_line1
INTEGER, INTENT(IN)              :: max_loc_col1
INTEGER, INTENT(IN)              :: max_loc_line2
INTEGER, INTENT(IN)              :: max_loc_col2
 
! Output variable
REAL, INTENT(OUT)                :: proj_himalat1
REAL, INTENT(OUT)                :: proj_himalon1
REAL, INTENT(OUT)                :: proj_himalat2
REAL, INTENT(OUT)                :: proj_himalon2
REAL, INTENT(OUT)                :: proj_fy2elat
REAL, INTENT(OUT)                :: proj_fy2elon
 
 
! FY-2E (Template) lat/lon coordinates
proj_fy2elat = fy2e_lat_d(center_line, center_col)
proj_fy2elon = fy2e_lon_d(center_line, center_col)



! Two Himawari-8 (Search) lat/lon coordinates (FY-2E before and FY-2E after)
proj_himalat1 = fy2e_lat_d(center_line+max_loc_line1, center_col+max_loc_col1)
proj_himalon1 = fy2e_lon_d(center_line+max_loc_line1, center_col+max_loc_col1)
proj_himalat2 = fy2e_lat_d(center_line+max_loc_line2, center_col+max_loc_col2)
proj_himalon2 = fy2e_lon_d(center_line+max_loc_line2, center_col+max_loc_col2)
 

END SUBROUTINE NMSC_Find_latlon



! ------------------------------------------------------------------------------
! 3. Himawari-8 time interpolation
! ------------------------------------------------------------------------------

!SUBROUTINE NMSC_Time_Interp(proj_himalat1, proj_himalon1, proj_himalat2,     &
!                            proj_himalon2, time_diff, proj_himalat,proj_himalon)
SUBROUTINE NMSC_Time_Interp(proj_himalat1, proj_himalon1, proj_himalat2,     &
                            proj_himalon2, center_line, center_col, &
                            hima_linenumber, proj_himalat, proj_himalon)


IMPLICIT NONE


! Input variables
REAL, INTENT(IN) :: Proj_himalat1 !< Himawari-8 lat  before FY-2E
REAL, INTENT(IN) :: Proj_himalon1 !< Himawari-8 long before FY-2E
REAL, INTENT(IN) :: Proj_himalat2 !< Himawari-8 lat  after FY-2E
REAL, INTENT(IN) :: Proj_himalon2 !< Himawari-8 long after FY-2E
INTEGER, INTENT(IN) :: center_line
INTEGER, INTENT(IN) :: center_col
REAL, DIMENSION(:,:), INTENT(IN) :: hima_linenumber !< line # of remapped himawari-8


! Output variables
REAL, INTENT(OUT) :: Proj_himalat !< time interpolated Himawari-8 latitude
REAL, INTENT(OUT) :: Proj_himalon !< time interpolated Himawari-8 longitude


! Local Variables
REAL :: Time_Diff !< time diff (sec) bet. FY-2E and Himawari-8


! Calculation of time difference
Time_Diff = ABS( fy2e_linetime*(center_line+stline_f-1) - &
                 hima_linetime*(hima_linenumber(center_line, center_col)+stline_h-1) )


Proj_HimaLat = Proj_HimaLat1 + &
               ((Proj_HimaLat2 - Proj_HimaLat1)/Hima_TimeResol)*Time_Diff
                    
Proj_HimaLon = Proj_HimaLon1 + &
               ((Proj_HimaLon2 - Proj_HimaLon1)/Hima_TimeResol)*Time_Diff


END SUBROUTINE NMSC_Time_Interp


! ----------------------------------------------------------------------------
! 4. Parallax correction (by Marianne Koenig, EUMETSAT)
! ----------------------------------------------------------------------------

   SUBROUTINE NMSC_Parallax_Corr(satheight, & !in
                                 satlat,    & !in
                                 satlon,    & !in
                                 height,    & !in
                                 lat,       & !in
                                 lon,       & !in
                                 latcorr,   & !out
                                 loncorr)     !out


!     Subroutine does a parallax correction for something seen at some
!     height in a position lat/lon by the satellite given by satheight,
!     satlat, satlon
!     The new coordinates are returned in latcorr and loncorr
 
!     Input:
!     satheight (REAL): height of the satellite in km (above earth
!     centre)
!     satlat (REAL): subsatellite latitude (deg, N is positive)
!     satlon (REAL): subsatellite longitude (deg, E is positive)
!     height (REAL): height of the cloud (km)
!     lat (REAL): latitude of the satellite pixel (N is positive)
!     lon (REAL): longitude of the satellite pixel (E is positive)
 
!     Output:
!     latcorr (REAL): corrected latitude accounting for parallax shift
!     (N positive)
!     loncorr (REAL): corrected longitude accounting for parallax shift
!     (E positive)

   IMPLICIT NONE
 
   REAL, INTENT(IN)  :: satheight, satlat, satlon
   REAL, INTENT(IN)  :: lat, lon, height
   REAL, INTENT(OUT) :: latcorr, loncorr
 
       
   REAL(KIND=8) :: radius_ratio
   REAL(KIND=8) :: mean_radius
   REAL(KIND=8) :: dheight
   REAL(KIND=8) :: alat,alon
   REAL(KIND=8) :: asatlat,asatlon
   REAL(KIND=8) :: satlat_geod,satlon_geod
   REAL(KIND=8) :: xsat,ysat,zsat
   REAL(KIND=8) :: xsurf,ysurf,zsurf
   REAL(KIND=8) :: alat_geod
   REAL(KIND=8) :: radius_surf
   REAL(KIND=8) :: radius_ratio_local
   REAL(KIND=8) :: xdiff,ydiff,zdiff
   REAL(KIND=8) :: xfact,zen
   REAL(KIND=8) :: e1,e2,e3
   REAL(KIND=8) :: corr
   REAL(KIND=8) :: xcorr,ycorr,zcorr

!     varius earth radius information
        
   dheight      = satheight       
   radius_ratio = radius_eq/radius_pole
   mean_radius  = 0.5*(radius_eq+radius_pole)
   zdiff        = 0.0

!     angle conversion to radians
 
   asatlat = satlat * deg2rad
   asatlon = satlon * deg2rad
   alat    = lat * deg2rad
   alon    = lon * deg2rad


!     cartesian coordinates for the satellite
!     satlat_geod is the geodetic satellite latitude
 
   satlat_geod = ATAN(TAN(asatlat)*radius_ratio**2)
   xsat = dheight * DCOS(satlat_geod) * DSIN(asatlon)
   ysat = dheight * DSIN(satlat_geod)
   zsat = dheight * DCOS(satlat_geod) * DCOS(asatlon)


!     cartesian coordinates of the surface point
 
   alat_geod = ATAN(TAN(alat)*radius_ratio**2)
   radius_surf = radius_eq/SQRT(COS(alat_geod)**2 +&
                           radius_ratio**2 * DSIN(alat_geod)**2)
   xsurf = radius_surf * DCOS(alat_geod) * DSIN(alon)
   ysurf = radius_surf * DSIN(alat_geod)
   zsurf = radius_surf * DCOS(alat_geod) * DCOS(alon)

!     compute new radius ratio depending on height
 
   radius_ratio_local = ((radius_eq+height)/(radius_pole+height))**2


!     Satellite minus surface location
 
   xdiff = xsat - xsurf
   ydiff = ysat - ysurf
   zdiff = zsat - zsurf

!     compute local zenith angle
 
   xfact = SQRT(xdiff**2 + ydiff**2 + zdiff**2)
   zen = (xdiff*xsurf+ydiff*ysurf+zdiff*zsurf)/(mean_radius*xfact)
   zen = ACOS(zen)
   zen = zen * rad2deg

!     equation to solve for the line of sight at height Z
 
   e1 = xdiff**2 + radius_ratio_local*ydiff**2 + zdiff**2
   e2 = 2.0 * (xsurf*xdiff + radius_ratio_local*ysurf*ydiff +&
               zsurf*zdiff)
   e3 = xsurf**2 + zsurf**2 + radius_ratio_local*ysurf**2 -&
        (radius_eq+height)**2
 
   corr = (SQRT(e2**2 - 4.0*e1*e3) - e2)/2.0/e1

!     corrected surface coordinates
 
   xcorr = xsurf + corr*xdiff
   ycorr = ysurf + corr*ydiff
   zcorr = zsurf + corr*zdiff
 
!     convert back to latitude and longitude
 
   latcorr = ATAN(ycorr/SQRT(xcorr**2 + zcorr**2))
   latcorr = ATAN(TAN(latcorr)/radius_ratio**2) * rad2deg
   loncorr = ATAN2(xcorr,zcorr) * rad2deg

   END SUBROUTINE NMSC_Parallax_Corr


!----------------------------------------------------------------------------
! 5. Calculating distance (Haversine formula)
!----------------------------------------------------------------------------

   SUBROUTINE NMSC_Calc_distance(corr_himalat, & !in
                                 corr_himalon, & !in
                                 corr_fy2elat, & !in
                                 corr_fy2elon, & !in                                
                                 dist)           !out

   IMPLICIT NONE

   
   REAL, INTENT(IN)  :: corr_himalat, corr_himalon
   REAL, INTENT(IN)  :: corr_fy2elat, corr_fy2elon
   REAL, INTENT(OUT) :: dist

   REAL :: lat1, lat2, lon1, lon2
   REAL :: diff_lat, diff_lon
   REAL :: a, c


! Latitude and Longitude of two projected pixels (unit:degree)
! and conversion lat/lon degree to radian
! lat/lon1 : FY-2E (Template)
! lat/lon2 : Himawari-8 (Subimage of source image)

   lat1 = corr_fy2elat * deg2rad
   lon1 = corr_fy2elon * deg2rad

   lat2 = corr_himalat * deg2rad
   lon2 = corr_himalon * deg2rad

  
! Calculate parallax distance using Haversine formula
   
   lat2 = lat1 ! himawari-8 along shift is ignored. 
   diff_lat = lat2 - lat1
   diff_lon = lon2 - lon1

   a = SIN(diff_lat/2) * SIN(diff_lat/2) + &
       COS(lat1) * COS(lat2) * SIN(diff_lon/2) * SIN(diff_lon/2)
   c = 2. * ATAN2( SQRT(a), SQRT(1-a) )

   dist = Re * c


   END SUBROUTINE NMSC_Calc_distance


! ---------------------------------------------------------------------------
! Calculating Satellite zenith/azimuth angle
! Reference : GK2A Testbed Algorithm Modulization & Standardization
! ---------------------------------------------------------------------------

SUBROUTINE NMSC_Calc_Angle(proj_himalat,proj_himalon,proj_fy2elat,&
                           proj_fy2elon,sza_hima,sza_fy2e,azi_hima,azi_fy2e)

IMPLICIT NONE

! Input variable
REAL, INTENT(IN) :: proj_himalat
REAL, INTENT(IN) :: proj_himalon
REAL, INTENT(IN) :: proj_fy2elat
REAL, INTENT(IN) :: proj_fy2elon

! Output variable
REAL, INTENT(OUT) :: sza_hima
REAL, INTENT(OUT) :: sza_fy2e
REAL, INTENT(OUT) :: azi_hima
REAL, INTENT(OUT) :: azi_fy2e

! Local variable
REAL :: lon1
REAL :: lat1
REAL :: lon2
REAL :: lat2
REAL :: beta1
REAL :: beta2

! Conversion degree to radian
lon1 = (proj_fy2elon - fy2elon ) * deg2rad
lat1 = (proj_fy2elat - fy2elat ) * deg2rad

lon2 = (proj_himalon - himalon ) * deg2rad
lat2 = (proj_himalat - himalat ) * deg2rad

! beta angle calculation
beta1 = ACOS ( COS(lat1) * COS(lon1) )
beta2 = ACOS ( COS(lat2) * COS(lon2) )

! Sensor zenith angle for each pixel measured in radian from nadir
! (0 ~ 90 degree)
! Should be checked for limits:
! arcsine input limit of [-1, 1]
  sza_fy2e = ASIN( MAX(-1.0, MIN(1.0, &
         42164.0 * SIN(beta1)/ SQRT( 1.808E09-5.3725E08 * COS(beta1) ))))

  sza_hima = ASIN( MAX(-1.0, MIN(1.0, &
         42164.0 * SIN(beta2)/ SQRT( 1.808E09-5.3725E08 * COS(beta2) ))))

! Sensor azimuth angle in degrees from north, pixel to sensor, 
! positive values are clockwise from north.
! (-180 to 180)

! Should be checked for limits:
! arcsine input limit of [-1,1]
  azi_fy2e = SIN(lon1) / SIN(beta1)
  azi_fy2e = MIN( 1.0, MAX(-1.0, azi_fy2e) ) ! arcsine input limit of [-1,1]
  azi_fy2e = ASIN(azi_fy2e)
  azi_fy2e = azi_fy2e / deg2rad              ! conversion from radian to degree

  azi_hima = SIN(lon2) / SIN(beta2)
  azi_hima = MIN( 1.0, MAX(-1.0, azi_hima) ) ! arcsine input limit of [-1,1]
  azi_hima = ASIN(azi_hima)
  azi_hima = azi_hima / deg2rad              ! conversion from radian to degree


  IF (lat1 < 0.0) THEN               ! southern hemisphere case
    azi_fy2e = 180.0 - azi_fy2e
  ENDIF

  IF (lat2 < 0.0) THEN               ! southern hemisphere case
    azi_hima = 180.0 - azi_hima
  ENDIF

  IF (azi_fy2e < 0.0) THEN
    azi_fy2e = azi_fy2e + 360.0 ! from 0 to 360
  ENDIF

  IF (azi_hima < 0.0) THEN
    azi_hima = azi_hima + 360.0 ! from 0 to 360
  ENDIF

  azi_fy2e = azi_fy2e - 180.0 ! Modis conversion (-180 to 180)
  azi_hima = azi_hima - 180.0 ! Modis conversion (-180 to 180)


END SUBROUTINE NMSC_Calc_Angle


! ----------------------------------------------------------------------------
! Calculating Cloud Top Height (CTH)
! -> Applying Cosine 2nd law and Calculating CTHs.
! -> a^2 = b^2 + c^2 - 2bcCOSA
! ----------------------------------------------------------------------------
SUBROUTINE NMSC_Calc_Cths(dist, sza_hima, sza_fy2e, azi_hima, azi_fy2e, cths)


IMPLICIT NONE

REAL, INTENT(IN)  :: dist
REAL, INTENT(IN)  :: sza_hima
REAL, INTENT(IN)  :: sza_fy2e
REAL, INTENT(IN)  :: azi_hima
REAL, INTENT(IN)  :: azi_fy2e
REAL, INTENT(OUT) :: cths

REAL :: nom, denom, azi


azi = (azi_hima - azi_fy2e) * deg2rad

nom = dist * dist
denom = TAN(sza_hima) * TAN(sza_hima) + &
        TAN(sza_fy2e) * TAN(sza_fy2e) - &
        2.*TAN(sza_hima)*TAN(sza_fy2e)*COS(azi)

cths = SQRT(nom/denom)


END SUBROUTINE NMSC_Calc_Cths

!----------------------------------------------------------------------------
! 6. Post processing (QC)
!----------------------------------------------------------------------------
!SUBROUTINE NMSC_CTH_PostProcessing(min_dist, max_ncc1, max_ncc2, cths)
 SUBROUTINE NMSC_CTH_PostProcessing (max_ncc1, max_ncc2, cths, min_dist)
! SUBROUTINE NMSC_CTH_PostProcessing (max_ncc1, max_ncc2, std_ncc1, std_ncc2, &
!                                     cths, min_dist)

IMPLICIT NONE

! Input variable
REAL, INTENT(IN)    :: max_ncc1
REAL, INTENT(IN)    :: max_ncc2
!REAL, INTENT(IN)    :: std_ncc1
!REAL, INTENT(IN)    :: std_ncc2


! Input/Output variable
REAL, INTENT(INOUT) :: cths
 
REAL, INTENT(IN), OPTIONAL   :: min_dist



IF ( present(min_dist) ) THEN 
   ! 1) Minumum distance qc
   IF (min_dist > mindist_threshold) cths = real_unavail
   ! 2) ncc value qc
   IF (max_ncc1 < ncc_threshold .or. max_ncc2 < ncc_threshold) cths = real_unavail
   ! 3) ncc std qc
!   IF (std_ncc1 < 0.05 .or. std_ncc2 < 0.05) cths = real_unavail
ELSE
   ! 2) ncc value qc
   IF (max_ncc1 < ncc_threshold .or. max_ncc2 < ncc_threshold) cths = real_unavail
   ! 3) ncc std qc
!   IF (std_ncc1 < 0.05 .or. std_ncc2 < 0.05) cths = real_unavail
ENDIF

END SUBROUTINE NMSC_CTH_PostProcessing


!----------------------------------------------------------------------------
! 7. Cloud masking
!----------------------------------------------------------------------------

SUBROUTINE NMSC_Cloud_Masking(fy2e_clc_d, center_line, center_col, CTHs)

IMPLICIT NONE

! Input variable
INTEGER(KIND=fy2e_int_kinds), DIMENSION(:,:), INTENT(IN) :: fy2e_clc_d
INTEGER, INTENT(IN) :: center_line
INTEGER, INTENT(IN) :: center_col

! Output variable
REAL, INTENT(INOUT) :: CTHs

IF ( fy2e_clc_d(center_line, center_col) == -1    .or. &   ! Fill value
     fy2e_clc_d(center_line, center_col) == 0     .or. &   ! Clear ocean
     fy2e_clc_d(center_line, center_col) == 1 ) CTHs = 0.0 ! Clear land

END SUBROUTINE NMSC_Cloud_Masking


END MODULE NMSC_Mod_Calc_parallax
