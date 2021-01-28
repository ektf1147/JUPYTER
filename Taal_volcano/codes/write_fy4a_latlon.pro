; Write_fy4a_latlon table (line, column) -> (lat, lon)
; by jhlee.
PRO write_fy4a_latlon


; Parameter description (constant)
; COFF =  500m : 10991.5
;      = 1000m : 5495.5
;      = 2000m : 2747.5
;      = 4000m : 1373.5
; CFAC =  500m : 81865099
;      = 1000m : 40932549
;      = 2000m : 20466274
;      = 4000m : 10233137
; LOFF is same as COFF
; LFAC is same as CFAC
        
COFF_arr  = [10991.5, 5495.5, 2747.5, 1373.5]
CFAC_arr  = [81865099., 40932549., 20466274., 10233137.]
LOFF_arr  = [10991.5, 5495.5, 2747.5, 1373.5]
LFAC_arr  = [81865099., 40932549., 20466274., 10233137.]
h     = 42164.		; distance from earth center to satellite
ea    = 6378.137 	; Semi-major axis of the earth
eb    = 6356.7523 	; Semi-minor axis of the earth
lon_d = 104.7 		; longitude of satellite
pi    = 3.1415926535897932384626
        

; Determine x, y dimensions
xsize_arr = [21984L, 10992L, 5496L, 2748L]
ysize_arr = [21984L, 10992L, 5496L, 2748L]


; Determine resolution
PRINT, 'Input resolution (500, 1000, 2000, 4000 m)'
READ, resol
IF (resol eq 500) THEN BEGIN
   COFF = COFF_arr[0]
   CFAC = CFAC_arr[0]
   LOFF = LOFF_arr[0]
   LFAC = LFAC_arr[0]
   xsize = xsize_arr[0]
   ysize = ysize_arr[0]
ENDIF
IF (resol eq 1000) THEN BEGIN
   COFF = COFF_arr[1]
   CFAC = CFAC_arr[1]
   LOFF = LOFF_arr[1]
   LFAC = LFAC_arr[1]
   xsize = xsize_arr[1]
   ysize = ysize_arr[1]
ENDIF
IF (resol eq 2000) THEN BEGIN
   COFF = COFF_arr[2]
   CFAC = CFAC_arr[2]
   LOFF = LOFF_arr[2]
   LFAC = LFAC_arr[2]
   xsize = xsize_arr[2]
   ysize = ysize_arr[2]
ENDIF
IF (resol eq 4000) THEN BEGIN
   COFF = COFF_arr[3]
   CFAC = CFAC_arr[3]
   LOFF = LOFF_arr[3]
   LFAC = LFAC_arr[3]
   xsize = xsize_arr[3]
   ysize = ysize_arr[3]
ENDIF


; Lat/long array
lon_arr = FLTARR(xsize, ysize)
lat_arr = FLTARR(xsize, ysize)


; Output file
dir = '/storage1/jhlee/NMSC_2018/FY_4A_AGRI/'
file_lat = dir + 'Lat_'+STRTRIM(string(FIX(resol)),2)+'m.bin'
file_lon = dir + 'Lon_'+STRTRIM(string(FIX(resol)),2)+'m.bin'

; For each column and line number..
FOR c = 1L, xsize do begin
FOR l = 1L, ysize do begin

; ------------------
; Step 1. Find x, y
; ------------------

x = ( pi * (c-COFF) ) / (180.*2.^(-16)*CFAC) ; pi/180 = !dtor (detree to radian)
y = ( pi * (l-LOFF) ) / (180.*2.^(-16)*LFAC)


; -----------------------------------
; Step 2. Find sd, sn, s1, s2, s3, sxy
; -----------------------------------

; Sd
sd = ( h*COS(x)*COS(y) ) * ( h*COS(x)*COS(y) ) - $
     ( COS(y)*COS(y) + ( (ea*ea)/(eb*eb) )*SIN(y)*SIN(y) ) * $
     ( h*h-ea*ea )
sd = SQRT(sd)

; Sn
sn = ( h * COS(x) * COS(y) -sd ) / $
     ( COS(y)*COS(y) + ( (ea*ea)/(eb*eb) )*SIN(y)*SIN(y) )

; S1 ~ S3
s1 = h - ( sn * COS(x) * COS(y) )
s2 = sn * ( SIN(x) * COS(y) )
s3 = -sn * SIN(y)

; Sxy
sxy = SQRT( ( (s1*s1) + (s2*s2) ) )


; ----------------------
; Step 3. Find lon, lat
; ----------------------

lon = ATAN( s2/s1 )*180./pi + lon_d
lat = ATAN( (ea*ea*s3) / (eb*eb*sxy) ) * 180./pi


; Longitude NaN value -> 300
IF (~FINITE(lon)) THEN lon = 300.


; Latitude NaN value -> 300
IF (~FINITE(lat)) THEN lat = 300. 


; Assign long and lat value to arrays (same as FY-2E format)
lon_arr[c-1, l-1] = lon
lat_arr[c-1, l-1] = lat


ENDFOR
ENDFOR


; ---------------------
; Step 4. Write output
; ---------------------

; Latitude/bin
OPENW, lun, file_lat, /get_lun
WRITEU, lun, lat_arr
FREE_LUN, lun

; Longitude/bin
OPENW, lun, file_lon, /get_lun
WRITEU, lun, lon_arr
FREE_LUN, lun


; lat/lon nc file
fname = dir + 'LatLon_'+STRTRIM(string(FIX(resol)),2)+'m.nc'

ncdfid = NCDF_CREATE(fname, /clobber)
xdimid = NCDF_DIMDEF(ncdfid, 'xsize', xsize)
ydimid = NCDF_DIMDEF(ncdfid, 'ysize', ysize)

latid = NCDF_VARDEF(ncdfid, 'Latitude', [xdimid,ydimid], /float)
lonid = NCDF_VARDEF(ncdfid, 'Longitude', [xdimid,ydimid], /float)

NCDF_CONTROL, ncdfid, /endef

NCDF_VARPUT, ncdfid, latid, lat_arr
NCDF_VARPUT, ncdfid, lonid, lon_arr

NCDF_CLOSE, ncdfid

END
