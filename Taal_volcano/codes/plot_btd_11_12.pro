; To plot brightness temperature difference (T10.8 - T12)
; by jhlee.
PRO plot_btd_11_12

; READ FY-4A channel 12, 13 DNs
sttime = '20200112090000'
edtime = '20200112091459'
dir = '/storage1/jhlee/NMSC_2018/FY_4A_AGRI/'
file = dir + 'FY4A-_AGRI--_N_DISK_1047E_L1-_FDI-_MULT_NOM_'+sttime+'_'+$
       edtime+'_4000M_V0001.HDF' 

fid = H5F_OPEN(file)
dname1 = 'NOMChannel12' 
dname2 = 'NOMChannel13'
did1 = H5D_OPEN(fid, dname1)
did2 = H5D_OPEN(fid, dname2)
dn11 = H5D_READ(did1)
dn12 = H5D_READ(did2)


; Read 4 km lat/lon
lat_file = dir + 'Lat_4000m.bin'
lon_file = dir + 'Lon_4000m.bin'

lat = READ_BINARY(lat_file, data_type=4, data_dims=[2748,2748])
lon = READ_BINARY(lon_file, data_type=4, data_dims=[2748,2748])


; Convert DN to TB
scale_11  = -0.003990
offset_11 = 16.344793
lamda_11  = 10.8

scale_12  = -0.003503
offset_12 = 14.354327
lamda_12  = 12.


rad_11 = scale_11 * dn11 + offset_11
rad_12 = scale_12 * dn12 + offset_12


c1 = 1.191042E08  ; W/m2-sr-um
c2 = 1.4387752E04 ; K/um


tb_11 = c2 / ( lamda_11 * alog( 1.+ c1/(rad_11 * lamda_11^5) ) )
tb_12 = c2 / ( lamda_12 * alog( 1.+ c1/(rad_12 * lamda_12^5) ) )
BTD_11_12 = tb_11 - tb_12


; Plot BTD (11-12) map
latmin = 12 & lonmin = 118  ; Target area (Taal Volcano)
latmax = 21 & lonmax = 125  ; Target area (Taal Volcano)
limit = [latmin, lonmin, latmax, lonmax]

idx = WHERE(lat lt latmin or lat gt latmax or $
            lon lt lonmin or lon gt lonmax, cnt)
IF (cnt ne 0) THEN BEGIN
  btd_11_12[idx] = !values.f_nan
  lat[idx] = !values.f_nan
  lon[idx] = !values.f_nan
ENDIF

maxmin, btd_11_12, /nan
maxmin, lat, /nan
maxmin, lon, /nan

end
csize = 2.7
range = [-5,5]
divisions = range[1]-range[0]
cgset_ttfont, 'times'
cgps_open, '../plots/FY4A_BTD_11_12_'+sttime+'.png', font=1, /encapsulated, xsize=8, ysize=8, charsize=csize

cgloadct, 33;22, /brewer, /reverse
TVLCT, cgcolor('white', /triple), 0
TVLCT, r, g, b, /get
palette = [ [r], [g], [b] ]

cgmap_set, 16.5, 121.5, position=[0.15,0.22,0.85,0.92], /cylindrical, limit=limit
imagejh, BTD_11_12, lat, lon, /current, range=range

cgmap_continents, /coast, /countries, color='black', thick=6, /hires
cgmap_grid, color='black', /box_axes, londel=1, latdel=1, thick=3, linestyle=2

cgcolorbar, range = range, title = 'BTD!d11-12!n (K)', $
            divisions = divisions, position = [0.15, 0.13, 0.85, 0.15]
cgps_close

END
