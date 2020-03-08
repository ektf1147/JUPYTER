# remap.py
import numpy as np

##########################
# 1. Define function : (Latitude, Longitude)	->	Full disc GEOS image(Line, Column)
##########################
def lincol_from_latlon_geos(satellite, Latitude, Longitude):
    deg2rad = 3.14159265358979 / 180.0
    Latitude = Latitude * deg2rad
    Longitude = Longitude * deg2rad
    
    if(satellite == 'himawari8'):
        COFF = 11000.5
        CFAC = 81865099.
        LOFF = 11000.5
        LFAC = 81865099.
        sub_lon = 140.7 * deg2rad
    else: # fy2e
        COFF = 5.50000000000E+03
        CFAC = 4.09325140000E+07 
        LOFF = 5.50000000000E+03
        LFAC = 4.09325140000E+07 
        sub_lon = 86.5 * deg2rad

    c_lat = np.arctan(0.993305616 * np.tan(Latitude))
    RL = 6356.7523 / np.sqrt( 1.0 - 0.00669438444 * np.cos(c_lat)**2.0 )
    R1 = 42164.0 - RL * np.cos(c_lat) * np.cos(Longitude - sub_lon)
    R2 = -RL * np.cos(c_lat) * np.sin(Longitude - sub_lon)
    R3 = RL * np.sin(c_lat)
    Rn = np.sqrt(R1**2.0 + R2**2.0 + R3**2.0 )

    x = np.arctan(-R2 / R1) / deg2rad
    y = np.arcsin(-R3 / Rn) / deg2rad
    ncol = COFF + (x * 2.0**(-16) * CFAC)
    nlin = LOFF + (y * 2.0**(-16) * LFAC)
        
    return (nlin,ncol)


##########################
# 2. Define function : Cut image_pixel_values/lat/lon array with latitude, longitude from GEOS data array
#
# Input Argument
#  - Array: GEOS full disc image_pixel_values/latitude/longitude Array [array/numpy array]
#  - Resolution: GEOS data's Resolution(km) [float]
#  - Latitude1: Left upper position's latitude of user defined area (degree) [float]
#  - Longitude1: Left upper position's longitude of user defined area (degree) [float] 
#  - Latitude2: Right lower position's latitude of user defined area (degree) [float]
#  - Longitude2: Right lower position's longitude of user defined area (degree) [float]
#
# Latitude1 >= Latitude2
# Longitude1 <= Latitude2
#
# Output: image_pixel_value/latitude/longitude array [numpy array]
##########################
def cut_with_latlon_geos(Array, satellite, Latitude1, Longitude1, Latitude2, Longitude2):
    Array = np.array(Array)
    
    if(satellite == 'himawari8'):
        Index_max = 22000
    else:
        Index_max = 9152

    (Lin1,Col1) = lincol_from_latlon_geos(satellite, Latitude1, Longitude1)
    (Lin2,Col2) = lincol_from_latlon_geos(satellite, Latitude2, Longitude2)
    Col1 = int(np.floor(Col1))
    Lin1 = int(np.floor(Lin1))
    Col2 = int(np.ceil(Col2))
    Lin2 = int(np.ceil(Lin2))
    
    cut = np.zeros((Index_max,Index_max))
    if( (Col1 <= Col2) and (Lin1 <= Lin2) and (0 <= Col1) and 
        (Col2 < Index_max) and (0 <= Lin1) and (Lin2 < Index_max) ):
        cut = Array[Lin1:Lin2,Col1:Col2]
        
    return cut

# ############################
# # 9. image_pixel_values DQF processing
# ############################
# cut_pixel[cut_pixel>49151] = 0 #set error pixel's value to 0


