# remap_satellite_data.py

# for array manimulation
import numpy as np

# for interpolation
from scipy.spatial import cKDTree

# This is the function used for converting lat/lon to x, y, z:
def lon_lat_to_cartesian(lon, lat, R = 1):
    '''
    Calculates lon, lat of a point on a sphere with 
    radius R    
    '''
    
    lon_r = np.radians(lon)
    lat_r = np.radians(lat)
    
    x = R * np.cos(lat_r) * np.cos(lon_r)
    y = R * np.cos(lat_r) * np.sin(lon_r)
    z = R * np.sin(lat_r)
    
    return x, y, z


def idw_cKDTree(lon_s, lat_s, val_s, lon_t, lat_t):
    
    # convert lat/lon to Cartesian grid.
    xs, ys, zs = lon_lat_to_cartesian(lon_s.flatten(), lat_s.flatten())
    xt, yt, zt = lon_lat_to_cartesian(lon_t.flatten(), lat_t.flatten()) 
    
    # create cKDTree object to represent source grid.
    tree = cKDTree(list(zip(xs, ys, zs)))
    
    # Interpolate using inverse distance weighting, using 10 nearest neighbors.
    d, inds = tree.query(list(zip(xt, yt, zt)), k = 10)
    w = 1.0 / d**2
    interp_idw = np.sum(w * val_s.flatten()[inds], axis=1) / np.sum(w, axis = 1)
    interp_idw.shape = lon_t.shape
    
    return interp_idw