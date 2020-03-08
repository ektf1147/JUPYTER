#!/usr/bin/env python
# coding: utf-8

import numpy as np
from netCDF4 import Dataset

class himawari8:
    
    # 필요 변수
    dtype_L1b = np.uint16
    dtype_pos = np.float32
    ncol = 22000
    nlin = 22000
    
    def read_L1b(self, himawari8_path, himawari8_date):
      
        # 입력 파일 이름 정의
        fname = himawari8_path + 'himawari8_ahi_le1b_ch03_fd005ge_' + himawari8_date + '.bin'
        
        # L1B 자료 읽기
        himawari8_DN = np.fromfile(fname, dtype = self.dtype_L1b).reshape(self.ncol, self.nlin)
                
        # 실수형 배열로 변경
        himawari8_DN = himawari8_DN.astype(float)
                
        return himawari8_DN
    
    
    def read_latlon(self, himawari8_path):
                
        # 입력 파일 이름 정의
        fname_lon = himawari8_path + 'Lon_500m.bin'
        fname_lat = himawari8_path + 'Lat_500m.bin'
        
        # 위/경도 자료 읽기
        himawari8_lon = np.fromfile(fname_lon, dtype = self.dtype_pos).reshape(self.ncol, self.nlin)
        himawari8_lat = np.fromfile(fname_lat, dtype = self.dtype_pos).reshape(self.ncol, self.nlin)

        return himawari8_lat, himawari8_lon
        
        
class fy2e:
    
    # 필요 변수
    dtype = np.float32
    ncol = 9152
    nlin = 9152
    lon_fy2e = 86.5
    
    def read_L1b(self, fy2e_path, fy2e_date):
        
        # 입력 파일 이름 정의
        fname = fy2e_path + 'FY2E_FDI_ALL_NOM_' + fy2e_date + '.hdf'

        # L1B 자료 읽기
        f = Dataset(fname, 'r')
        fy2e_DN = f.variables['NOMChannelVIS'][:]
        fy2e_clc = f.variables['NOMCloudClassification'][:] 

        return fy2e_DN, fy2e_clc
    
    
    def read_latlon(self, fy2e_path):
               
        # 입력 파일 이름 정의
        fname = fy2e_path + 'NOM_ITG_9152_9152_0E0N_LE.dat'
        
        # 위/경도 자료 읽기
        fy2e_lonlat = np.fromfile(fname, dtype = np.float32).reshape(self.ncol,self.nlin,2)
               
        fy2e_lon = fy2e_lonlat[:,:,0] + self.lon_fy2e
        fy2e_lat = fy2e_lonlat[:,:,1]

        return fy2e_lat, fy2e_lon