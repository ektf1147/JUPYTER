#!/usr/bin/env python
# coding: utf-8

import numpy as np
from netCDF4 import Dataset

class himawari8:
    
    def read_himawari8_L1b(self, 
                           himawari8_path,
                           himawari8_date):
    
        # 입력 파일 이름 정의
        fname = himawari8_path + 'himawari8_ahi_le1b_ch03_fd005ge_' + himawari8_date + '.bin'

        # L1B 자료 읽기
        himawari8_DN = np.fromfile(fname, dtype='uint16')

        # 2차원 배열로 변경
        himawari8_DN = himawari8_DN.reshape(22000, 22000)

        return himawari8_DN


class fy2e:
    
    def read_fy2e_L1b(self,
                      fy2e_path,
                      fy2e_date):
        
        # 입력 파일 이름 정의
        fname = fy2e_path + 'FY2E_FDI_ALL_NOM_' + fy2e_date + '.hdf'

        # L1B 자료 읽기
        f = Dataset(fname, 'r')
        fy2e_DN = f.variables['NOMChannelVIS'][:]

        return fy2e_DN
