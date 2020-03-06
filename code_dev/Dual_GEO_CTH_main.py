#!/usr/bin/env python
# coding: utf-8

# ------------------------------------------------------------------------------
# Description:
#
#  The main program for cloud top height retrieval using two simultaneous 
#  GEO observations.
#
# Method:
#
# (1) Reads two GEO digital number.
# (2) Selects cloud target and remap images.
# (3) Image matching based on the NCC method.
# (4) Cloud top height retrieval by the iterative process.
# (5) Post-process cloud top height products.
#
# History:
#
# Version  Date      Comment
# -------  ----      -------
# 1.0      19/02/21  Original version.  Jonghyuk Lee (Yonsei Univ.)
# 1.1      20/03/05  Change to Python.  Jonghyuk Lee (Yonsei Univ.)
# 
#
# References:
#
# (1) Hasler, 1981 
#     Stereographic Observations from Geosynchronous Satellties:
#     An Important New Tool for the Atmospheric Sciences.
# (2) Wylie et al., 1998
#     Cloud-Top Heights from GOES-8 and GOES-9 Stereoscopic Imagery.
# (3) Seiz et al., 2007
#     Multiview Cloud-Top Height and Wind Retrieval with Photogrammetric Method:
#     Application to Meteosat-8 HRV Observations.
# (4) Zaksek et al., 2013
#     Monitoring volcanic ash cloud top height through simultaneous retrieval of
#     optical data from polar orbiting and geostationary satellites.
#
#
# Code Description:
#   Language:      Python 3.8
# 
# End of header ----------------------------------------------------------------                                


# ## 알고리즘 준비
# 1. 필요한 라이브러리 불러오기

import sys
sys.path.append('/sdata_storage/jhlee/JUPYTER/code_dev/module/')
import numpy as np
import read_satellite_data as read


# 2. 입력 변수 정의

himawari8_path = '/storage1/jhlee/NMSC_2018/Himawari_8_AHI/'
himawari8_date1 = '201711030530'
himawari8_date2 = '201711030540'
fy2e_path = '/storage1/jhlee/NMSC_2018/FY_2E_VISSR/'
fy2e_date = '20171103_0530'


# ## 알고리즘 시작
# 
# 1. 위성자료 읽기
# - Level 1B 위성 자료 읽기 (디지털 값)

himawari8 = read.himawari8()
fy2e = read.fy2e()
himawari8_DN1 = himawari8.read_himawari8_L1b(himawari8_path, himawari8_date1)
himawari8_DN2 = himawari8.read_himawari8_L1b(himawari8_path, himawari8_date2)
fy2e_DN = fy2e.read_fy2e_L1b(fy2e_path, fy2e_date)


# - 위성 위/경도 자료 읽기



