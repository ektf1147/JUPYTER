#!/usr/bin/env python
# coding: utf-8

# In[3]:


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


# In[2]:


import numpy as np


# In[ ]:




