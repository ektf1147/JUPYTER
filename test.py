import numpy as np
import matplotlib.pyplot as plt
from netCDF import Dataset

# Read GK-2A dataset
file = 
nc = Dataset(file)
DNs = nc.variables('image_pixel_values')
