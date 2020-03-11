# match_satellite_images.py
import numpy as np
from skimage.feature import match_template
import matplotlib.pyplot as plt

def match_templates(array_template, array_source1, array_source2, clc, max_shift):
                        
    y1_array = np.arange(max_shift, array_template.shape[0] - ( 2 * max_shift + 1) - max_shift)
    x1_array = np.arange(max_shift, array_template.shape[1] - ( 2 * max_shift + 1) - max_shift)
         
    for y1 in y1_array:
        for x1 in x1_array:
            if (clc[y1,x1] == -1 or clc[y1,x1] == 0 or clc[y1,x1] == 1): 
                continue
            else:                
                x2 = x1 + (2 * max_shift + 1)
                y2 = y1 + (2 * max_shift + 1)
                template = array_template[y1:y2, x1:x2]
                source1 = array_source1[y1-max_shift:y2+max_shift, x1-max_shift:x2+max_shift]
                source2 = array_source2[y1-max_shift:y2+max_shift, x1-max_shift:x2+max_shift]
                result1 = match_template(source1, template)
                result2 = match_template(source2, template)
                                                
        return result1, result2
                