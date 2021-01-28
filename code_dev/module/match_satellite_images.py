# match_satellite_images.py
import numpy as np
from skimage.feature import match_template

def match_templates(array_template, array_source1, array_source2, x1, y1, max_shift):
                                           
    x2 = x1 + (2 * max_shift + 1)
    y2 = y1 + (2 * max_shift + 1)
    
    template = array_template[y1:y2, x1:x2]
    source1 = array_source1[y1-max_shift:y2+max_shift, x1-max_shift:x2+max_shift]
    source2 = array_source2[y1-max_shift:y2+max_shift, x1-max_shift:x2+max_shift]
                
    result1 = match_template(source1, template)
    result2 = match_template(source2, template)
                
    nlin1, ncol1 = np.unravel_index(np.argmax(result1), result1.shape)
    nlin2, ncol2 = np.unravel_index(np.argmax(result2), result2.shape)
                                                               
    return nlin1, ncol1, nlin2, ncol2
                