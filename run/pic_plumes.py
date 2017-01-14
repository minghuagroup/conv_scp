from netCDF4 import Dataset as netcdf
import numpy as np
import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams.update({'font.size': 14})
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mc
import os

#import sys
#sys.path.append('/disk2/yhy/Work/tests/pymods')
#from mod_std_read import *
#from mod_std_fdiag import *
#from mod_std_fplot import *
#from mod_std_OOP import *

#========================================================================


def fncread(fn,var):
    fid = netcdf(fn, 'r')
    data = fid.variables[var][:]
    fid.close()
    return data


#===============================================================================
if __name__ == '__main__':
    nlev = 40
    ncol = 15

    fin = 'scmdiag-output.nc'
    
    plt.figure(1, figsize=(16,12))
    for i in range(1,5):
        plt.subplot(2,4,i)
        if i==1: 
            var  = ['mse', 'msesat', 'mse_up']
            fac  = [1/1000.0, 1/1000.0, 1/1000.0]
            icol = [0, 0, -1]
        if i==2: 
            var  = ['mse', 'msesat', 'mse_up']
            fac  = [1/1000.0, 1/1000.0, 1/1000.0]
            icol = [0, 0, -1]


    plt.tight_layout()
    ffig = './pic_scmdiag-output.png'; print ffig
    plt.savefig(ffig)
    plt.close(1)

