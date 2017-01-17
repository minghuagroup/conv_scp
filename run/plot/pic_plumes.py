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



def fncread(fn,var):
    fid = netcdf(fn, 'r')
    data = fid.variables[var][:]
    fid.close()
    return data


#===============================================================================
if __name__ == '__main__':

    pin = '/disk2/yhy/Research/ECP/conv_scp/'
    fin = pin+'run/scmdiag-output.nc'

    lev  = fncread(fin, 'p')[0,:,0]/100.0
    levp = fncread(fin, 'pint')[0,:,0]/100.0

    cstr = ['r','g','b']    
    plt.figure(1, figsize=(16,15))

    for i in range(1,10):
        plt.subplot(3,4,i)
        if i==1: 
            var  = ['mse', 'msesat', 'mse_up']
            fac  = [1/1000.0, 1/1000.0, 1/1000.0]
            icol = [0, 0, -1]
            xlim = [330, 365]
        if i==2: 
            var  = ['ent_rate']
            fac  = [1000.0]
            icol = [-1]
            xlim = [-1, 4]
        if i==3: 
            var  = ['buoy_mid', 'buoy']
            fac  = [100.0, 100.0]
            icol = [-1, -1]
            xlim = [-1, 6]
        if i==4: 
            var  = ['w_up_mid', 'w_up']
            fac  = [1, 1]
            icol = [-1, -1]
            xlim = [-1, 10]
        if i==5: 
            var  = ['normassflx_up']
            fac  = [1]
            icol = [-1]
            xlim = [-1, 10]
        if i==6: 
            var  = ['qliq_up','qice_up']
            fac  = [1e5,1e5]
            icol = [-1,-1]
            xlim = [-0.1, 1]
        if i==7: 
            var  = ['rainrate','snowrate','precrate']
            fac  = [1e5,1e5,1e5]
            icol = [-1,-1,-1]
            xlim = [-0.1, 1]
        if i==8: 
            var  = ['condrate', 'evaprate']
            fac  = [100000,100000]
            icol = [-1,-1]
            xlim = [-0.1, 1]
        if i==9: 
            var  = ['accuprec']
            fac  = [10]
            icol = [-1,-1]
            xlim = [-0.1, 1]


        tstr = ''
        for ivar in range(len(var)):
            tstr += var[ivar]+', '
            data  = fncread(fin, var[ivar])[0,:,:]
            if data.shape[0] == len(lev):
                piclev = lev + 0
            else:
                piclev = levp + 0
            if icol[ivar] < 0:
                for k in range(data.shape[1]):
                    plt.plot(data[:,k]*fac[ivar], piclev, '-'+cstr[ivar]+'.')
            else:
                plt.plot(data[:,0]*fac[ivar], piclev, '-'+cstr[ivar]+'.')
        plt.title(tstr)
        plt.axis([xlim[0], xlim[1], 100,1000])
        plt.grid('on')
        plt.gca().invert_yaxis()


    plt.tight_layout()
    ffig = pin+'run/plot/pic_scmdiag-output.png'; print ffig
    plt.savefig(ffig)
    plt.close(1)

