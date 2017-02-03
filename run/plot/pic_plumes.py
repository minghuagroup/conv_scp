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

    pin = '/T3/yhy/research/ECP/conv_scp/'
    #pin = '/Users/Oscar/yhy/Research/ECP/conv_scp/'
    fin = pin+'run/scmdiag-output.nc'

    lev  = fncread(fin, 'p')[0,:,0]/100.0
    levp = fncread(fin, 'pint')[0,:,0]/100.0

    cstr = ['r','g','b','m','c']    
    plt.figure(1, figsize=(20,15))

    for i in range(1,12):
        plt.subplot(3,4,i)
        if i==1: 
            var  = ['mse', 'mse_up', 'mse_dn', 'msesat']
            fac  = [1/1000.0, 1/1000.0, 1/1000.0, 1/1000.0]
            icol = [0, -1, -1, 0]
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
            xlim = [-1, 18]
        if i==5: 
            var  = ['normassflx_up','normassflx_dn']
            fac  = [1,1]
            icol = [-1,-1]
            xlim = [-1, 10]
        if i==6: 
            var  = ['qliq_up','qice_up']
            fac  = [1.0e3]*2
            icol = [-1,-1,-1]
            xlim = [-1, 10]
        if i==7: 
            var  = ['rainrate','snowrate','precrate']
            fac  = [1e7,1e7,1e7]
            icol = [-1,-1,-1]
            xlim = [-0.1, 4]
        if i==8: 
            var  = ['condrate', 'evaprate']
            fac  = [1e7,1e7]
            icol = [-1,-1]
            xlim = [-0.1, 4]
        if i==9: 
            var  = ['accuprec']
            fac  = [86400]
            icol = [-1,-1]
            xlim = [-0.1, 100]
        if i==10: 
            var  = ['stend','stendcond','stendevap','stendtranup','stendtrandn']
            fac  = np.array([1,1,1,1,1]) /1004.0 * 86400
            icol = [-1,-1,-1,-1,-1]
            xlim = [-120, 120]
        if i==11: 
            var  = ['qtend','qtendcond','qtendevap','qtendtranup','qtendtrandn']
            fac  = [86400*1000] * 5
            icol = [-1,-1,-1,-1,-1]
            xlim = [-60, 60]


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
        if i in range(1,10):
            plt.title(tstr)
        plt.axis([xlim[0], xlim[1], 100,1000])
        plt.grid('on')
        plt.gca().invert_yaxis()


    plt.tight_layout()
    ffig = pin+'run/plot/pic_scmdiag-output.png'; print ffig
    plt.savefig(ffig)
    plt.close(1)

