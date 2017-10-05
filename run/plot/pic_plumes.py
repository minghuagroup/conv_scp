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

#def foldplot():
if __name__ == '__main__':

    plt.figure(1, figsize=(20,10))

    for icase in range(2):
        pin = '/T3/yhy/research/ECP/conv_scp/'
        #pin = '/Users/Oscar/yhy/Research/ECP/conv_scp/'
        if icase == 0:  fin = pin+'run/scmdiag-output_GRE.nc'
        if icase == 1:  fin = pin+'run/scmdiag-output_MZhang.nc'

        lev  = fncread(fin, 'p')[0,:,0]/100.0
        levp = fncread(fin, 'pint')[0,:,0]/100.0

        nplume = 15
        clrs = [( min(255*i/nplume,255)/255.0, 
            min(255-abs(255-2*255*i/nplume),255)/255.0, 
            min(255*(nplume-i)/nplume, 255)/255.0 ) for i in range(nplume)]

        #---------------------------------------------------------------
        plt.subplot(2,5,icase*5 + 1)
        mse = fncread(fin, 'mseint')[0,:,0] / 1000.0
        msesat = fncread(fin, 'msesatint')[0,:,0] / 1000.0
        mseup = fncread(fin, 'mse_up')[0,:,:] / 1000.0

        plt.plot(mse, levp, 'k', lw=2)
        plt.plot(msesat, levp, 'k--', lw=2)
        for i in range(nplume):
            mseup[abs(mseup[:,i]-mse)<1e-5,i] = np.nan
            plt.plot(mseup[:,i], levp, '-', color=clrs[i], lw=1)
        if icase==0: plt.title('a) GRE: MSE $(10^{-3} \ J \ kg^{-1})$', loc='left', fontsize=16)
        if icase==1: plt.title('f) New: MSE $(10^{-3} \ J \ kg^{-1})$', loc='left', fontsize=16)
        plt.axis([335, 360, 140,1000])
        plt.ylabel('pressure (hPa)')
        plt.grid('on')
        plt.gca().invert_yaxis()
        #---------------------------------------------------------------
        plt.subplot(2,5,icase*5 + 2)
        entr = fncread(fin, 'ent_rate')[0,:,:] * 1000.0
        detr = fncread(fin, 'det_rate')[0,:,:] * 1000.0

        for i in range(nplume):
            entr[abs(entr[:,i])<1e-5,i] = np.nan
            detr[abs(detr[:,i])<1e-5,i] = np.nan
            plt.plot(entr[:,i], lev, '-', color=clrs[i], lw=1)
            plt.plot(detr[:,i], lev, '--', color=clrs[i], lw=1)

        if icase==0: plt.title('b) GRE: '+r'$\epsilon,\ \delta \ (10^{-3} s^{-1})$', loc='left', fontsize=16)
        if icase==1: plt.title('g) New: '+r'$\epsilon,\ \delta \ (10^{-3} s^{-1})$', loc='left', fontsize=16)
        plt.axis([0, 4, 140,1000])
        #plt.ylabel('pressure (hPa)')
        plt.grid('on')
        plt.gca().invert_yaxis()
        #---------------------------------------------------------------
        plt.subplot(2,5,icase*5 + 3)
        mflxup = fncread(fin, 'normassflx_up')[0,:,:]
        mflxdn = fncread(fin, 'normassflx_dn')[0,:,:]

        for i in range(nplume):
            mflxup[abs(mflxup[:,i])<1e-5,i] = np.nan
            mflxdn[abs(mflxdn[:,i])<1e-5,i] = np.nan
            plt.plot(mflxup[:,i], levp, '-', color=clrs[i], lw=1)
            plt.plot(mflxdn[:,i], levp, '--', color=clrs[i], lw=1)

        if icase==0: plt.title('c) GRE: '+r'$\hat \eta,\ \breve{\eta}$', loc='left', fontsize=16)
        if icase==1: plt.title('h) New: '+r'$\hat \eta,\ \breve{\eta}$', loc='left', fontsize=16)
        plt.axis([-1, 6, 140,1000])
        #plt.ylabel('pressure (hPa)')
        plt.grid('on')
        plt.gca().invert_yaxis()

        #---------------------------------------------------------------
        plt.subplot(2,5,icase*5 + 4)
        w = fncread(fin, 'w_up')[0,:,:]

        for i in range(nplume):
            w[abs(w[:,i])<1e-5,i] = np.nan
            plt.plot(w[:,i], levp, '-', color=clrs[i], lw=1)
            
        if icase==0: plt.title('d) GRE: w $(m \ s^{-1})$', loc='left', fontsize=16)
        if icase==1: plt.title('i) New: w $(m \ s^{-1})$', loc='left', fontsize=16)
        plt.axis([0, 8, 140,1000])
        #plt.ylabel('pressure (hPa)')
        plt.grid('on')
        plt.gca().invert_yaxis()

        #---------------------------------------------------------------
        plt.subplot(2,5,icase*5 + 5)
        buoy = fncread(fin, 'buoy')[0,:,:] * 100

        for i in range(nplume):
            buoy[abs(buoy[:,i])<1e-5,i] = np.nan
            plt.plot(buoy[:,i], levp, '-', color=clrs[i], lw=1)
            
        if icase==0: plt.title('e) GRE: B $(10^{-2} \ m \ s^{-2})$', loc='left', fontsize=16)
        if icase==1: plt.title('j) New: B $(10^{-2} \ m \ s^{-2})$', loc='left', fontsize=16)

        plt.plot([0,0],[0,1000],'k')
        plt.axis([-6, 6, 140,1000])
        #plt.ylabel('pressure (hPa)')
        plt.grid('on')
        plt.gca().invert_yaxis()


    plt.tight_layout()
    ffig = pin+'run/plot/fig18_plumes.png'; print ffig
    plt.savefig(ffig)
    plt.close(1)



#===============================================================================
def foldplot():
#if __name__ == '__main__':
    
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
            xlim = [-30, 30]
        if i==11: 
            var  = ['qtend','qtendcond','qtendevap','qtendtranup','qtendtrandn']
            fac  = [86400*1000] * 5
            icol = [-1,-1,-1,-1,-1]
            xlim = [-30, 30]


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
    ffig = pin+'run/plot/pic_plumes_fig18.png'; print ffig
    plt.savefig(ffig)
    plt.close(1)
    
    return
    
    

