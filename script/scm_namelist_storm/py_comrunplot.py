import os
from datetime import datetime, timedelta


# Available casestr: ['TOGA', 'KWAJEX']
casestr = 'TOGA'
runcase = 'ECP_TOGA_cosz5_c02'
nrundays = 8
nrun = 1
dtime=1200
flagcompile     = 1
flagrun         = 1
flagpic2d       = 1
flagpic3d       = 1
flagpicdiag2d   = 1
flagpicscmdiag  = 1; timescmdiag = datetime(1992,12,19,hour=14,minute=0,second=0)
flagpicmse      = 1; timemse = [datetime(1992,12,19,hour=14), datetime(1992,12,19,hour=16)]

pscm  = '/T3/yhy/models/cesm1_2_1/scm/'
pcase   = pscm+'run/'+runcase+'/'
pfigure = pscm+'run/'+runcase+'/figures/'

# ============================================================================================

if casestr == 'TOGA':
    dm0 = datetime(1992, 12, 19, hour=0, minute=0, second=0)
    fo = '/T3/yhy/models/cesm1_2_1/scm/IOP/TOGA/TOGA_COARE_6hr_scm.nc'
    fcrm = '/T3/yhy/models/crm/TOGA/TOGA_LONG_256x256x64_1km_10s.nc'
    tcrm0 = datetime(1992,11,1, hour=6)
    dtimecrm = 3600
    scmlat = -2.101151
    scmlon = 154.6875

if casestr == 'KWAJEX':
    dm0 = datetime(1999, 7, 25, hour=0, minute=0, second=0)
    fo = '/T3/yhy/models/cesm1_2_1/scm/IOP/KWAJEX/KWAJEX_6hr_scm.nc'
    fcrm = '/T3/yhy/models/crm/KWAJEX/KWAJEX_256x256x64_1km_10s.nc'
    tcrm0 = datetime(1999,7,25,hour=0)
    dtimecrm =3600
    scmlat = 8.6
    scmlon = 167.4

start_ymd  = format(dm0.year,'04') + format(dm0.month,'02') + format(dm0.day,'02')

#=============================================================================

nstep = nrundays *24*3600//dtime-1
tm = dm0
os.system('mkdir '+pcase)
os.system('mkdir '+pfigure)

if flagcompile>0:
    os.system('cd '+pscm+'bld/;  make -j 12; cd '+pscm+'run/')

if flagrun > 0:
    for irun in range(nrun):
        dstr = format(tm.year,'04') + format(tm.month,'02') + format(tm.day,'02')
        tstr = format(tm.hour*3600+tm.minute*60+tm.second)

        tend = tm + timedelta(days=0, seconds=nstep*dtime)
        print 'run: ',casestr, ' --- ', runcase, ' from ', tm.isoformat(sep='-'), ' to ', tend.isoformat(sep='-')

        infile = open('./drv_in_'+casestr, 'r')
        outfile = open('./drv_in', 'w')
        for line in infile:
            tmp = line.strip()
            newline = line
            if 'stop_n' in tmp: 
                newline = ' stop_n = ' + format(nstep) + '\n'
            if 'stop_option' in tmp:
                newline = " stop_option = \'nsteps\' \n" 
            if 'scmlat' in tmp:
                newline = ' scmlat = ' + format(scmlat) + '\n'
            if 'scmlon' in tmp:
                newline = ' scmlon = ' + format(scmlon) + '\n'
            if 'start_ymd' in tmp:
                newline = ' start_ymd = ' + start_ymd + '\n'
            outfile.write(newline) 
        outfile.close()        
        os.system('./scam >& run.log.'+dstr+'-'+tstr)

        tm = tm + timedelta(days=0, seconds=nstep*dtime)

    os.system('cp -fv ./atm_in  ./'+runcase)
    os.system('mv -fv ./camrun.cam.h*.nc  ./'+runcase)
    os.system('mv -fv ./scmdiag-output.nc ./'+runcase)
    os.system('rm -fv ./camrun.*.nc ./camrun.*.bin rpointer.*')

#========================================================================

from netCDF4 import Dataset as netcdf
from datetime import datetime, timedelta
import numpy as np
import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams.update({'font.size': 14})
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mc
import os

fid = netcdf(fcrm, 'r')
tmp  = fid.variables['time'][:]
tcrm = [tcrm0 + timedelta(seconds=i*dtimecrm) for i in range(len(tmp))]
fid.close()

def fncread(fn,var):
    fid = netcdf(fn, 'r')
    data = fid.variables[var][:]
    fid.close()
    return data

def fpictime(time, t0=datetime(1969,1,1)):
    pictime = [(t-t0).days+(t-t0).seconds/86400.0 for t in time]
    return np.array(pictime)

def ftimeaxis(dhour=24, t0=dm0, t1=dm0+timedelta(seconds=nrun*nstep*dtime)):
    xpos  = []
    xstr  = []
    tx    = t0
    x0    = fpictime([t0])[0]
    nt    = ((t1 - t0).days * 24 + (t1 - t0).seconds//3600)//dhour
    for i in range(nt):
        xpos.append(x0 + (i*dhour)/24.0)
        if tx.hour==0:
            xstr.append(format(tx.day,'02')+'/'+format(tx.month,'02') )
        else:
            xstr.append(format(tx.hour,'02'))

        tx = tx + timedelta(days=0, seconds=dhour*3600)
        plt.xticks(xpos, xstr, fontsize=12)
    return

bdate = fncread(fo,'bdate');  tsec = fncread(fo, 'tsec')
tobs = [datetime(bdate//10000, (bdate%10000)//100, bdate%100) + \
    timedelta(days=0, seconds=int(i)) for i in tsec]
print 'OBS: ', tobs[0], tobs[-1]
pictobs = fpictime(tobs)

#=============================================================================

if flagpic2d>0:
    case = runcase; print 'plot 2d: ', case

    crmvars = ['CAPE','CAPEOBS']
    crmdata = {}
    fid = netcdf(fcrm, 'r')
    for ivar in range(len(crmvars)):
        crmdata[crmvars[ivar]] = fid.variables[crmvars[ivar]][:]

    print "CRM: ", tcrm[0], tcrm[-1]
    fid.close()
    pictcrm = fpictime(tcrm)

    npic = 3
    #plt.figure(1, figsize=(max(12,nstep*nrun*dtime//86400//2), 4*npic))
    plt.figure(1, figsize=(max(12,nstep*nrun*dtime//86400//2), 4*npic))

    for ipic in range(npic):
        plt.subplot(npic,1,ipic+1)

        dm = dm0
        
        if ipic==0:
            varstr = 'precip'
            vars = ['PRECT', 'PRECL', 'PRECSH', 'PRECDP', 'NNPREC', 'Prec']
            facs = [24*3600*1000]*6
            clrs = ['k-', 'g--', 'b--', 'm--', 'c-', 'r-']
            flags = ['model']*5 + ['obs']
            bars  = [0]*5 + [1]
            ylim = [-10, 60]
        if ipic==1:
            varstr = 'cape'
            vars = ['CAPE','CAPEOBS', 'DILUCAPE']
            facs = [1.0, 1, 1]
            clrs = ['k-', 'k--', 'r-']
            flags = ['CRM', 'CRM', 'model']
            bars  = [1, 1, 1]
            ylim = [-100, 4000]
        if ipic==2:
            varstr = 'cloud base mass flux'
            vars = ['MBCONVDP', 'MBCONVDP_P']
            facs = [1, 1]
            clrs = ['k-', 'r--']
            flags = ['model']*2
            bars  = [0]*2
            ylim = [-0.001, 0.02]

        for irun in range(1,nrun+1):

            fm = pscm+'run/'+case+'/camrun.cam.h0.'+ \
                format(dm.year,'04')+'-'+format(dm.month,'02')+'-'+format(dm.day,'02')+'-'+ \
                format(dm.hour*3600+dm.minute*60+dm.second, '05')+'.nc'

            for ivar in range(len(vars)):
                if flags[ivar] == 'model':
                    print fm, ' ', vars[ivar]
                    data = fncread(fm, vars[ivar])
                    if data[:].shape[1] == 1:
                        pic = data[:,0,0] + 0
                        ntime = len(pic)
                        pictime = fpictime([dm+timedelta(seconds=dtime*i) for i in range(ntime)])
                        plt.plot(pictime, pic*facs[ivar], clrs[ivar])
                    else:
                        nplume = data.shape[1]
                        for iplume in range(nplume):
                            pic = data[:,iplume,0,0]
                            pictime = fpictime([dm+timedelta(seconds=dtime*i) for i in range(len(pic))])
                            plt.plot(pictime, pic*facs[ivar])

                if flags[ivar] == 'obs':
                    print fo, ' ', vars[ivar]
                    data  = fncread(fo, vars[ivar])[:,0,0]                    
                    # plt.plot(pictobs, data*facs[ivar], clrs[ivar])
                    plt.bar(pictobs, data*facs[ivar], width=pictobs[1]-pictobs[0], 
                        color='r', alpha=0.3, edgecolor='none', linewidth=0)                
                if flags[ivar] == 'CRM':
                    print  'CRM: ', vars[ivar]
                    plt.plot(pictcrm, crmdata[vars[ivar]]*facs[ivar], clrs[ivar])

            dm = dm + timedelta(days=0, seconds=dtime*nstep)
            
        #-------------
        plt.grid(True)
        plt.legend([flags[i]+': '+vars[i] for i in range(len(vars))], fontsize=10, loc=1)
        plt.title(case+': '+varstr)
        ftimeaxis(dhour=2)
        plt.axis(list(fpictime([ dm0, dm0+timedelta(seconds=nrun*nstep*dtime)])) + [ylim[0], ylim[1]])

    plt.tight_layout()
    ffig = pfigure+case+'_2d.png' 
    print ffig
    plt.savefig(ffig)
    plt.close(1)


#==============================================================================
def freadcrm():

    crmvars = ['MCUP','Q1C','Q2']

    crmdata = {}
    fid = netcdf(fcrm, 'r')
    crmdata['lev']  = fid.variables['p'][:] * 100
    for ivar in range(len(crmvars)):
        crmdata[crmvars[ivar]] = fid.variables[crmvars[ivar]][:]

    print "CRM: ", tcrm[0], tcrm[-1]
    fid.close()
    
    return crmdata


#==============================================================================
if flagpic3d>0:

    crmdata = freadcrm()   

    case = runcase; print 'plot 3d: ', case
    
    vars = ['TDIFF', 'QDIFF', 'STENDCONVDP', 'NNSTEND', 'Q1', 'QTENDCONVDP', 'NNQTEND', 'Q2', 'MFCONVDP', 'MCUP']
    flags = ['SCM'] * 4 + ['obs', 'SCM', 'SCM', 'obs'] + ['SCM', 'CRM'] 

    facs = [1, 1000, 86400/1004.0, 86400/1004.0, 86400, 1000*86400, 1000*86400, -86400.0*1004/2.5e6*1000, 100, 100]
    dmeans = [0]*10
    clevs = [np.linspace(-4,4,30)] + [np.linspace(-4,4,30)] + \
        [np.linspace(-20,20,31)] * 2 + [np.linspace(-20,20,31)] + \
        [np.linspace(-8,8,30)]*2 + [np.linspace(-8,8,30)] + \
        [np.linspace(0,4,30)] * 2

    
    plt.figure(1, figsize=(max(14,nstep*nrun*dtime//86400//2), 3.5*len(vars)))
    ncol = 7

    for ivar in range(len(vars)):
        plt.subplot2grid((len(vars), ncol), (ivar, 0), colspan=ncol-1, rowspan=1)

        if flags[ivar] == 'CRM':
            pictime = fpictime(tcrm)
            lev = crmdata['lev']
            data = crmdata[vars[ivar]]
            
            print pictime.shape, lev.shape, data.shape
            plt.contourf(pictime, lev/100.0, np.transpose(data)*facs[ivar], 
                            clevs[ivar], norm = mc.BoundaryNorm(clevs[ivar], 256), cmap = cm.jet)
            plt.contour(pictime, lev/100.0, np.transpose(data)*facs[ivar], [0], colors='k')

            datamean = np.mean(data, axis=0) * facs[ivar]


        if flags[ivar] == 'SCM':
            dm = dm0
            for irun in range(nrun):
                fm = pscm+'run/'+case+'/camrun.cam.h0.'+ \
                    format(dm.year,'04')+'-'+format(dm.month,'02')+'-'+format(dm.day,'02')+'-'+ \
                    format(dm.hour*3600+dm.minute*60+dm.second, '05')+'.nc'
                data = fncread(fm, vars[ivar])[:,:,0,0] + 0

                time = [dm + timedelta(seconds=i*dtime)  for i in range(nstep+1)]
                pictime = fpictime(time)

                ps = fncread(fm, 'PS')[:,0,0]
                hyam = fncread(fm, 'hyam')
                hybm = fncread(fm, 'hybm')
                hyai = fncread(fm, 'hyai')
                hybi = fncread(fm, 'hybi')
                lev  = np.transpose(data) * 0
                if vars[ivar] in ['umf_Cu']:
                    for ilev in range(len(hyai)):
                        lev[ilev,:] = hyai[ilev]*1.e5 + hybi[ilev]*ps
                    ptime, tmp = np.meshgrid(pictime, hyai)
                else:
                    for ilev in range(len(hyam)):
                        lev[ilev,:] = hyam[ilev]*1.e5 + hybm[ilev]*ps
                    ptime, tmp = np.meshgrid(pictime, hyam)
                
                plt.contourf(ptime, lev/100.0, np.transpose(data)*facs[ivar], 
                                clevs[ivar], norm = mc.BoundaryNorm(clevs[ivar], 256), cmap = cm.jet)
                plt.contour(ptime, lev/100.0, np.transpose(data)*facs[ivar], [0], colors='k')

                if irun==0:
                    datamean = np.mean(data, axis=0) * facs[ivar]
                else:
                    datamean += np.mean(data, axis=0) * facs[ivar]
                    
                dm = dm +  timedelta(days=0, seconds=nstep*dtime)

            datamean /= nrun

        if flags[ivar] == 'obs':
            data = fncread(fo, vars[ivar])[:,:,0,0] + 0
            lev = fncread(fo, 'lev')
            ptime = np.array(pictobs) + 0
            print len(ptime), len(lev), data.shape

            plt.contourf(ptime, lev/100.0, np.transpose(data)*facs[ivar], 
                            clevs[ivar], norm = mc.BoundaryNorm(clevs[ivar], 256), cmap = cm.jet)
            plt.contour(ptime, lev/100.0, np.transpose(data)*facs[ivar], [0], colors='k')
            
            datamean = np.mean(data, axis=0) * facs[ivar]

        ftimeaxis()
        plt.axis(list(fpictime([dm0, dm0+timedelta(seconds=nrun*nstep*dtime)]))  + [1,1000])
        plt.gca().invert_yaxis()

        plt.subplot2grid((len(vars), ncol), (ivar, ncol-1), colspan=1, rowspan=1)
        plt.plot(datamean, lev/100.0,'r-o')
        plt.plot(lev*0, lev/100, 'k-', lw=0.3)
        plt.axis([min(datamean), max(datamean), 1, 1000])
        plt.gca().invert_yaxis()



    plt.tight_layout()
    ffig = pfigure+case+'_3d.png' 
    print ffig
    plt.savefig(ffig)
    plt.close(1)


#===========================================================================================
if flagpicdiag2d > 0:
    vars = ['massflxbase', 'prec', 'dilucape']
    pfig = pfigure
    os.system('mkdir '+pfig)
    fid = netcdf(pscm+'/run/'+runcase+'/scmdiag-output.nc', 'r')
    nplume = fid.variables['p'][:].shape[-1]
    ntime  = fid.variables['p'][:].shape[0]
    pictime = [dm0 + timedelta(seconds = dtime*itime) for itime in range(ntime)]

    for var in vars:
        ffig = pfig+'/'+var+'.png'
        print(ffig)
        plt.figure(1, figsize=(20, 4))
        for k in range(nplume):
            plt.plot(fpictime(pictime), fid.variables[var][:,k])
        
        ftimeaxis(dhour=6)
        plt.grid(True)
        plt.title(var)
        plt.tight_layout()
        plt.savefig(ffig)
        plt.close(1)

#===========================================================================================
if flagpicmse > 0:
    pfig = pfigure
    os.system('mkdir '+pfig)
    dt0 = timemse[0] - dm0
    itime0 = int((dt0.days*86400 + dt0.seconds)/dtime) 
    dt1 = timemse[-1] - dm0
    itime1 = int((dt1.days*86400 + dt1.seconds)/dtime) 
    print('picmse: itime = ', itime0, itime1)

    fid = netcdf(pscm+'/run/'+runcase+'/scmdiag-output.nc', 'r')
    nlev  = fid.variables['p'][:].shape[1]
    nplume = fid.variables['p'][:].shape[-1]

    for itime in range(itime0, itime1+1):
        pmid = fid.variables['p'][itime, :, 0] / 100
        pint = fid.variables['pint'][itime, :, 0] / 100
        pictime = dm0 + timedelta(seconds = dtime*itime)
        ffig = pfig+'/mse_'+pictime.isoformat(sep='-')+'.png'
        print(ffig)
        plt.plot(fid.variables['mse'][itime,:,0]/1004, pmid, 'k-', lw=2)
        plt.plot(fid.variables['msesat'][itime,:,0]/1004, pmid, 'k--', lw=2)
   
        for k in range(nplume):
            plt.plot(fid.variables['mse_up'][itime,:,k]/1004, pint)
            
        plt.xlim([320, 370])
        plt.ylim([0,1000])
        plt.gca().invert_yaxis()
        plt.title(pictime.isoformat(sep='-'))
        plt.tight_layout()
        plt.savefig(ffig)
        plt.close(1)

        
#===========================================================================================
if flagpicscmdiag > 0:
    picvars = ['ent_rate', 'det_rate', 'ent_rate_dp', 'det_rate_dp', \
            'ent_rate_sh', 'det_rate_sh', 'weight', \
            'ent_rate_org', 'ent_rate_turb', 'det_rate_org', 'det_rate_turb', \
            'bs_xc', 'diffdse_up', 'diffq_up', 'diffdse_dn', 'diffq_dn', 'qcheck', \
            'w_up_mid', 'buoy_mid', 'mse_up_mid', 't_up_mid', 'q_up_mid', 'normassflx_up_mid', \
            'w_up_init', 'w_up', 'buoy', 'radius_up', 'dse_up', 'mse_up', 't_up', 'q_up', \
            'qliq_up', 'qice_up', 'normassflx_up', 'mse_up_plume', 'mse_dn', 'normassflx_dn', \
            'stend', 'qtend', 'stendcond', 'qtendcond', 'stendtranup', 'qtendtranup', \
            'stendtrandn', 'qtendtrandn', 'dilucape', 'mseqi', 'condrate', 'rainrate', 'snowrate', \
            'precrate', 'accuprec', 'evaprate', 'massflxbase', 'massflx', 'prec', 'trigdp']

    pfig = pfigure+timescmdiag.isoformat(sep='-')+'/'
    os.system('mkdir '+pfig)
    dt = timescmdiag - dm0
    itime = int((dt.days*86400 + dt.seconds)/dtime) 
    print('picscmdiag: itime =', itime)

    fid = netcdf(pscm+'/run/'+runcase+'/scmdiag-output.nc', 'r')
    pmid = fid.variables['p'][itime, :, 0] / 100
    pint = fid.variables['pint'][itime, :, 0] / 100
    nlev = len(pmid)
    nlevp = len(pint)
    
    for var in picvars:
        ffig = pfig+var+'.png'
        print('plot scmdiag: '+ffig)
        plt.figure(1, figsize=(4,3))
        plt.subplot(1,1,1)
        data = fid.variables[var][itime]
        ndim = len(data.shape)
        if ndim == 1:
            plt.plot(data, 'ro-')
        if ndim == 2:
            nplume = data.shape[1]
            if data.shape[0] == nlev:
                plev = pmid
            else:
                plev = pint
            #plt.contourf(range(nplume), plev, data, 20, cmap='rainbow')
            #plt.colorbar()
            for iplume in range(nplume):
                plt.plot(data[:,iplume], plev)
            plt.legend(list(range(nplume)), loc='best', fontsize=8, framealpha=0.5)
            plt.gca().invert_yaxis()

        plt.title(var)
        plt.tight_layout()
        plt.savefig(ffig)
        plt.close(1)

    fid.close()



