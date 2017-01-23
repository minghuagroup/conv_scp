
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset as DS
import atmopy.plot as aplt

f = DS("../scmdiag-output.nc","r")

time = f.dimensions['time']
ntime = len(time)
subcol = f.dimensions['subcol']
nsubcol = len(subcol)
nlev = len( f.dimensions['lev'] )

xsubcol = range(nsubcol)

ymax = 18

plt.figure( figsize=(18,12) )

cm_subsection = np.linspace(0., 1., nsubcol)
cm = aplt.read_cmap( 'ncl_default' )
colors = [ cm(x) for x in cm_subsection ]

#for itime in range(ntime-1,ntime):
#for itime in range(ntime-10,ntime):
#for itime in range(35, 40):
#for itime in [97]:
for itime in range(ntime):
    print( ('%02i'%(itime+1) ) )

    z = f.variables['z'][itime,::-1,0]/1000.
    p = f.variables['p'][itime,::-1,0]

    zint = f.variables['zint'][itime,::-1,0]/1000.
    pint = f.variables['zint'][itime,::-1,0]

    fcmse = 1.e-3
    dse = fcmse*f.variables['dse'][itime,::-1,0]
    mse = fcmse*f.variables['mse'][itime,::-1,0]
    msesat = fcmse*f.variables['msesat'][itime,::-1,0]
    mse_closure = fcmse*f.variables['mse_closure'][itime,::-1,0]

    ent_rate = 1000*f.variables['ent_rate'][itime,::-1,:]
    w_up = f.variables['w_up'][itime,::-1,:]
    buoy = f.variables['buoy_mid'][itime,::-1,:]*100

    t = f.variables['t'][itime,::-1,0]
    q = f.variables['q'][itime,::-1,0]
    qsat = f.variables['qsat'][itime,::-1,0]

    t_up = f.variables['t_up'][itime,::-1,0]
    q_up = f.variables['q_up'][itime,::-1,0]
    dse_up = fcmse*f.variables['dse_up'][itime,::-1,:]


    diffdse_up = 0.01*f.variables['diffdse_up'][itime,::-1,0]
    diffq_up = 1000.*f.variables['diffq_up'][itime,::-1,0]

    normassflx = f.variables['normassflx_up'][itime,::-1,0]
    normassflx_mid = f.variables['normassflx_up_mid'][itime,::-1,0]


    cp = 1004
    lat = 2.5e6
    fc = 24*3600

    camttend = fc/cp*f.variables['camstend'][itime,::-1,0]
    camttendcond = fc/cp*f.variables['camstendcond'][itime,::-1,0]
    camttendtranup = fc/cp*f.variables['camstendtranup'][itime,::-1,0]
    camttendtrandn = fc/cp*f.variables['camstendtrandn'][itime,::-1,0]
    camqtend     = lat/cp*fc*f.variables['camqtend'][itime,::-1,0]
    camqtendcond = lat/cp*fc*f.variables['camqtendcond'][itime,::-1,0]
    camqtendtranup = lat/cp*fc*f.variables['camqtendtranup'][itime,::-1,0]
    camqtendtrandn = lat/cp*fc*f.variables['camqtendtrandn'][itime,::-1,0]

    ttend     = fc/cp*f.variables['stend'][itime,::-1,:]
    ttendcond = fc/cp*f.variables['stendcond'][itime,::-1,:]
    ttendtran = fc/cp*f.variables['stendtranup'][itime,::-1,:]
    qtend     = lat/cp*fc*f.variables['qtend'][itime,::-1,:]
    qtendcond = lat/cp*fc*f.variables['qtendcond'][itime,::-1,:]
    qtendtran = lat/cp*fc*f.variables['qtendtranup'][itime,::-1,:]

    ttend_avg = np.mean( ttend, axis=1 )
    qtend_avg = np.mean( qtend, axis=1 )

    #ttendcomp = fc/cp*f.variables['stendcomp'][itime,::-1,0]
    #qtendcomp = lat/cp*fc*f.variables['qtendcomp'][itime,::-1,0]

    #tmp1ttend     = fc/cp*f.variables['tmp1stend'][itime,::-1,0]
    #tmp1qtend     = lat/cp*fc*f.variables['tmp1qtend'][itime,::-1,0]
    #tmp2ttend     = fc/cp*f.variables['tmp2stend'][itime,::-1,0]
    #tmp2qtend     = lat/cp*fc*f.variables['tmp2qtend'][itime,::-1,0]

    #ttendevap     = fc/cp*f.variables['stendevap'][itime,::-1,0]
    #qtendevap     = lat/cp*fc*f.variables['qtendevap'][itime,::-1,0]

    rainrate = 1000*fc*f.variables['rainrate'][itime,::-1,:]
    condrate = 1000*fc*f.variables['condrate'][itime,::-1,:]
    evaprate = 1000*fc*f.variables['evaprate'][itime,::-1,:]

    massflxbase = f.variables['massflxbase'][itime,:]
    dilucape    = f.variables['dilucape'][itime,:]
    massflxbase_cape = f.variables['massflxbase_cape'][itime,0]
    massflxbase_w = f.variables['massflxbase_w'][itime,0]
    massflxbase_mconv = f.variables['massflxbase_mconv'][itime,0]
    qcheck = f.variables['qcheck'][itime,0]

    w_up_init = f.variables['w_up_init'][itime,:]

    trigdp = f.variables['trigdp'][itime,0]

    kuplcl    = nlev - int( f.variables['kuplcl'][itime,0]-1 )
    kuplaunch = nlev - int( f.variables['kuplaunch'][itime,0]-1 )

    mse_up = fcmse*f.variables['mse_up'][itime,::-1,:]

    plt.subplot(2,5,1)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)
    plt.title( "time {:d}".format(itime) )

    for i in range(nsubcol):
        mse_up_tmp = mse_up[:,i]
        levind = (mse_up_tmp > 0)

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format( w_up_init[i] ) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format( w_up_init[i] ) )
        plt.plot( mse_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    plt.plot( mse, z, 'k.-')
    plt.plot( msesat, z, 'k.:')
    plt.xlabel("MSE (10^3J/kg)")
    plt.ylabel("Z")
    plt.xlim(330, 365)


    plt.subplot(2,5,2)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)
    plt.title( 'qcheck:{:8.3f}'.format(qcheck) )

    for i in range(nsubcol):
        ent_rate_tmp = ent_rate[:,i]
        levind = (ent_rate_tmp > 0)
        plt.plot( ent_rate_tmp[levind], z[levind], '.-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("entrainment (10^-3)")
    plt.ylabel("Z")
    plt.xlim(0, 4)


    plt.subplot(2,5,3)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    for i in range(nsubcol):
        w_up_tmp = w_up[:,i]
        levind = (w_up_tmp > 0)
        plt.plot( w_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("w(ms-1)")
    plt.xlim(-1, 18)


    plt.subplot(2,5,4)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    for i in range(nsubcol):
        buoy_tmp = buoy[:,i]
        levind = (buoy_tmp > 0)
        plt.plot( buoy_tmp[levind], z[levind], '.-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("B 10^-2 ms-2")
    plt.xlim(-1, 6)


    plt.subplot(2,5,5)
    plt.bar( xsubcol ,massflxbase )
    plt.ylabel('Base Mass Flux')
    plt.ylim( 0, 0.5 )


    plt.subplot(2,5,6)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.plot( condrate , z, 'b.-', lw=1)
    plt.plot( rainrate , z, 'r.-', lw=1)
    plt.plot( evaprate , z, 'c.-', lw=1)
    plt.xlabel("g/kg/day cond(b) rain(r)")
    plt.xlim(0, 60)


    plt.subplot(2,5,7)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.plot( dse, z, 'k.-')
    for i in range(nsubcol):
        dse_up_tmp = dse_up[:,i]
        levind = (dse_up_tmp > 0)
        plt.plot( dse_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("DSE ^3 J/kg")
    plt.xlim(290, 360)


    plt.subplot(2,5,8)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    for i in range(nsubcol):
        ttendcond_tmp = ttendcond[:,i]
        qtendcond_tmp = qtendcond[:,i]
        plt.plot( ttendcond_tmp, z, '.-', color=colors[i], ms=3.5, mew=1)
        plt.plot( qtendcond_tmp, z, '.--', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("tend K/day")


    plt.subplot(2,5,9)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    for i in range(nsubcol):
        ttend_tmp = ttend[:,i]
        qtend_tmp = qtend[:,i]
        plt.plot( ttend_tmp, z, '.-', color=colors[i], ms=3.5, mew=1)
        plt.plot( qtend_tmp, z, '.--', color=colors[i], ms=3.5, mew=1)
    plt.plot( ttend_avg, z, 'b-' , lw=2)
    plt.plot( qtend_avg, z, 'b--', lw=2)
    plt.xlabel("total tend K/day")
    plt.xlim(-60, 60)


    plt.subplot(2,5,10)
    plt.bar( xsubcol ,dilucape )
    plt.ylabel('DILUCAPE')
    plt.ylim( 0, 2000)

    if (itime==0):
        plt.tight_layout(w_pad=0.1, h_pad=0.1)

    plt.savefig('plot/detail-plume-'+("%03d" % (itime+1) )+'.png', dpi=150, bbox_inches='tight', pad_inches=0)

    plt.clf()


#dilcape = f.variables['dilcape'][:,0]
#pmassflxbase = f.variables['pmassflxbase'][:,0]

#plt.figure( figsize=(10,5) )
#plt.plot( dilcape )
#plt.plot( pmassflxbase )
#plt.savefig('cape.png', dpi=120, bbox_inches='tight', pad_inches=0)



