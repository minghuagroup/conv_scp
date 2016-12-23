
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset as DS
import atmopy.plot as aplt

f = DS("../scmdiag-output.nc","r")

time = f.dimensions['time']
ntime = len(time)
subcol = f.dimensions['subcol']
nsubcol = len(subcol)

nlev = 28

ymax = 18

wmin=0.3
wmax=4.5

#plt.figure( figsize=(16,10) )
plt.figure( figsize=(18,12) )

#for itime in range(ntime-1,ntime):
#for itime in range(ntime-10,ntime):
for itime in range(35, 40):
#for itime in [97]:
    print( ('%02i'%(itime+1) ) )

    z = f.variables['z'][itime,-nlev:,0]/1000.
    p = f.variables['p'][itime,-nlev:,0]

    zint = f.variables['zint'][itime,-nlev:,0]/1000.
    pint = f.variables['zint'][itime,-nlev:,0]

    zf = f.variables['z'][itime,:,0]/1000.
    zintf = f.variables['zint'][itime,:,0]/1000.
    fcmse = 1.e-3
    dse = fcmse*f.variables['dse'][itime,-nlev:,0]
    mse = fcmse*f.variables['mse'][itime,-nlev:,0]
    msesat = fcmse*f.variables['msesat'][itime,-nlev:,0]
    mse_closure = fcmse*f.variables['mse_closure'][itime,-nlev:,0]

    ent_rate = 1000*f.variables['ent_rate'][itime,-nlev:,0]
    det_rate = 1000*f.variables['det_rate'][itime,-nlev:,0]
    w_up = 0.1*f.variables['w_up'][itime,-nlev:,0]
    buoy = f.variables['buoy'][itime,-nlev:,0]*100
    #buoy_closure = f.variables['buoy_closure'][itime,-nlev:,0]*100

    diffq_up = 1000.*f.variables['diffq_up'][itime,-nlev:,0]
    t = f.variables['t'][itime,-nlev:,0]
    q = f.variables['q'][itime,-nlev:,0]
    qsat = f.variables['qsat'][itime,-nlev:,0]

    t_up = f.variables['t_up'][itime,-nlev:,0]
    q_up = f.variables['q_up'][itime,-nlev:,0]
    dse_up = fcmse*f.variables['dse_up'][itime,-nlev:,0]

    diffdse_up = 0.01*f.variables['diffdse_up'][itime,-nlev:,0]

    normassflx = f.variables['normassflx_up'][itime,-nlev:,0]
    normassflx_mid = f.variables['normassflx_up_mid'][itime,-nlev:,0]


    cp = 1004
    lat = 2.5e6
    fc = 24*3600
    camttend = fc/cp*f.variables['camstend'][itime,-nlev:,0]
    camttendcond = fc/cp*f.variables['camstendcond'][itime,-nlev:,0]
    camttendtranup = fc/cp*f.variables['camstendtranup'][itime,-nlev:,0]
    camttendtrandn = fc/cp*f.variables['camstendtrandn'][itime,-nlev:,0]
    camqtend     = lat/cp*fc*f.variables['camqtend'][itime,-nlev:,0]
    camqtendcond = lat/cp*fc*f.variables['camqtendcond'][itime,-nlev:,0]
    camqtendtranup = lat/cp*fc*f.variables['camqtendtranup'][itime,-nlev:,0]
    camqtendtrandn = lat/cp*fc*f.variables['camqtendtrandn'][itime,-nlev:,0]

    ttend     = fc/cp*f.variables['stend'][itime,-nlev:,0]
    ttendcond = fc/cp*f.variables['stendcond'][itime,-nlev:,0]
    ttendtran = fc/cp*f.variables['stendtran'][itime,-nlev:,0]
    qtend     = lat/cp*fc*f.variables['qtend'][itime,-nlev:,0]
    qtendcond = lat/cp*fc*f.variables['qtendcond'][itime,-nlev:,0]
    qtendtran = lat/cp*fc*f.variables['qtendtran'][itime,-nlev:,0]
    compttend     = fc/cp*f.variables['compstend'][itime,-nlev:,0]
    compqtend     = lat/cp*fc*f.variables['compqtend'][itime,-nlev:,0]

    tmp1ttend     = fc/cp*f.variables['tmp1stend'][itime,-nlev:,0]
    tmp1qtend     = lat/cp*fc*f.variables['tmp1qtend'][itime,-nlev:,0]
    tmp2ttend     = fc/cp*f.variables['tmp2stend'][itime,-nlev:,0]
    tmp2qtend     = lat/cp*fc*f.variables['tmp2qtend'][itime,-nlev:,0]

    evapttend     = fc/cp*f.variables['evapstend'][itime,-nlev:,0]
    evapqtend     = lat/cp*fc*f.variables['evapqtend'][itime,-nlev:,0]

    qliq = 1000.*f.variables['qliq'][itime,-nlev:,0]
    rainrate = lat/cp*fc*f.variables['rainrate'][itime,-nlev:,0]
    condrate = lat/cp*fc*f.variables['condrate'][itime,-nlev:,0]
    evaprate = lat/cp*fc*f.variables['evaprate'][itime,-nlev:,0]

    massflxbase = f.variables['massflxbase'][itime,0]
    dilucape    = f.variables['dilucape'][itime,0]
    massflxbase_cape = f.variables['massflxbase_cape'][itime,0]
    massflxbase_w = f.variables['massflxbase_w'][itime,0]
    massflxbase_mconv = f.variables['massflxbase_mconv'][itime,0]
    qcheck = f.variables['qcheck'][itime,0]

    trigdp = f.variables['trigdp'][itime,0]

    nconvlev  = int( f.variables['nconvlev'][itime,0] )
    kuplaunch = int( f.variables['kuplaunch'][itime,0]-1 )
    #kupbase   = int( f.variables['kupbase'][itime,0]-1 )
    kuplcl    = int( f.variables['kuplcl'][itime,0]-1 )

    #print( "{} {} {}".format(kuplaunch, kupbase, kuplcl) )

    mse_up = fcmse*f.variables['mse_up'][itime,-nlev:,0]
    levind = (mse_up > 10)
    #levind[:] = True


    plt.subplot(241)
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    cm_subsection = np.linspace(0., 1., nsubcol)
    cm = aplt.read_cmap( 'ncl_default' )
    colors = [ cm(x) for x in cm_subsection ]

    for i in range(nsubcol):
        mse_up_tmp = fcmse*f.variables['mse_up'][itime,-nlev:,i]
        levind = (mse_up_tmp > 10)

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format(wmin) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format(wmax) )
        plt.plot( mse_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    #plt.plot( mse_up[levind], zint[levind], 'rx-', ms=3.5, mew=1)
    #plt.plot( mse_up, zint, 'rx-', ms=3.5, mew=1)
    plt.plot( mse, z, 'k.-')
    plt.plot( msesat, z, 'k.:')
    #plt.plot( mse_closure, z, 'b', lw=1)
    plt.title("time "+('%02i'%itime)+('%s%8.3f'%(' mfb:',massflxbase) ) )
    plt.xlabel("MSE (10^3J/kg)")
    plt.ylabel("Z")
    plt.xlim(300, 400)
    plt.ylim(0, ymax)

    plt.subplot(242)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    for i in range(nsubcol):
        ent_rate_tmp = 1000*f.variables['ent_rate'][itime,-nlev:,i]
        levind = (ent_rate_tmp > 0)

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format(wmin) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format(wmax) )
        #plt.plot( ent_rate_tmp, zint, 'x-', color=colors[i], ms=3.5, mew=1)
        plt.plot( ent_rate_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    #plt.plot( ent_rate , z, 'r.-')
    #plt.plot( det_rate , z, 'g.-')
    plt.title( ('%s%8.2f'%(' dilucape:', dilucape) ) )
    plt.xlabel("entrainment (10^-3)")
    plt.ylabel("Z")
    plt.xlim(0, 4)
    plt.ylim(0, ymax)

    plt.subplot(243)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    for i in range(nsubcol):
        w_up_tmp = f.variables['w_up'][itime,-nlev:,i]
        levind = (w_up_tmp > 0)

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format(wmin) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format(wmax) )
        plt.plot( w_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    #plt.plot( w_up , zint, 'rx--', ms=3.5, mew=1)
    #plt.plot( normassflx , zint, 'kx-', ms=3.5, mew=1)
    plt.title( ('%s%8.4f'%(' mflx_cape:', massflxbase_cape) ) )
    plt.xlabel("w(ms-1)")
    plt.xlim(-1, 18)

    plt.ylabel("Z")
    plt.ylim(0, ymax)


    plt.subplot(244)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    for i in range(nsubcol):
        buoy_tmp = 100.*f.variables['buoy'][itime,-nlev:,i]

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format(wmin) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format(wmax) )
        plt.plot( buoy_tmp, zint, 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlim(-150, 80)

    #plt.plot( buoy , zint, 'kx-', ms=3.5, mew=1)
    #plt.plot( buoy_closure , z, 'b.-')
    plt.title( ('%s%8.4f'%(' mflx_w:', massflxbase_w) ) )
    plt.xlabel("B (10^-2ms-2)")
    plt.ylabel("Z")
    plt.ylim(0, ymax)


    plt.subplot(245)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    #plt.plot( qcheck , z, 'k.-', lw=1)
    #plt.xlabel("qcheck g/kg")
    #plt.xlim( -1, 1 )

    #plt.plot( diffq_up , z, 'k.-', lw=1)
    #plt.xlabel("diff (q_up-q)*massflx g/kg")

    #plt.plot( diffdse_up , z, 'k', lw=2)
    #plt.xlabel("diff DSE 10^2")
    #plt.xlabel("qliq g/kg")

    #plt.plot( qliq , z, 'g.-', lw=1)
    #plt.plot( condrate , z, 'b.-', lw=1)
    #plt.plot( rainrate , z, 'r.-', lw=1)
    #plt.plot( evaprate , z, 'c.-', lw=1)
    #plt.title( ('%s%8.4f'%(' mflx_mconv:', massflxbase_mconv) ) )
    #plt.xlabel("qliq(g) g/kg cond(b) rain(r)")
    #plt.xlim(0, 30)

    #plt.plot( dse, z, 'k.-')
    #plt.plot( dse_up[levind], zint[levind], 'rx-', ms=3.5, mew=1)
    #plt.xlabel("DSE ^4 J/kg")
    #plt.xlim(300, 400)

    for i in range(nsubcol):
        normassflx_tmp = f.variables['normassflx_up'][itime,-nlev:,i]
        levind = (normassflx_tmp > 0)
        print( normassflx_tmp)

        label=None
        if ( i==0 ):
            label=("w0={:4.1f}".format(wmin) )
        if ( i==(nsubcol-1) ):
            label=("w0={:4.1f}".format(wmax) )
        plt.plot( normassflx_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("normflux")
    plt.xlim(-1, 100)

    plt.title( ('%s%8.4f'%(' mflx_mconv:', massflxbase_mconv) ) )
    plt.ylim(0, ymax)
    plt.ylabel("Z")


    plt.subplot(246)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    #plt.plot( diffq_up , z, 'k.-', lw=1)
    #plt.xlabel("diff (q_up-q)*massflx g/kg")

    plt.plot( q, z, 'k.-', lw=1)
    plt.plot( qsat, z, 'g.-', lw=1)
    plt.plot( q_up[levind], zint[levind], 'rx-', ms=3.5, mew=1)
    plt.title( 'qcheck:{:8.3f} trigdp:{:2.0f}'.format(qcheck, trigdp) )
    plt.xlabel("Q kg/kg")

    plt.ylabel("Z")
    plt.ylim(0, ymax)


    plt.subplot(247)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    plt.plot( ttend, z, 'k.-', lw=2)
    plt.plot( ttendtran, z, 'b.-')
    plt.plot( ttendcond, z, 'r.-')
    plt.plot( qtend, z, 'k.--', lw=2)
    plt.plot( qtendtran, z, 'b.--')
    plt.plot( qtendcond, z, 'r.--')
    plt.plot( compttend, z, 'm.-', lw=2)
    plt.plot( compqtend, z, 'm.--', lw=2)
    plt.plot( evapttend, z, 'g.-', lw=2)
    plt.plot( evapqtend, z, 'g.--', lw=2)

    plt.plot( tmp1ttend, z, 'c.-', lw=2)
    plt.plot( tmp1qtend, z, 'c.--', lw=1)
    plt.plot( tmp2ttend, z, 'y.-', lw=2)
    plt.plot( tmp2qtend, z, 'y.--', lw=1)

    plt.xlabel("Heating (K/day)")
    plt.ylabel("Z")
    plt.xlim(-60, 60)
    plt.ylim(0, ymax)


    plt.subplot(248)
    plt.axvline(x=0, lw=1, color='k')
    plt.axhline(y=zf[kuplaunch], lw=1, color='g')
    plt.axhline(y=zintf[kuplcl], lw=1, color='b')

    plt.plot( camttend, z, 'k.-', lw=2)
    plt.plot( camttendtranup, z, 'b.-')
    plt.plot( camttendtrandn, z, 'g.-')
    plt.plot( camttendcond, z, 'r.-')
    plt.plot( camqtend, z, 'k.--', lw=2)
    plt.plot( camqtendtranup, z, 'b.--')
    plt.plot( camqtendtrandn, z, 'g.--')
    plt.plot( camqtendcond, z, 'r.--')
    plt.xlabel("CAM Heating (K/day)")
    plt.ylabel("Z")
    plt.xlim(-60, 60)
    plt.ylim(0, ymax)

    if (itime==0):
        plt.tight_layout(w_pad=0.1, h_pad=0.1)

    plt.savefig('plot/detail-'+("%03d" % (itime+1) )+'.png', dpi=150, bbox_inches='tight', pad_inches=0)

    plt.clf()


#dilcape = f.variables['dilcape'][:,0]
#pmassflxbase = f.variables['pmassflxbase'][:,0]

#plt.figure( figsize=(10,5) )
#plt.plot( dilcape )
#plt.plot( pmassflxbase )
#plt.savefig('cape.png', dpi=120, bbox_inches='tight', pad_inches=0)


#plt.show()

