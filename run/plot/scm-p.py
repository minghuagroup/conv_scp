
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

plt.figure( figsize=(22,12) )

cm_subsection = np.linspace(0., 1., nsubcol)
cm = aplt.read_cmap( 'ncl_default' )
colors = [ cm(x) for x in cm_subsection ]

#for itime in range(ntime-1,ntime):
#for itime in range(ntime-10,ntime):
for itime in range(34, 40):
#for itime in [97]:
#for itime in range(ntime):
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
    q_up = f.variables['q_up'][itime,::-1,:]
    dse_up = fcmse*f.variables['dse_up'][itime,::-1,:]


    diffdse_up = 0.01*f.variables['diffdse_up'][itime,::-1,:]
    diffq_up = 1000.*f.variables['diffq_up'][itime,::-1,:]

    normassflx = f.variables['normassflx_up'][itime,::-1,:]
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

    qcheck = f.variables['qcheck'][itime,0]

    ttend     = qcheck*fc/cp*f.variables['stend'][itime,::-1,:]
    ttendcond = qcheck*fc/cp*f.variables['stendcond'][itime,::-1,:]
    ttendtran = qcheck*fc/cp*f.variables['stendtranup'][itime,::-1,:]
    ttendtrandn = qcheck*fc/cp*f.variables['stendtrandn'][itime,::-1,:]
    ttendsum  = qcheck*fc/cp*f.variables['stendsum'][itime,::-1,0]

    qtend     = qcheck*lat/cp*fc*f.variables['qtend'][itime,::-1,:]
    qtendcond = qcheck*lat/cp*fc*f.variables['qtendcond'][itime,::-1,:]
    qtendtran = qcheck*lat/cp*fc*f.variables['qtendtranup'][itime,::-1,:]
    qtendtrandn = qcheck*lat/cp*fc*f.variables['qtendtrandn'][itime,::-1,:]
    qtendsum  = qcheck*lat/cp*fc*f.variables['qtendsum'][itime,::-1,0]

    qliqtend_det = qcheck*lat/cp*fc*f.variables['qliqtenddet'][itime,::-1,:]

    #ttendevap     = fc/cp*f.variables['stendevap'][itime,::-1,0]
    #qtendevap     = lat/cp*fc*f.variables['qtendevap'][itime,::-1,0]

    rainrate = qcheck*1000*fc*f.variables['rainrate'][itime,::-1,:]
    condrate = qcheck*1000*fc*f.variables['condrate'][itime,::-1,:]
    evaprate = qcheck*1000*fc*f.variables['evaprate'][itime,::-1,:]
    snowrate = qcheck*1000*fc*f.variables['snowrate'][itime,::-1,:]

    qliq_up = f.variables['qliq_up'][itime,::-1,:]
    qice_up = f.variables['qice_up'][itime,::-1,:]

    massflxbase = f.variables['massflxbase'][itime,:]
    dilucape    = f.variables['dilucape'][itime,:]
    massflxbase_cape = f.variables['massflxbase_cape'][itime,0]
    massflxbase_w = f.variables['massflxbase_w'][itime,0]
    massflxbase_mconv = f.variables['massflxbase_mconv'][itime,0]

    w_up_init = f.variables['w_up_init'][itime,:]

    trigdp = f.variables['trigdp'][itime,0]

    kuplcl    = nlev - int( f.variables['kuplcl'][itime,0]-1 )
    kuplaunch = nlev - int( f.variables['kuplaunch'][itime,0]-1 )

    mse_up = fcmse*f.variables['mse_up'][itime,::-1,:]

    plt.subplot(2,5,1)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)
    plt.title( "time {:d}".format(itime+1) )

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
    plt.title( 'qcheck:{:8.3f} trigdp:{:1.0f}'.format(qcheck, trigdp) )

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

    plt.axvline(x=0, color='grey')
    for i in range(nsubcol):
        w_up_tmp = w_up[:,i]
        levind = (w_up_tmp > 0)
        plt.plot( w_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("w(ms-1)")
    plt.xlim(-1, 18)

    #plt.axvline(x=0, color='grey')
    #for i in range(nsubcol):
        #normassflx_tmp = normassflx[:,i]
        #levind = (normassflx_tmp > 0)
        #plt.plot( normassflx_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    #plt.xlabel("normasssflx")
    #plt.xlim(0, 10)


    plt.subplot(2,5,4)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.axvline(x=0, color='grey')
    for i in range(nsubcol):
        buoy_tmp = buoy[:,i]
        levind = (buoy_tmp > 0)
        plt.plot( buoy_tmp[levind], z[levind], '.-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("B 10^-2 ms-2")
    plt.xlim(-1, 6)


    plt.subplot(2,5,5)
    plt.bar( xsubcol ,massflxbase )
    plt.ylabel('Base Mass Flux')
    plt.ylim( 0, 0.1 )


    plt.subplot(2,5,6)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    #plt.axvline(x=0, color='grey')
    #plt.plot( condrate , z, 'b.-', lw=1)
    #plt.plot( rainrate , z, 'r.-', lw=1)
    #plt.plot( snowrate , z, 'm.-', lw=1)
    #plt.plot( evaprate , z, 'c.-', lw=1)
    #plt.xlabel("g/kg/day cond(b) rain(r)")

    #for i in range(nsubcol):
        #qwat_up_tmp = qliq_up[:,i]+qice_up[:,i]
        #levind = (qwat_up_tmp > 0)
        #plt.plot( qwat_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    #plt.axvline(x=0, color='grey')
    #plt.plot( q, z, 'k.-')
    #for i in range(nsubcol):
        #q_up_tmp = q_up[:,i]
        #levind = (q_up_tmp > 0)
        #plt.plot( q_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)

    plt.plot( dse, z, 'k.-')
    for i in range(nsubcol):
        dse_up_tmp = dse_up[:,i]
        levind = (dse_up_tmp > 0)
        plt.plot( dse_up_tmp[levind], zint[levind], 'x-', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("DSE ^3 J/kg")
    plt.xlim(290, 360)

    #plt.axvline(x=0, color='grey')
    #for i in range(nsubcol):
        #diffq_up_tmp = diffq_up[:,i]
        #plt.plot( diffq_up_tmp, zint, 'x-', color=colors[i], ms=3.5, mew=1)
    #plt.xlabel("diff Q e3")
    #plt.xlim(-10, 10)

    #plt.axvline(x=0, color='grey')
    #for i in range(nsubcol):
        #diffdse_up_tmp = diffdse_up[:,i]
        #plt.plot( diffdse_up_tmp, zint, 'x-', color=colors[i], ms=3.5, mew=1)
    #plt.xlabel("diff DSE e3")


    plt.subplot(2,5,7)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.axvline(x=0, color='grey')
    for i in range(nsubcol):
        ttendtran_tmp = ttendtran[:,i]
        qtendtran_tmp = qtendtran[:,i]
        plt.plot( ttendtran_tmp, z, '.-', color=colors[i], ms=3.5, mew=1)
        plt.plot( qtendtran_tmp, z, '.--', color=colors[i], ms=3.5, mew=1)
    #plt.plot( ttendtrandn, z, 'k.-')
    #plt.plot( qtendtrandn, z, 'g.-')
    plt.xlabel("T/Q TRANS K/day")
    plt.xlim(-20, 20)


    plt.subplot(2,5,8)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.axvline(x=0, color='grey')
    for i in range(nsubcol):
        ttendcond_tmp = ttendcond[:,i]
        qtendcond_tmp = qtendcond[:,i]
        plt.plot( ttendcond_tmp, z, '.-', color=colors[i], ms=3.5, mew=1)
        plt.plot( qtendcond_tmp, z, '.--', color=colors[i], ms=3.5, mew=1)
    plt.xlabel("COND tend K/day")
    plt.xlim(-20, 20)

    #plt.axvline(x=0, color='grey')
    #plt.plot( qliqtend_det, z, 'k.-')
    #plt.xlabel("QLIQTEND_DET tend K/day")
    ##plt.xlim(-20, 20)


    plt.subplot(2,5,9)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.axvline(x=0, color='grey')
    for i in range(nsubcol):
        ttend_tmp = ttend[:,i]
        qtend_tmp = qtend[:,i]
        plt.plot( ttend_tmp, z, '.-', color=colors[i], ms=3.5, mew=1)
        plt.plot( qtend_tmp, z, '.--', color=colors[i], ms=3.5, mew=1)
    plt.plot( ttendsum, z, 'b-' , lw=2)
    plt.plot( qtendsum, z, 'b--', lw=2)
    plt.xlabel("total tend K/day")
    plt.xlim(-60, 60)


    plt.subplot(2,5,10)
    plt.axhline(y=z[kuplaunch], lw=1, color='g')
    plt.axhline(y=zint[kuplcl], lw=1, color='b')
    plt.ylim(0, ymax)

    plt.axvline(x=0, color='grey')
    plt.plot( camttendcond, z, 'r.-' , ms=3.5, mew=1)
    plt.plot( camqtendcond, z, 'r.--', ms=3.5, mew=1)
    plt.plot( camttendtranup, z, 'b.-' , ms=3.5, mew=1)
    plt.plot( camqtendtranup, z, 'b.--', ms=3.5, mew=1)
    plt.plot( camttend, z, 'k.-' , ms=3.5, mew=1)
    plt.plot( camqtend, z, 'k.--', ms=3.5, mew=1)
    plt.xlim(-60, 60)

    #plt.bar( xsubcol ,dilucape )
    #plt.ylabel('DILUCAPE')
    #plt.ylim( 0, 2000)

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



