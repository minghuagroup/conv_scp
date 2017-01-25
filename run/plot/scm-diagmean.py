
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset as DS

f = DS("../scmdiag-output.nc","r")

time = f.dimensions['time']
ntime = len(time)

plt.figure( figsize=(10,5) )

cp = 1004
lat = 2.5e6
fc = 3600*24
camttend     = np.mean(fc/cp*f.variables['camstend'][:,::-1,0], axis=0)
camttendcond = np.mean(fc/cp*f.variables['camstendcond'][:,::-1,0], axis=0)
camttendtranup = np.mean(fc/cp*f.variables['camstendtranup'][:,::-1,0], axis=0)
camttendtrandn = np.mean(fc/cp*f.variables['camstendtrandn'][:,::-1,0], axis=0)

camqtend     = np.mean(lat/cp*fc*f.variables['camqtend'][:,::-1,0], axis=0)
camqtendcond = np.mean(lat/cp*fc*f.variables['camqtendcond'][:,::-1,0], axis=0)
camqtendtranup = np.mean(lat/cp*fc*f.variables['camqtendtranup'][:,::-1,0], axis=0)
camqtendtrandn = np.mean(lat/cp*fc*f.variables['camqtendtrandn'][:,::-1,0], axis=0)

qcheck    = f.variables['qcheck'][:,0]

ttend_all = qcheck*fc/cp*f.variables['stend'][:,::-1,0].T

ttend     = np.mean(qcheck*fc/cp*f.variables['stend'][:,::-1,0].T, axis=1)
ttendcond = np.mean(qcheck*fc/cp*f.variables['stendcond'][:,::-1,0].T, axis=1)
ttendtran = np.mean(fc/cp*f.variables['stendtranup'][:,::-1,0].T, axis=1)

qtend_all = qcheck*lat/cp*fc*f.variables['qtend'][:,::-1,0].T

qtend     = np.mean(qcheck*lat/cp*fc*f.variables['qtend'][:,::-1,0].T, axis=1)
qtendcond = np.mean(qcheck*lat/cp*fc*f.variables['qtendcond'][:,::-1,0].T, axis=1)
qtendtran = np.mean(qcheck*lat/cp*fc*f.variables['qtendtranup'][:,::-1,0].T, axis=1)

ttendevap = np.mean(qcheck*fc/cp*f.variables['stendevap'][:,::-1,0].T, axis=1)
qtendevap = np.mean(qcheck*lat/cp*fc*f.variables['qtendevap'][:,::-1,0].T, axis=1)

#ttendcomp = qcheck*np.mean(fc/cp*f.variables['stendcomp'][:,::-1,0], axis=0)
#qtendcomp = qcheck*np.mean(lat/cp*fc*f.variables['qtendcomp'][:,::-1,0], axis=0)


z = np.mean(f.variables['z'][:,::-1,0]/1000., axis=0)

#xmin = -10
#xmax =  10
xmin = -5
xmax =  5

ymin = 0
ymax = 18

plt.subplot(121)
plt.ylabel("Z")
plt.ylim( ymin, ymax )

plt.plot( ttend_all, z, 'b.-', alpha=0.1, lw=2)
plt.plot( qtend_all, z, 'g.--', alpha=0.1, lw=2)

plt.plot( ttend, z, 'b.-', lw=2)
#plt.plot( ttendtran, z, 'b.-')
#plt.plot( ttendcond, z, 'r.-')
plt.plot( qtend, z, 'g.--', lw=2)
#plt.plot( qtendtran, z, 'b.--')
#plt.plot( qtendcond, z, 'r.--')

#plt.plot( ttendcomp, z, 'm.-', lw=2)
#plt.plot( qtendcomp, z, 'm.--', lw=2)

#plt.plot( ttendevap, z, 'g.-', lw=2)
#plt.plot( qtendevap, z, 'g.--', lw=2)

plt.xlabel("Heating(J/K/day)")
plt.axvline(x=0, lw=1, color='k')
#plt.xlim(xmin, xmax)


plt.subplot(122)
plt.ylabel("Z")
plt.ylim( ymin, ymax )

plt.plot( camttend, z, 'k.-', lw=2)
plt.plot( camttendtranup, z, 'b.-')
plt.plot( camttendtrandn, z, 'g.-')
plt.plot( camttendcond, z, 'r.-')
plt.plot( camqtend, z, 'k.--', lw=2)
plt.plot( camqtendtranup, z, 'b.--')
plt.plot( camqtendtrandn, z, 'g.--')
plt.plot( camqtendcond, z, 'r--')
plt.axvline(x=0, lw=1, color='k')

plt.xlabel("CAM Heating(J/K/day)")
plt.xlim(xmin, xmax)


plt.tight_layout()

#plt.savefig('plot/detail-'+("%03d" % itime)+'.png', dpi=120, bbox_inches='tight', pad_inches=0)

#dilcape = f.variables['dilcape'][:,0]
#pmassflxbase = f.variables['pmassflxbase'][:,0]

#plt.figure( figsize=(10,5) )
#plt.plot( dilcape )
#plt.plot( pmassflxbase )
plt.savefig('plot/scm-diagmean.png', dpi=100, bbox_inches='tight', pad_inches=0)

plt.show()

