import os
import glob

def strip_code(fnin, fnout):
    fidin = open(fnin, 'r')
    fidout = open(fnout, 'w')
    for line in fidin:
        tmp = line.strip()
        if len(tmp) > 0:
            if not (tmp[0] in ['!', '#']):
                fidout.write(line)
        #else:
        #    fidout.write('\n')
    fidin.close()
    fidout.close()
    return

def replace_string(fnin, fnout, replacements={ 'real(r8)':'real(kind=8)', '_r8':'', \
        'private':'!private', 'public':'!public', 'save':'!save'}):
    infile = open(fnin, 'r')
    outfile = open(fnout, 'w')
    for line in infile:
        for src, target in replacements.iteritems():
            line = line.replace(src, target)
        outfile.write(line)
    infile.close()
    outfile.close()
    return

def test():
    subroutines = ['conv_jp_tend', 'cal_twet2d', 'cal_qsat2d', 'cal_qsat', 'cal_mse2tsat', 'mse2tsat', \
            'cal_launchtocldbase', 'cal_mse_up', 'cal_mse_up_old', 'cal_evap', 'cal_mse_dn', \
            'cal_cape', 'cal_tendtransport']

    cdef  = 0
    insub = 0
    i = 1
    fidin = open(fnin, 'r') 
    fidout = open(fnout, 'w')
    fidout.write('module offline_ecp\n')
    fidout.write('implicit none\n')
    fidout.write('integer :: ncol=0, nlev=0, nlevp=0\n')
    fidout.write('contains\n')

    for line in fidin:
        outflag = True
        tmp = line.strip()
        if len(tmp) > 0:
            if tmp[0] == '!':  outflag = False

            if tmp[0] == '#':  
                cdef += 1
                outflag = False 
            if cdef%2 == 1:  outflag = False

            if insub%2 == 0: outflag = False
            if 'subroutine' in tmp:
                for sub in subroutines:
                    if sub in tmp:
                        insub += 1
                        outflag = True
                        break


            #print(i, cdef, insub, line)
            if outflag:
                fidout.write(line)
        i += 1

    fidin.close()

    fidout.write('end module offline_ecp')
    fidout.close()

    return


if __name__ == '__main__':
    flist = glob.glob('../src/*.F90') 
    for fn in flist:
        #replace_string(fn, './'+fn.split('/')[-1] )
        replace_string(fn, './tmp.F90' )
        os.system('gfortran -E -DOFFLINECP -o tmp tmp.F90')
        strip_code('./tmp', './'+fn.split('/')[-1])
        os.system('rm -f ./tmp*')


