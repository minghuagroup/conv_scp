
print,filein2
kuplcl = get_fld(filein2,'kuplcl')
nplume = n_elements(kuplcl[*,0])
kuplev = get_fld(filein2,'nconvlev')
kupbase = get_fld(filein2,'kupbase')
kuptop = kupbase - kuplev

var='trigdp'
trigdp=get_fld(filein2,var)

var='kuplaunch'              
kuplaunch=get_fld(filein2,var)

var='massflxbase_mconv'       
massflxbase_mconv=get_fld(filein2,var)

var='massflxbase_cape'                
massflxbase_cape=get_fld(filein2,var) 

var='prec'                           
prec=get_fld(filein2,var)            

var='massflxbase'                    
massflxbase=get_fld(filein2,var)

var='diffdse_up'
diffdse_up = get_fld(filein2,var)
var='diffq_up'
diffq_up = get_fld(filein2,var)
var='diffdse_dn'
diffdse_dn = get_fld(filein2,var)
var='diffq_dn'
diffq_dn = get_fld(filein2,var)

var    = 'tbef'
tbef       = get_fld(filein2,var)
var    = 't'
tt       = get_fld(filein2,var)
var    = 'q'
qq       = get_fld(filein2,var)
var    = 'tint'
tint       = get_fld(filein2,var)
var    = 'qint'
qint       = get_fld(filein2,var)
var    = 'qsatint'
qsatint      = get_fld(filein2,var)
rh = qq
rh[0,*,*] = qq[0,*,*]/qsatint[0,*,*]*100. 


var    = 'qbef'
qbef       = get_fld(filein2,var)

var    = 'ent_rate'
ent_rate       = get_fld(filein2,var)
var    = 'ent_rate_sh'
ent_rate_sh       = get_fld(filein2,var)
ent_rate_dp = ent_rate - ent_rate_sh

var    = 'det_rate'
det_rate       = get_fld(filein2,var)
var    = 'det_rate_sh'
det_rate_sh       = get_fld(filein2,var)
det_rate_dp = det_rate - det_rate_sh


var    = 't'
t       = get_fld(filein2,var)

var    = 'q'
q       = get_fld(filein2,var)

var    = 'diffq_up'
diffq_up       = get_fld(filein2,var)
var    = 'diffdse_up'
diffdse_up       = get_fld(filein2,var)
var    = 'diffq_dn'
diffq_dn       = get_fld(filein2,var)
var    = 'diffdse_dn'
diffdse_dn       = get_fld(filein2,var)

var    = 'w_up_mid'
w_up_mid       = get_fld(filein2,var)
var    = 'buoy_mid'
buoy_mid       = get_fld(filein2,var)
var    = 'mse_up_mid'
mse_up_mid       = get_fld(filein2,var)
var    = 't_up_mid'
t_up_mid       = get_fld(filein2,var)
var    = 'q_up_mid'
q_up_mid       = get_fld(filein2,var)
var    = 'normassflx_up_mid'
normassflx_up_mid       = get_fld(filein2,var)

var    = 'mseint'
mseint       = get_fld(filein2,var)

var    = 'msesatint'
msesatint       = get_fld(filein2,var)

var    = 'mse_closure'
mse_closure       = get_fld(filein2,var)

var    = 'mse'
mse= get_fld(filein2,var)
var    = 'msesat'
msesat= get_fld(filein2,var)
var    = 'z'
z= get_fld(filein2,var)
var    = 'p'
p= get_fld(filein2,var)

var    = 'rho'
rho= get_fld(filein2,var)

var    = 'bs_xc'
bs_xc= get_fld(filein2,var)
var    = 'buoy_closure'
buoy_closure= get_fld(filein2,var)


var    = 'radius_up'
radius_up= get_fld(filein2,var)
var    = 'mse_up'
mse_up= get_fld(filein2,var)
var    = 'dse_up'
dse_up= get_fld(filein2,var)
var    = 't_up'
t_up= get_fld(filein2,var)
var    = 'q_up'
q_up= get_fld(filein2,var)
var    = 'qliq_up'
qliq_up= get_fld(filein2,var)
var    = 'qice_up'
qice_up= get_fld(filein2,var)
var    = 'normassflx_up'
normassflx_up= get_fld(filein2,var)
var    = 'mse_up_plume'
mse_up_plume= get_fld(filein2,var)
var    = 'mse_dn'
mse_dn= get_fld(filein2,var)
var    = 'normassflx_dn'
normassflx_dn= get_fld(filein2,var)

var    = 'stendsum'
stendsum= get_fld(filein2,var)
var    = 'qtendsum'
qtendsum= get_fld(filein2,var)

var    = 'camstend'
camstend= get_fld(filein2,var)
var    = 'camqtend'
camqtend= get_fld(filein2,var)
var    = 'camstendcond'
camstendcond= get_fld(filein2,var)
var    = 'camqtendcond'
camqtendcond= get_fld(filein2,var)
var    = 'camstendtranup'
camstendtranup= get_fld(filein2,var)
var    = 'camqtendtranup'
camqtendtranup= get_fld(filein2,var)

var    = 'accuprec'
accuprec= get_fld(filein2,var)

var    = 'evaprate'
evaprate= get_fld(filein2,var)

var    = 'condrate'
condrate= get_fld(filein2,var)


var    = 'camstend'
camstend= get_fld(filein2,var)

var    = 'stend'
stend= get_fld(filein2,var)
var    = 'qtend'
qtend= get_fld(filein2,var)
var    = 'stendcond'
stendcond= get_fld(filein2,var)
var    = 'qtendcond'
qtendcond= get_fld(filein2,var)
var    = 'stendevap'
stendevap= get_fld(filein2,var)
var    = 'qtendevap'
qtendevap= get_fld(filein2,var)
var    = 'stendtranup'

stendtranup= get_fld(filein2,var)
var    = 'qtendtranup'
qtendtranup= get_fld(filein2,var)
var    = 'stendtrandn'
stendtrandn= get_fld(filein2,var)
var    = 'qtendtrandn'
qtendtrandn= get_fld(filein2,var)
var    = 'massflxsum'
massflxsum= get_fld(filein2,var)

for k=1,nplume-1 do begin
 t[k,*,*] = t[0,*,*]
 q[k,*,*] = q[0,*,*]
endfor

tv = t*(1.+0.61*q)
tv_up = t_up*(1.+0.61*q_up)
dtv_up = tv_up - tv
dtt = t_up-t
dqq = q_up-q

ent_rate3 = ent_rate   
ent_rate_sh3 = ent_rate_sh
det_rate_sh3 = det_rate_sh
det_rate3 = det_rate     
normassflx_up3 = normassflx_up
normassflx_dn3 = normassflx_dn
massflxsum3 = massflxsum
massflxbase3 = massflxbase

stop
;below is for comparative diagnostics and plots can be ignored


ent_rate2 = ent_rate 
ent_rate_sh2 = ent_rate_sh
det_rate_sh2 = det_rate_sh
det_rate2 = det_rate      
normassflx_up2 = normassflx_up
normassflx_dn2 = normassflx_dn
massflxsum2 = massflxsum
massflxbase2 = massflxbase

window,/free
ip=1 ;nplume-1                                                          

plot,reform(ent_rate3[ip,*,it])                                
oplot,reform(ent_rate_sh3[ip,*,it]),linest=2,color=colors.brown                  
oplot,reform(ent_rate2[ip,*,it]),color=colors.red              
oplot,reform(ent_rate_sh2[ip,*,it]),linest=2,color=colors.cyan
oplot,reform(normassflx_up3[ip,*,it])/10000.,color=colors.blue ,thick=2
oplot,reform(normassflx_up2[ip,*,it])/10000.,color=colors.green,thick=2
print,reform(normassflx_up2[ip,*,it]-normassflx_up3[ip,*,it])

oplot,reform(det_rate3[ip,*,it]),linest=1                                
oplot,reform(det_rate_sh3[ip,*,it]),linest=1,color=colors.brown  ,thick=2                
oplot,reform(det_rate2[ip,*,it]),color=colors.red,linest=1,thick=2              
oplot,reform(det_rate_sh2[ip,*,it]),linest=1,color=colors.cyan,thick=2
end
