
function vname,var
dqdt = 'STENDCONVDPCOND'
dqldt = 'QTENDCONVDPCOND'
dqidt = 'STENDCONVDPEVAP'
dncdt = 'QTENDCONVDPEVAP'
rprd = 'STENDCONVDPTRANUP'
dlf = 'QTENDCONVDPTRANUP'
ntprprd = 'STENDCONVDPTRANDN'
precrate = 'QTENDCONVDPTRANDN'

vars1 = ['dqdt','dqldt','dqidt','dncdt','rprd','dlf','ntprprd','precrate']
print,vars1
vars2 = [dqdt,dqldt,dqidt,dncdt,rprd,dlf,ntprprd,precrate]

jj=where(var eq vars1,cnt)
if(cnt eq 0)then begin
    print,'No variable match in vname conversion stopped inside vname'
    stop
endif else begin
    i=jj[0]
    print,var, '--> ',vars1[i],' ',vars2[i]
    return, vars2[i]
endelse

end
  

