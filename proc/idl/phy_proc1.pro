;goto,jump1


var = 'q' & k=29
;var = 'qpert2'
scale =1.e3
aa = get_fldphy3(2,var,folder='fout9',x=xx,y=yy)

siz = size(aa)
if(siz[0] eq 3)then aa = reform(aa[*,k,*])
dd = aa*scale

range=[min(dd),max(dd)]
lev1 = cal_lev(range,20)
;lev1=cal_lev([0,2.],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*100,title=var
print,range
end
