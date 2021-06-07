function struct_dim_change,data3,vtime,$
     take=take,$
     put=put,data3_in=data3_in,$
     expand=expand,varsne=varsne,$
     diff = diff, varsndiff=varsndiff,$
     lev2=lev2,clev=clev,$
     trans=trans
;----------------------------
;time dimension  ;everything starts from 1
;d[np,nt],expand in time,varsne ; variable not to expand 

; d4 = struct_dim_change(data3,'tsec',take=2) ;take the second time data3
; d4 = struct_dim_change(data3,'tsec',put=2,data3_in) 
;                        ;put the data3_in in second time step data3
; d4 = struct_dim_change(data3,'tsec',expand=6) ;expand the time dimension
;                                     take the 1st value 
;    expand needs to have input of 1 step only       
; d4 = struct_dim_change(data3,'tsec',expand=6,varsne=['bdate']) 
;                                     take the 1st value 
; d4 = struct_dim_change(data3,'tsec',trans=1) ; transpose all d[np,nt] arrays
;            transpose all d[np,nt] arrays
; dd4 = struct_dim_change(data3,'tsec',diff=1,data3_in=data0,varsndiff=['lev']) ;
;                dd3=data3-data0, no vertical interpolation
; 
; dd4 = struct_dim_change(data3,'tsec',levl2=lev2) ;
; lev to do vertical interpolation for 2d array d[np,nt] to lev2
;------------------------------

  d3=data3

  vars=tag_names(d3)
  vars=strlowcase(vars)
  jj = where(vars eq vtime,cnt)
  if(cnt le 0)then begin
         print,'time mark not right',tsec,' not in ',vars
         print,'stopped in struct_dim_change'
         stop
  endif
  jj=jj[0]
  time = d3.(jj)
    sz=size(time) & j1=n_elements(sz)
   nt1 = sz[j1-1]

;=================================

 if(keyword_set(take))then begin  ; take out one time sample
  it=take-1

  for i=0,n_elements(vars)-1 do begin
    dj=d3.(i)
    sz = size(dj)
    case sz[0] of
 1: if(sz[1] eq nt1)then dj=dj[it]
 2: dj = reform(dj[*,it])
 else:
 endcase 

    if(i eq 0)then d4 = create_struct(vars[i],dj) else $
     d4=    create_struct(d4,vars[i],dj)
    endfor
 endif

;=================================
 if(keyword_set(put))then begin  ; expant the t dimension to expand
  it=put-1

  if(not keyword_set(data3_in))then print,'data3 has to be input'

  for i=0,n_elements(vars)-1 do begin
    dj=d3.(i)
    sz = size(dj)
    case sz[0] of
 1: if(sz[1] eq nt1)then dj[it] = data3_in.(i)
 2: dj[*,it] = data3_in.(i)
 else:
 endcase

    if(i eq 0)then d4 = create_struct(vars[i],dj) else $
     d4=    create_struct(d4,vars[i],dj)
    endfor
;========================================
endif ;put

;=================================
 if(keyword_set(lev2))then begin  ; expant the t dimension to expand

  if(not keyword_Set(clev))then clev='lev'
  jlev = where(vars eq clev) & jlev=jlev[0]
  lev = data3.(jlev)

    d4 = create_struct(clev,lev2) 

  for i=0,n_elements(vars)-1 do begin
  if(i ne jlev)then begin
    dj=d3.(i)
    sz = size(dj)
   if(sz[0] eq 2)then begin
     for it=0,nt1-1 do begin
         djj=reform(dj[*,it])
         dj[*,it] = interpol(djj,lev,lev2)
     endfor
   endif
     d4=    create_struct(d4,vars[i],dj)
  endif
  endfor
 endif  ;vertical intepolation
;=================================
 if(keyword_set(trans))then begin  ; expant the t dimension to expand

  for i=0,n_elements(vars)-1 do begin
    dj=d3.(i)
    sz = size(dj)
   if(sz[0] eq 2)then dj=transpose(dj) 

    if(i eq 0)then d4 = create_struct(vars[i],dj) else $
     d4=    create_struct(d4,vars[i],dj)
   endfor
;========================================
endif ;expand


;============================
 if(keyword_set(expand))then begin  ; take out one time sample

  nt = expand
  if(not keyword_set(varsne))then $
         varsne=['fileout','bdate','phis','lat','lon','lev']
  
  for i=0,n_elements(vars)-1 do begin
    dj=d3.(i)
    var=vars[i]
    jvar = where(varsne eq var,cnt)

    if(cnt eq 0)then begin ; do the expansion 
    sz = size(dj)
    case sz[0] of
 0: dj = replicate(dj,nt)
 1: dj = replicate2(dj,nt)
 else:
 endcase
    endif
     if(i eq 0)then d4 = create_struct(vars[i],dj) else $
     d4=    create_struct(d4,vars[i],dj)

    endfor
 endif ;expand

;============================
 if(keyword_set(diff))then begin  ; take out one time sample

  if(not keyword_set(varsne))then $
         varsndiff=['fileout','bdate','phis','lat','lon','lev',vtime]
  if(not keyword_Set(data3_in))then stop

   vars_in=strlowcase(tag_names(data3_in))

  for i=0,n_elements(vars)-1 do begin
    dj=d3.(i)
    var=vars[i]
    jvar = where(varsndiff ne var,cnt1)
    if(cnt1)then begin ; do the diff
        k = where(vars_in eq var,cnt2)
        if(cnt2) then begin
          dj=d3.(i) - data3_in.(k[0])
            if(i eq 0)then d4 = create_struct(vars[i],dj) else $
                    d4=    create_struct(d4,vars[i],dj)
        endif
    endif else begin
     if(i eq 0)then d4 = create_struct(vars[i],dj) else $
     d4=    create_struct(d4,vars[i],dj)
    endelse

    endfor
 endif ;diff

   return,d4  
end

