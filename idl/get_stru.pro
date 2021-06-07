function get_stru,st,name
  vars=strupcase( tag_names(st))
  jj=where(vars eq strupcase(name),cnt) & jj=jj[0]
;  print,vars
;  print,name
; stop
  if(cnt eq 0)then begin
    d = -999.
    print,' ---------> ', name ,'  not in structure. returns -999 '
    return, d
  endif
  d=st.(jj)
  return,d
end

