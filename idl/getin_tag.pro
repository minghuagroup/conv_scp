function getin_tag, var, stru, r=r

  if(keyword_set(r))then begin
     if(belongsto(strupcase(var), tag_names(stru))) then return,1
     return,0
  endif

  if(not belongsto(strupcase(var), tag_names(stru))) then return,1
  return,0
end 

