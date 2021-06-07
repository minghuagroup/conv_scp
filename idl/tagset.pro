function tagset,str,tag

if n_elements(str) eq 0 then begin     ; Has it been defined ?
   is_it_set=0
   return,is_it_set
endif

s$=size(str)
n$=n_elements(s$)
if s$(n$-2) ne 8 then begin            ; Is it a Structure ?
   is_it_set=0
   return,is_it_set
endif
   

tag  = strupcase(tag)

tagns=tag_names(str)


if max(where(tagns eq tag )) ne -1 then begin

   tagval    = str.(max(where(tagns eq tag )) )
   sz        = size(tagval)

   IF ( SZ(0) GT 0 )         THEN is_it_set = 1

   IF ( SZ(0) EQ 0 ) AND $ 
      ( SZ(1) EQ 7 )         THEN BEGIN 

      IF ( (TAGVAL) NE '') THEN is_it_set = 1

      IF ( (TAGVAL) EQ '') THEN is_it_set = 0

   ENDIF

   IF ( SZ(0) EQ 0 ) AND  $
      ( SZ(1) LT 6 ) THEN BEGIN

      IF ( FIX(TAGVAL*10000) EQ 0) THEN is_it_set = 0

      IF ( FIX(TAGVAL*10000) GT 0) THEN is_it_set = 1

      IF ( FIX(TAGVAL*10000) LT 0) THEN is_it_set = 1

   ENDIF

ENDIF ELSE BEGIN

   is_it_set = 0

ENDELSE


return,is_it_set

end
