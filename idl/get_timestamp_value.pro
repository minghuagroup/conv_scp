function get_timestamp_value, yymmdd, hhmmss = hhmmss, year=year,month=month,day=day,$
                             hour=hour,minute = minute, second=second

 ymd = str_sep(yymmdd,'-')
 year = long(ymd[0])
 month = long(ymd[1])
 day = long(ymd[2])

if(keyword_Set(hhmmss))then begin
 hms = str_Sep(hhmmss,':')
 hour = long(hms[0])
 minute = long(hms[1])
 second = long(hms[2] )
endif

 return, 1
end

