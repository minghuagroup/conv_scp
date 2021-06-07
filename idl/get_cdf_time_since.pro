function get_cdf_time_since, file, year=year,month=month,day=day,$
                             hour=hour,minute = minute, second=second , base0_day = base0_day

 c = get_cdf_att(file,'time', 'units')
 c = strcompress(c)
 line = str_sep(c, ' ')
 jj = where(line eq 'since')
 yymmdd  =  line[jj[0]+1]
 hhmmss  =  line[jj[0]+2] 

 r1 = get_timestamp_value(yymmdd, hhmmss = hhmmss, year=year, month=month, day=day, hour=hour, minute=minute, second=second)

 ;base0_day = julday(month,day,year) - julday(1,1,1) - 365

 base0_day = year*long(365) + julday(month,day,year) -  julday(1,1,year)

 ;asssume 365 day calendar

 return,yymmdd
end

