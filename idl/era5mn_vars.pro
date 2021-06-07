function era5mn_vars,varc,rev=rev, ml=ml

;https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Monthlymeans
; LH and SH fluxes may need to change sign and divide by 86400., seems to be more appropriate

if( not keyword_set(rev))then begin
case varc of 
'CLDSNOW':  var = 'CSWC'
'CLDRAIN':  var = 'CRWC'
'CLDICE':  var = 'CIWC'
'CLDLIQ':  var = 'CLWC'
'CLOUD':   var = 'CC'

'CLDHGH':  var = 'HCC'
'CLDLOW':  var = 'LCC'
'CLDMED':  var = 'MCC'
'CLDTOT':  var = 'TCC'
'FLDS':    var = 'msdwlwrf' ;STRD'
'FLNS':    var = 'msnlwrf' ;'STR'
'FLDSC':   var = 'msdwlwrfcs' ;STRC'
'FLDT':    var = 'mtnlwrf' ;'TTR'
'FLNSC':   var = 'msnlwrfcs' ;STRC'
'FLNT':    var = 'mtnlwrf' ;'TTR'
'FLNTC':   var = 'mtnlwrfcs' ;'TTRC'
'FSDS':    var = 'msdwswrf' ;'SSR'
'FSDSC':   var = 'msdwswrfcs' ;SSR'
'FSNS':    var = 'msnswrf' ;SSR'
'FSNSC':   var = 'msnswrfcs' ;'SSRC'
'FSNT':    var = 'mtnswrf' ;'TSR'
'FSNTC':   var = 'mtnswrfcs' ;TSRC'
'ICEFRAC': var = 'CI'
'LHFLX':   var = 'MSLHF' ;'IE' ;'SLHF'
'OMEGA':   var = 'W'
'ORO':     var = 'LSM'
'PBLH':    var = 'BLH'
'PHIS':     var = 'Z'
'PRECC':   var = 'mcpr' ;'MCPR' ;CRR' ;'CP'
'PRECL':   var = 'mlspr' ;MLSPR' ; LCP' ;LSRR';'LSP'
'PRECT':   var = 'mtpr' ;TP'
'PRECSC':  var = 'mcsr' ;'MSR' ;SF' ;CSFR' ;'CSF'
'PRECSL':  var = 'mlssr' ;LSF' ;'LSSFR'; 'LSF'
'PS':      var = 'SP'
'PSL':     var = 'MSL'
'QFLX':    var = 'mer'
'RELHUM':  var = 'R'
'SHFLX':   var = 'MSSHF' ;'ISHF' ;SSHF'
;'SNOWHICE':var = 'SD'
'SNOWDP':var = 'sd'
'SOLIN':   var = 'mtdwswrf' ;'SI'
'SST':     var = 'sst'
'TD':      var = 'VAR_2D'
'TAUX':    var = 'iews' ;EWSS' ;'IEWS' ;'EWSS'
'TAUY':    var = 'inss' ;NWSS' ;'INSS' ;'NSSS'
'TAUTMSX': var = 'LGWS'
'TAUTMSY': var = 'MGWS'
'TGCLDLWP':var = 'TCLW'
'TGCLDIWP':var = 'TCIW'
'TGCLDCWP':var = 'TCW'
'TMQ':     var = 'TCWV'
'TS':      var = 'VAR_2T'
'TS1':     var = 'STL1'
'TS2':     var = 'STL2'
'TS3':     var = 'STL3'
'TS4':     var = 'STL4'
'TSICE':   var = 'ISTL1'
'TSMX':    var = 'MX2T'
'TSMN':    var = 'MN2T'
'U10':     var = 'VAR_10U'
'V10':     var = 'VAR_10V'
'WGUSTD':  var = '10FG'
'Z3':      var = 'Z'
'T':      var = 'T'
'Q':      var = 'Q'
'U':      var = 'U'
'V':      var = 'V'
else:    var = varc
endcase


return,strupcase(strtrim(var,2))

endif ;from cam var to era var

restore,'era5mn.sav0'

if( not keyword_set(ml))then ml=0
if(ml) then vars = varsm else vars = [varss,varsfi,varsfm,varsv,varsac]

varc2 = era5mn_vars(varc)
for k=0,n_elements(vars)-1 do begin
 var2 = era5mn_vars(vars[k])
 print,k,var2
 if(varc2 eq var2)then stop; return, var2
endfor

print,'no such variable name is found: in era5mn_vars ', varc
print,'make sure ML flag is set when calling for a multi-level field'
stop

end

