
function era5_vars
;========================== 



 files0 = ['filem','files','filev','filefi','filefm','fileac']
 files  = '../data/ERA5/' + files0 + '.csv'
;
;
 vars_names = [str_replace(files0,'file','vars_'), 'vars_era5_all','vars_era5_cesm',$
     'vars_era5_cesm_stru']
 
 ; To change names, edit the spreadsheet files
 
 nf = n_elements(files)
 vars_era5 =''
 vars_era5_str =''
 vars_cesm = ''
 era5_table = create_struct('Name','ERA5 Table', 'input_file',files0)
 era5_vars  = create_struct('Name',vars_names)
 era5_vars_str  = create_struct('Long_Name',vars_names)
 cesm_vars  = create_struct('Name',vars_names)
 for i = 0, nf-1 do begin
   file = files[i]
   Table = READ_CSV(file, HEADER=names, n_TABLE_HEADER=0, TABLE_HEADER = title)
   jj = where(names eq 'shortName') & jj=jj[0]
   if(i eq 0)then begin
     vars_era5 = table.(jj)
     vars_era5_str = table.(jj-1)
     vars_cesm = table.(jj+1)
   endif else begin
     vars_era5 = [vars_era5, table.(jj)]
     vars_era5_str = [vars_era5_str, table.(jj-1)]
     vars_cesm = [vars_cesm, table.(jj+1)]
   endelse
   era5_table = create_struct(era5_table, files0[i], table)
   era5_vars  = create_struct(era5_vars, vars_names[i], table.(jj))
   era5_vars_str  = create_struct(era5_vars_str, vars_names[i], table.(jj-1))
   cesm_vars  = create_struct(cesm_vars, vars_names[i], table.(jj+1))
  endfor

  jj = sort(vars_era5)
; jj = uniq(vars_era5,sort(vars_era5))
 vars_era5_all  = vars_era5[jj]
 vars_era5_str_all  = vars_era5_str[jj]
 vars_era5_cesm = vars_cesm[jj]


 vars_number = ['100U','100V','10U','10V','10SI','2T','2D']

 era5_vars  = create_struct(era5_vars, 'vars_era5_all',vars_era5_all, $
    'vars_era5_cesm', vars_era5_cesm, 'vars_era5_cesm_stru',cesm_vars, $
    'vars_era5_str_all',vars_era5_str_all, 'vars_number',vars_number )
  
save,file='era5_vars.sav0', vars_era5, vars_cesm, era5_table, era5_vars,vars_era5_str_all

return,era5_vars
end

;NAME            STRING    Array[6]
;VARS_M          STRING    Array[16]
;VARS_S          STRING    Array[14]
;VARS_V          STRING    Array[44]
;VARS_FI         STRING    Array[75]
;VARS_FM         STRING    Array[39]
;VARS_AC         STRING    Array[39]
;VARS_ERA5_ALL   STRING    Array[228]
;VARS_ERA5_CESM  STRING    Array[228]
;VARS_ERA5_CESM_STRU
;STRUCT    -> <Anonymous> Array[1]
;IDL> print,a.VARS_M
;pv crwc cswc z t u v q w vo d r o3 clwc ciwc cc
;IDL> print,a.VARS_s
;cl dl cvl cvh tvl tvh slt sdfor z sdor isor anor slor lsm
;IDL> print,a.VARS_v
;vima vit vike vithe vipie vipile vitoe viec vimae viman vikee viken vithee vithen viwve viwvn vige vign vitoee vitoen vioze viozn vilwd viiwd vimad viked vithed viwvd vigd
;vitoed viozd vilwe vilwn viiwe viiwn vimat tclw tciw tcslw tcrw tcsw tcw tcwv tco3
;IDL> print,a.VARS_fi
;cin zust lmlt lmld lblt ltlt lshf lict licd aluvp dndzn aluvd dndza alnip dctb alnid tplb tplt cbh deg0l i10fg ci asn rsn sst istl1 istl2 istl3 istl4 swvl1 swvl2 swvl3 swvl4
;cape lai_lv lai_hv u10n v10n sp stl1 sd chnk msl blh tcc 10u 10v 2t 2d stl2 stl3 lcc mcc hcc src ilspf crr lsrr csfr lssfr iews inss ishf ie skt stl4 tsn fal fsr flsr 100u
;100v ptype kx totalx
;IDL> print,a.VARS_fm
;msror mssror mser msmr mlspf msdwuvrf mlspr mcpr msr mbld msshf mslhf msdwswrf msdwlwrf msnswrf msnlwrf mtnswrf mtnlwrf metss mntss mer megwss mngwss mgwd mror mtnswrfcs
;mtnlwrfcs msnswrfcs msnlwrfcs mtdwswrf mvimd mtpr mcsr mlssr msdrswrf msdrswrfcs msdwswrfcs msdwlwrfcs mper
;IDL> print,a.VARS_ac
;lspf uvb bld sshf slhf ssrd strd ssr str tsr ttr ewss nsss lgws mgws gwd tsrc ttrc ssrc strc tisr vimd fdir cdir ssrdc strdc sro ssro es smlt lsp cp sf e ro tp csf lsf pev
;IDL> print,a.VARS_era5_all
;100u 100v 10u 10v 2d 2t alnid alnip aluvd aluvp anor asn bld blh cape cbh cc cdir chnk ci cin ciwc cl clwc cp crr crwc csf csfr cswc cvh cvl d dctb deg0l dl dndza dndzn e es
;ewss fal fdir flsr fsr gwd hcc i10fg ie iews ilspf inss ishf isor istl1 istl2 istl3 istl4 kx lai_hv lai_lv lblt lcc lgws licd lict lmld lmlt lsf lshf lsm lsp lspf lsrr lssfr
;ltlt mbld mcc mcpr mcsr megwss mer metss mgwd mgws mlspf mlspr mlssr mngwss mntss mper mror msdrswrf msdrswrfcs msdwlwrf msdwlwrfcs msdwswrf msdwswrfcs msdwuvrf mser msl
;mslhf msmr msnlwrf msnlwrfcs msnswrf msnswrfcs msr msror msshf mssror mtdwswrf mtnlwrf mtnlwrfcs mtnswrf mtnswrfcs mtpr mvimd nsss o3 pev ptype pv q r ro rsn sd sdfor sdor
;sf skt slhf slor slt smlt sp src sro sshf ssr ssrc ssrd ssrdc ssro sst stl1 stl2 stl3 stl4 str strc strd strdc swvl1 swvl2 swvl3 swvl4 t tcc tciw tclw tco3 tcrw tcslw tcsw
;tcw tcwv tisr totalx tp tplb tplt tsn tsr tsrc ttr ttrc tvh tvl u u10n uvb v v10n viec vigd vige vign viiwd viiwe viiwn vike viked vikee viken vilwd vilwe vilwn vima vimad
;vimae viman vimat vimd viozd vioze viozn vipie vipile vit vithe vithed vithee vithen vitoe vitoed vitoee vitoen viwvd viwve viwvn vo w z z zust
;
;CAPE CIN CLDHGH CLDICE CLDLIQ CLDLOW CLDMED CLDTOT CLOUD CRWC CSWC DIV DREHFT FLDS FLDS FLDSC FLDSC FLNS FLNS FLNSC FLNSC FLNT FLNT FLNTC FLNTC FSDS FSDS FSDSC FSDSC
;FSNS FSNS FSNSC FSNSC FSNT FSNT FSNTC FSNTC ICEFRAC LACKD LAKEC LANDFRAC LHFLX LHFLX O3 OMEGA PBLH PHIS PRECC PRECC PRECC PRECCS PRECCS PRECCS PRECL PRECL PRECL PRECLS
;PRECLS PRECLS PRECT PRECT PRECTS PS PSL PV Q QFLX QFLX QFLX RELHUM SHFLX SHFLX SHFLX SNOWTH SOLIN SOLIN SST T TAUGWX TAUGWX TAUGWY TAUGWY TAUX TAUX TAUX TAUY TAUY TAUY
;TGCLDIWP TGCLDLWP TMQ TREFHT TS TS1 TS2 TS3 TS4 U U100 V V100 VOR WGUST Z3 alnid alnip aluvd aluvp anor asn bld cbh cdir chnk cvh cvl dctb deg0l dndza dndzn es fal
;fdir flsr fsr gwd i10fg ilspf isor istl1 istl2 istl3 istl4 kx lai_hv lai_lv lblt licd lict lmld lmlt lsf lshf lspf ltlt mbld mgwd mlspf mper mror msdrswrf msdrswrfcs
;msdwuvrf mser msmr msror mssror mvimd pev ptype ro rsn sdfor sdor slor slt smlt src sro ssro swvl1 swvl2 swvl3 swvl4 tco3 tcrw tcslw tcsw tcw totalx tplb tplt tsn tvh
;tvl u10n u10n uvb v10n v10n viec vigd vige vign viiwd viiwe viiwn vike viked vikee viken vilwd vilwe vilwn vima vimad vimae viman vimat vimd viozd vioze viozn vipie
;vipile vit vithe vithed vithee vithen vitoe vitoed vitoee vitoen viwvd viwve viwvn
