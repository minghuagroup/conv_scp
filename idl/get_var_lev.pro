
 function get_var_lev,var,diff=diff

  spec =  create_struct('var',var)

      lev_Dtdt  = cal_lev([-5.,5.],20) 
      lev_Dtdt_diff  = cal_lev([-2.,2.],20) 
      lev_Dqdt_diff  = cal_lev([-1.,1.],20) 
      ;if(keyword_set(diff))then levj = cal_lev([],20)

  case  strlowcase(var) of

 'u': begin 
      levj = cal_lev([-15,15],15) 
      if(keyword_set(diff))then levj = cal_lev([-5.,5],10)
      scale = 1.
      offset = 0.
      end

 'v': begin
      levj = cal_lev([-15,15],15) 
      if(keyword_set(diff))then levj = cal_lev([-5.,5],10)
      scale = 1.
      offset = 0.
      end

 't': begin
      levj = cal_lev([200.,300],10)
      if(keyword_set(diff))then levj = cal_lev([-1.,5],12)
      scale = 1.
      offset = 0.
      end

 'theta': begin
      levj = cal_lev([285.,365.],15)
      if(keyword_set(diff))then levj = cal_lev([-1.,5],12)
      scale = 1.
      offset = 0.
      end

 'q': begin
      levj = cal_lev([1,20],19)  & levj=[0.1,levj]
      if(keyword_set(diff))then levj = cal_lev([-1.,5],12)
      scale = 1000.
      offset = 0.
      end
 'relhum': begin
      levj = cal_lev([0,100],20)
      if(keyword_set(diff))then levj = cal_lev([-20.,20],20)
      scale = 1.
      offset = 0.
      end
 'cloud': begin
      levj = cal_lev([0.,100],20)
      if(keyword_set(diff))then levj = cal_lev([-20.,20],20)
      scale = 100.
      offset = 0.
      end
 'cldliq': begin
      levj = cal_lev([0,100.],20)
      if(keyword_set(diff))then levj = cal_lev([-30.,30],20)
      scale = 1.0e6
      offset = 0.
      end
 'cldice': begin
      levj = cal_lev([0,10.],20)
      if(keyword_set(diff))then levj = cal_lev([-5.,5],20)
      scale = 1.0e6
      offset = 0.
      end
 'omega': begin
      levj = cal_lev([-70.,70.],20)
      if(keyword_set(diff))then levj = cal_lev([-30.,30],20)
      scale = 36.*24  ;mb/day
      offset = 0.
      end
 'div': begin
      levj = cal_lev([-5,5],20) 
      if(keyword_set(diff))then levj = cal_lev([-2.,2],10)
      scale = 1.0e6  ;
      offset = 0.
      end
 'divt': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'vertdivt': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'divt3d': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'qrl': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'qrs': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'qrlc': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'qrsc': begin
      levj = lev_dtdt
      if(keyword_set(diff))then levj = lev_dtdt_diff
      scale = 86400.
      offset = 0.
      end
 'divq': begin
      levj = cal_lev([-3.0,3.0],20)
      if(keyword_set(diff))then levj = lev_dqdt_diff
      scale = 1000*86400.
      offset = 0.
      end
 'vertdivq': begin
      levj = cal_lev([-3.0,3.0],20)
      if(keyword_set(diff))then levj = lev_dqdt_diff
      scale = 1000*86400.
      offset = 0.
      end
 'divq3d': begin
      levj = cal_lev([-3.0,3.0],20)
      if(keyword_set(diff))then levj = lev_dqdt_diff
      scale = 1000*86400.
      offset = 0.
      end
 'uadv': begin
      levj = cal_lev([-10,10],10) 
      if(keyword_set(diff))then levj = cal_lev([-1.,1],10)
      scale = 86400.
      offset = 0.
      end
 'vadv': begin
      levj = cal_lev([-10.,10],10)
      if(keyword_set(diff))then levj = cal_lev([-1.,1],10)
      scale = 86400.
      offset = 0.
      end
'lhflx': begin
         levj =[0.,150]
      if(keyword_set(diff))then levj = [-10.,30]
          scale = 1.
         offset = 0.
             end
'ps': begin
         levj =[0.,25]
      if(keyword_set(diff))then levj = [-2.,2]
         scale = 0.01
         offset = -1000.
             end
'tsair': begin
         levj =[16.,32]
      if(keyword_set(diff))then levj = [-1.,4]
          scale = 1.
         offset = -273.16
             end
'tg': begin
         levj =[16.,32]
      if(keyword_set(diff))then levj = [-1.,4]
          scale = 1.
         offset = -273.16
             end
'mlb': begin
       levj =[0.,0.5]
      if(keyword_set(diff))then levj = [-0.1,0.1]
          scale = 1.
         offset = 0.
             end
'prec': begin
       levj =[0.,20]
      if(keyword_set(diff))then levj = [-10.,10.]
          scale = 1.
         offset = 0.
       levj = cal_lev(levj,20)
             end
'flnt': begin
       levj =[150.,300]
      if(keyword_set(diff))then levj = [-30.,30.]
          scale = 1.
         offset = 0.
       levj = cal_lev(levj,20)
             end

'u_srf': begin
       levj =[-10.,10]
      if(keyword_set(diff))then levj = [-3.,3.]
          scale = 1.
         offset = 0.
             end

'v_srf': begin
       levj =[-10.,10]
      if(keyword_set(diff))then levj = [-3.,3.]
          scale = 1.
         offset = 0.
             end
'solin': begin
       levj =[380.,480.]
      if(keyword_set(diff))then levj = [-30.,30.]
          scale = 1.
         offset = 0.
             end

 else: begin
      levj = cal_lev([-1000.,1000],20)
      scale = 1.
      offset = -9999.
;;        print,'This vname is not set in get_var_lev!!'
       end
 endcase

  spec = create_struct(spec,'levj',levj,'scale',scale,'offset',offset)

   return,spec
END

