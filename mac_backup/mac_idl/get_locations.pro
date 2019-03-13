 pro get_locations,location,xrange,yrange,londel,latdel,xsize,ysize
 
;for plot_cam_xx to set boundary and labels 

  xsize = 0. 
  ysize = 0. 
  londel = 30.
  latdel = 20

 case location of

'eastern pacific': begin
  xrange=[210.,250]
  yrange=[10.,40]
  londel = 10.
  latdel=10.
  xsize= 520
  ysize=460
                   end
'global': begin
  xrange=[0,360]
  yrange=[-90.,90]
  londel = 60.
  latdel=  30.
  xsize= 0
  ysize= 0
                   end
'asian monsoon': begin
  xrange=[50.,140]
  yrange=[-10.,50]
  londel = 10.
  latdel=10.
                   end
'asian monsoon big': begin
  xrange=[30.,180]
  yrange=[-20.,60]
  londel = 10.
  latdel=10.
                   end
'tropics 10': begin
  xrange=[0.,360]
  yrange=[-20.,20]
  londel = 30.
  latdel=5.
  xsize = 960.
  ysize = 380.
                   end


 else:

 endcase
 
 return

 end
 
