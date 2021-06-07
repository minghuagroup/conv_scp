
 htmlfile ='html/clds_2017072724.html'
 dir='gif_cld_2017072724/'

 ;htmlfile ='html/clds_20170727.html'
 ;dir='gif_clouds/'

 groups = file_search(dir+'*.gif')
 groups2 = file_search(dir+'*.GIF')
 groups  = [groups,groups2]
 web_view,'../'+groups,htmlfile,title='clouds',ncolumns=2  ;relative to html folder
 print,'webfile:',htmlfile

end
