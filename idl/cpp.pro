
 cc = fltarr(200)
 for i =0, 199 do begin
  
    fi = 'tmp/cmip'+strtrim(i,2)
    make_folder,fi
    cmd = 'cp -r /Users/minghuazhang/Music ' + fi + '/.'
    print,cmd
    spawn, cmd 
 endfor

end


