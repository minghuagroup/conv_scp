function get_homedir
 spawn, 'pwd', dir
 dirs = reverse(str_Sep(dir,'/'))
 dir0 = dirs[0]
 return,dir0
end

