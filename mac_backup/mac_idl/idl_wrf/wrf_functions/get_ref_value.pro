;get_ref_value.pro

function get_ref_value, coord, input, ind, ref

if coord[ind] GT ref then begin
coord2 = coord[ind-1]
input2 = input[ind-1]
endif
if coord[ind] LT ref then begin
coord2 = coord[ind+1]
input2 = input[ind+1]
endif
if coord[ind] EQ ref then begin 
coord2 = coord[ind]
input2 = input[ind]
endif

coord1 = coord[ind]
input1 = input[ind]
input_ref = linear_interpolate(coord1,coord2,input1,input2,ref)

return, input_ref

end

