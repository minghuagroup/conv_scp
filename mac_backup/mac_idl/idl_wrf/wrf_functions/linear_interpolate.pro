;linear_interpolate.pro


FUNCTION linear_interpolate, x1, x2, y1, y2, xin

m = (y2 - y1)/(x2 - x1)

b = y2 - (m*x2)

ynew = (m*xin) + b

return, ynew
END
