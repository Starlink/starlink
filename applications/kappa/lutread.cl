procedure lutread( lut )
begin
 string tlut=""
 string glut
 glut = str(lut)
 if ( glut != "" ) tlut = "lut=" + glut + " "
 ;
 print ("lutable ",tlut,"mapping=linear ","coltab=external ") | cl
end
