procedure yoffset (offset)

real offset {0.0,prompt="Offset for Y data"}

begin
   if ( defpac("pongo") ) {

#  Offset the YCOL values.
      ccmath (y="y+("//offset//")")
   }
end
