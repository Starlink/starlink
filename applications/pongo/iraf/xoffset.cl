procedure xoffset (offset)

real offset {0.0,prompt="Offset for X data"}

begin
   if ( defpac("pongo") ) {

#  Offset the XCOL values.
      ccmath (x="x+("//offset//")")
   }
end
