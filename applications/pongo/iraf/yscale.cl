procedure yscale ( scale )

real scale {0.0,prompt="Scale factor for Y data"}

begin
   if ( defpac("pongo") ) {

#  Scale the YCOL values.
      ccmath (y="y*("//scale//")")
   }
end
