procedure xscale ( scale )

real scale {0.0,prompt="Scale factor for X data"}

begin
   if ( defpac("pongo") ) {

#  Scale the XCOL values.
      ccmath (x="x*("//scale//")")
   }
end
