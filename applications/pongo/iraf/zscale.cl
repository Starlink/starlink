procedure zscale ( scale )

real scale {0.0,prompt="Scale factor for Z data"}

begin
   if ( defpac("pongo") ) {
      ccmath (z="z*("//scale//")")
   }
end
