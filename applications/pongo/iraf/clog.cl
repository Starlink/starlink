procedure clog (action)

string action {"x", prompt="Data column to transform (x,y,z)"}

begin
   string lact
   string opt
   lact = action
   if ( defpac("pongo") ) {
      cllog (action=lact, mode=h)

      #  Now set the L option of XOPT or YOPT of boxframe and readf.
      if ( lact == "x" || lact == "X" ) {
         opt = boxframe.xopt
         if ( stridx( "L", opt ) != 1 && stridx( "l", opt ) != 1 ) {
            boxframe.xopt = "L"//opt
            readf.xopt = "L"//opt
         }
      } else if ( lact == "y" || lact == "Y" ) {
         opt = boxframe.yopt
         if ( stridx( "L", opt ) != 1 && stridx( "l", opt ) != 1 ) {
            boxframe.yopt = "L"//opt
            readf.yopt = "L"//opt
         }
      }

   }
end
