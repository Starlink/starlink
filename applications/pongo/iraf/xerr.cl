procedure xerr ()

real erterm {1.0,prompt="Relative length of terminals"}

begin
   if ( defpac("pongo") ) {
      errorbar (action="x", symerr=yes, erterm=erterm )
   }
end
