procedure yerr ()

real erterm {1.0,prompt="Relative length of terminals"}

begin
   if ( defpac("pongo") ) {
      errorbar (action="y", symerr=yes, erterm=erterm )
   }
end
