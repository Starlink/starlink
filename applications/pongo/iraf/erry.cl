procedure erry () 

real erterm {1.0, prompt="Relative length of terminals on error bars"}

begin
   if ( defpac("pongo") ) {
      errorbar (action="y", erterm=erterm)
   } 
end
