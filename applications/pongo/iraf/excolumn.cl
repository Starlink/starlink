procedure excolumn ( excol )

string excol {prompt="Identifier of X error column"}

begin
   if ( defpac("pongo") ) { 
      readf.excol = excol
   }
end
