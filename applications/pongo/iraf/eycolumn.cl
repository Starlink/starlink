procedure eycolumn ( eycol )

string eycol {prompt="Identifier of Y error column"}

begin
   if ( defpac("pongo") ) { 
      readf.eycol = eycol
   }
end
