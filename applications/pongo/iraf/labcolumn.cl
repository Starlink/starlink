procedure labcolumn ( labcol )

string labcol {prompt="Identifier of column with labels"}

begin
   if ( defpac("pongo") ) { 
      readf.labcol = labcol
   }
end
