procedure pcolumn ( symcol )

string symcol {prompt="Column containing symbol codes"}

begin
   if ( defpac("pongo") ) { 
      readf.symcol = symcol
   }
end
