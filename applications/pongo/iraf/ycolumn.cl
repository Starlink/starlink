procedure ycolumn ( ycol )

string ycol {"0",prompt="Y column identifier"}

begin
   if ( defpac("pongo") ) { 
      readf.ycol = ycol
   }
end
