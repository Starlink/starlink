procedure xcolumn ( xcol )

string xcol {"0",prompt="X column identifier"}

begin
   if ( defpac("pongo") ) {
      readf.xcol = xcol
   }
end
