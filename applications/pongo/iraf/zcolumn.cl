procedure zcolumn ( zcol )

string zcol {"0",prompt="Z column identifier"}

begin
   if ( defpac("pongo") ) {
      readf.zcol = zcol
   }
end
