procedure data ( data )

string data {prompt="Name of data file"}

begin
   if ( defpac("pongo") ) { 
      readf.data = data
   }
end
