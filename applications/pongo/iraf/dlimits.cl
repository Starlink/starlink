procedure dlimits ()

string projection {"NONE",prompt="Astrometric projection"}
string racentre {"0",prompt="Projection centre, RA"}
string deccentre {"0",prompt="Projection centre, DEC"}

begin
   if ( defpac("pongo") ) {
      world (action="data", projection=projection, racentre=racentre, 
             deccentre=deccentre)
   }
end
