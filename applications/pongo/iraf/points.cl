procedure points ( symbol ) 

int symbol {1,prompt="PGPLOT marker number"}
string projection {"NONE",prompt="Astrometric projection"}
string racentre {"0",prompt="Projection centre (RA)"}
string deccentre {"0",prompt="Projection centre (DEC)"}

begin
   if ( defpac("pongo") ) {
     gpoints (action="p", projection=projection, racentre=racentre,
              deccentre=deccentre, symbol=symbol)
   }
end
