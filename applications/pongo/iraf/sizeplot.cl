procedure sizeplot ( ) 

int symbol {INDEF,prompt="PGPLOT marker number (INDEF for none)"}
real scale  {1.0,prompt="Scale factor for ZCOL values"}
string projection {"NONE",prompt="Astrometric projection"}
string racentre {"0",prompt="Projection centre (RA)"}
string deccentre {"0",prompt="Projection centre (DEC)"}

begin
   if ( defpac("pongo") ) {
      gpoints (action="s", projection=projection,
               racentre=racentre, deccentre=deccentre, 
               symbol=symbol, scale=scale)
   }
end
