procedure mark ( xpos, ypos, symbol )

real xpos {0.0,prompt="X position"}
real ypos {0.0,prompt="Y position"}
int  symbol {1,prompt="PGPLOT marker number"}
string projection {"NONE",prompt="Astrometric projection"}
string racentre {"0",prompt="Projection centre  (RA)"}
string deccentre {"0",prompt="Projection centre (DEC)"}

begin
   if ( defpac("pongo") ) {
      prim (action="k", x=xpos, y=ypos, projection=projection,
            racentre=racentre, deccentre=deccentre, symbol=symbol)
   }
end
