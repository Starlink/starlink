procedure radiate ( x, y, np )

real x {0.0,prompt="X position"}
real y {0.0,prompt="Y position"}
int np {1,prompt="Number of positions to join"}

begin
   real xpos = 0.0
   real ypos = 0.0
   int n = 0
   xpos = x
   ypos = y 
   n = np

   if ( defpac("pongo") ) { 
      for ( i=1; i <= n; i += 1 ) { 
         getpoint (action="n", value=i)
         move (xpos=xpos, ypos=ypos)
         draw (xpos=pongo.getpoint.x, ypos=pongo.getpoint.y)
      }
   }
end
