procedure fillsty (fillsty)

int fillsty {2,prompt="Fill style", min=1, max=4}
real angle  {45.0,prompt="Hatch angle"}
real separation {1.0,prompt="Separation of hatched lines"}
real phase  {0.0,prompt="Fraction of separation hatched lines are displaced"}

begin
   if ( defpac("pongo") ) {
      change (cheight=INDEF, colour=INDEF, font=INDEF,
              fillsty=fillsty, linesty=INDEF, linewid=INDEF, 
              angle=angle, separation=separation, phase=phase, 
              textback=INDEF )
   }
end
