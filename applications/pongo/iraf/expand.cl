procedure expand ( cheight )

real cheight {1.0,prompt="Relative height of characters"}

begin
   if ( defpac("pongo") ) {
      change (cheight=cheight, colour=INDEF, font=INDEF,
              fillsty=INDEF, linesty=INDEF, linewid=INDEF, 
              angle=INDEF, separation=INDEF, phase=INDEF, 
              textback=INDEF )
   }
end
