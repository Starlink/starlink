procedure lweight ( linewid )

int linewid {1.0, prompt="Relative line width"}

begin
   if ( defpac("pongo") ) {
      change (cheight=INDEF, colour=INDEF, font=INDEF,
              fillsty=INDEF, linesty=INDEF, linewid=linewid, 
              angle=INDEF, separation=INDEF, phase=INDEF, textback=INDEF )
   }
end
