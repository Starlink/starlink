procedure ltype ( linesty )

int linesty {prompt="Line style index", min=1, max=5}

begin
   if ( defpac("pongo") ) {
      change (cheight=INDEF, colour=INDEF, font=INDEF, fillsty=INDEF, 
              linesty=linesty, linewid=INDEF, angle=INDEF, separation=INDEF,
              phase=INDEF, textback=INDEF )
   }
end
