procedure font ( font )

int font {1,prompt="Font index", min=1, max=4}

begin
   if ( defpac("pongo") ) {
      change (cheight=INDEF, colour=INDEF, font=font,
              fillsty=INDEF, linesty=INDEF, linewid=INDEF, 
              angle=INDEF, separation=INDEF, phase=INDEF, textback=INDEF )
   }
end
