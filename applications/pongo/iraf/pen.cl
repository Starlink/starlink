procedure pen ( colour )

int colour {1,prompt="Colour index of required pen",min=0,max=255}

begin
   if ( defpac("pongo") ) { 
      change (colour=colour, cheight=INDEF, font=INDEF, fillsty=INDEF,
              linesty=INDEF, linewid=INDEF, angle=INDEF, separation=INDEF,
              phase=INDEF, textback=INDEF)
   }
end
