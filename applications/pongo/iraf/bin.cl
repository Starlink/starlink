procedure bin ()

   bool centre {no, prompt="XCOL values are bin centre"}

begin
   if ( defpac("pongo") ) {
      plothist (action="b", centre=centre)
   }
end
