procedure endplot ()

string comment { "PONGO: Final Viewport", 
                 prompt = "Comment for AGI database entry" }

begin
   if ( defpac("pongo") ) {
         endpongo (comment=comment, mode="h")
    } else {
       print "Package PONGO is not available"
    }
end
