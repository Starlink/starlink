procedure ptinfo ( label ) 

string label {prompt="Label of interesting data point"}

begin
   if ( defpac("pongo") ) { 
      getpoint (action="c", value=label)
      print ("  X = " // pongo.getpoint.x // ", Y = " //pongo.getpoint.y)
   }
end
