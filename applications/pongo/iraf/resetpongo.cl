procedure resetpongo () 

begin
   if ( defpac("pongo") ) {

#   Clear all plotting attributes.
      unlearn world
      limits (0, 1, 0, 1)
      unlearn change
      change (cheight=1, mode=h)
      clear (screen=yes, data=yes, lablst=yes, agi=yes, erscale=1)
      viewport (action="standard")
      
#   X and Y options of BOXFRAME
      unlearn boxframe

#   Any projections back to default.
      setproj ("none", "0","0")

#   Inform the user that the action is complete.
      print (" ")
      print ("PONGO reset.")
      print (" ")
   }
end
