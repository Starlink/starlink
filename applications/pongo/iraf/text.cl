procedure text (xpos, ypos, text)

real xpos {0.0,prompt="X position"}
real ypos {0.0,prompt="Y position"}
string text {"PONGO",prompt="Text string to plot"}

begin
   if ( defpac("pongo") ) {
      wtext (action="s", xpos=xpos, ypos=ypos, text=text)
   }
end
