procedure mtext ( xpos, ypos, angle, justification, text )

real xpos {0.0,prompt="X position"}
real ypos {0.0,prompt="Y position"}
real angle {0.0,prompt="Angle of text"}
real justification {0.0,prompt="Text justification", min=0.0, max=1.0}
string text {"",prompt="Text"}

begin
   if ( defpac("pongo")) {
      wtext (action="p", xpos=xpos, ypos=ypos, text=text, angle=angle, justification=justification)
   }
end
