procedure mtext ( side, xpos, ypos, justification, text )

string side {"t",prompt="Side of viewport (t|b|l|r|lv|rv)"}
real xpos {0.0,prompt="X position"}
real ypos {0.0,prompt="Y position"}
real justification {0.0,prompt="Text justification", min=0.0, max=1.0}
string text {"",prompt="Text"}

begin
   if ( defpac("pongo")) {
      wtext (action="m", xpos=xpos, ypos=ypos, text=text, side=side, justification=justification)
   }
end
