procedure vp_tl ()

bool labels  {no,prompt="Reserve space for labels"}

begin
   bool reserve = no
   reserve = labels
   if ( defpac("pongo") ) {
      if ( reserve ) { 
         viewport (action="ndc", xvpmin=0.0417, 
                   xvpmax=0.459, yvpmin=0.6, yvpmax=0.9)
      } else {
         viewport (action="ndc", xvpmin=0.0417, 
                   xvpmax=0.459, yvpmin=0.55, yvpmax=0.95)
      }
   }
end
