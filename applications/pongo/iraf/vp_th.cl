procedure vp_th ()

bool labels  {no,prompt="Reserve space for labels"}

begin
   bool reserve = no
   reserve = labels
   if ( defpac("pongo") ) {
      if ( reserve ) {
         viewport (action="ndc", xvpmin=0.0833, xvpmax=0.917,
                   yvpmin=0.6, yvpmax=0.9)
      } else {
         viewport (action="ndc", xvpmin=0.0833, xvpmax=0.917,
                   yvpmin=0.55, yvpmax=0.95)
      }
   }
end
