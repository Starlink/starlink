procedure vp_bl ()

bool labels  {no,prompt="Reserve space for labels"}

begin
   bool reserve = no
   reserve = labels
   if ( defpac("pongo") ) {
      if ( reserve ) { 
         viewport (action="ndc", xvpmin=0.0417, xvpmax=0.459, 
                   yvpmin=0.1, yvpmax=0.4)
      } else {
         viewport (action="ndc", xvpmin=0.0417, xvpmax=0.459, 
                   yvpmin=0.05, yvpmax=0.45)
      }
   }
end
