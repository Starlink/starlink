procedure vport (xvpmin, xvpmax, yvpmin, yvpmax)

real xvpmin {0.0,prompt="Left hand side of viewport"}
real xvpmax {1.0,prompt="Right hand side of viewport"}
real yvpmin {0.0,prompt="Lower side of viewport"}
real yvpmax {1.0,prompt="Upper side of viewport"}

begin
   if ( defpac("pongo") ) {
      viewport (action="ndc", xvpmin=xvpmin, xvpmax=xvpmax, yvpmin=yvpmin, yvpmax=yvpmax)
   }
end
