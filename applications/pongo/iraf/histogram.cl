procedure histogram (binmin, binmax, nbin)

real binmin     {0,prompt="Lower limit"}
real binmax     {1,prompt="Upper limit"}
int nbin        {10,prompt="Number of bins"}
bool autoscale  {yes,prompt="Autoscale plot limits"}
bool fill       {no,prompt="Fill histograms"}

begin
   if ( defpac("pongo") ) {
      plothist (action="h", binmin=binmin, binmax=binmax, nbin=nbin,
                autoscale=autoscale, fill=fill)
   }
end
