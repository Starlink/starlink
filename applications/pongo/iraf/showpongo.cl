procedure showpongo () 
begin
if ( defpac("pongo") ) {
   print (" ")
   print ("PONGO setup:")
   print (" ")
   inquire (pgplot=yes, limits=yes, columns=no, devices=no, data=no, mode=h)
}
end
