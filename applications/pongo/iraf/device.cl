procedure device ( device )

 string device  { "xw", prompt = "Name of display device (help to get list)" }
 string label   { " ", prompt = "AGI picture label to search for" }
 string action  { "C", prompt = "AGI database action on entry (B,C or L)" }
 bool   clear   { yes, prompt = "Clear the display on entry?" }
 bool   overlay { no, prompt = "Overlay the AGI picture on entry?" }

begin
   string dev 
   dev = device
   if ( defpac("pongo") ) {
      if ( dev == "help" ) { 
         inquire (devices=yes)
      } else {
         begpongo (device=dev, label=label, action=action, clear=clear, overlay=overlay)
      }
    } else {
       print "Package PONGO is not available"
    }
end
