procedure rtodeg (column)

string column {"X", prompt="Data column to convert"}

begin
   string par
   char c
   string col
   col=column
   if ( defpac("pongo") ) {
      par= col//"/1.745329251994329e-02"
      if ( col == "X" || col =="x" ) {
         ccmath (x=par)
      } else if ( col == "Y" || col == "y" ) {
         ccmath (y=par)
      } else if ( col == "Z" || col == "z" ) {
            ccmath (z=par)
      } else if ( col == "EX" || col == "ex" ) {
         ccmath (ex=par)
      } else if ( col == "EY" || col == "ey" ) {
         ccmath (ey=par)
      } else {
         error (1, "Unknown column, must be one of: X, Y, Z, EX or EY")
      }
   }
end
