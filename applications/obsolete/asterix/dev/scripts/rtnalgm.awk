BEGIN {
  print "<html><head>";
  print "<title>Algorithm of module "FILENAME"</title>";
  print "</head><body>";
  }

{
if ( $2 == "Name:" ) {
  getline;
  print "<h1>"$2" Algorithm</h1>";
  }

if ( $2 == "Algorithm:" ) {
  getline;
  while ( NF != 0 ) {
    if ( NF == 1 )
      print "<p>";
    else
      print substr($0,7,80);
    getline
    }
  }
}

END {
  print "</body></html>";
  }
