BEGIN {name="";}

{
if ( $2 == "Name:" ) {
  getline;
  name = $2;
  }

if ( $2 == "Keywords:" ) {
  getline;
  keys = substr($0,7,132);
  print name,keys;
  }

}
