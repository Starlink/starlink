BEGIN {
name=""; lang="";
nsub=0; isub=0; contin=0; adder=0; newl="\n";

  print "<html><head>";
  print "<title>Source of module "FILENAME"</title>";
  print "</head><body><pre>";
  }

{
if ( NF > 2 ) {
  if ( $1 == "SUBROUTINE" ) {
    nlen = length($2);
    if ( substr($2,nlen,1) == "(" ) nlen--;
    name = substr($2,1,nlen);
    ipos=index($0,$2);
    print "      "$1" <A name=\""name"\">"name"</a>"substr($0,ipos+nlen,99);
    }
  else if ( $2 == "FUNCTION" ) {
    nlen = length($3);
    if ( substr($3,nlen,1) == "(" ) nlen--;
    name = substr($3,1,nlen);
    ipos=index($0,$3);
    print "      "$1 $2" <A name=\""name"\">"name"</a>"substr($0,ipos+nlen,99);
    }
  else if ( $1 == "BLOCK" && $2 == "DATA" ) {
    nlen = length($3);
    name = substr($3,1,nlen);
    ipos=index($0,$3);
    print "      "$1 $2" <A name=\""name"\">"name"</a>"substr($0,ipos+nlen,99);
    }
  else
    print $0;
  }
else
  print $0;
}

END {
  print "</pre></body></html>";
  }
