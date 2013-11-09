BEGIN {
name=""; lang=""; nrtn=0;
nsub=0; isub=0; contin=0; adder=0; gotalg=0; gotexa=0; newl="\n";
  print "<html><head>";
  }

{
if ( NF == 0 )
  getline;

if ( $2 == "Name:" ) {
  getline;
  name = $2;
  nrtn++;
  }

if ( $2 == "Purpose:" ) {
  getline;
  purp = substr($0,7,132);
  if ( nrtn == 1 ) {
    print "<title>"name": "purp;
    print "</title></head>";
    print "<body>";
    }
  else {
    print "<hr>";
    }
    
  print "<h1>"name"</h1>";
  print "<blockquote>";
  print purp;
  print "</blockquote>";
  }

if ( $2 == "Invocation:" ) {
  getline;
  print "<h2>Invocation</h2>";
  print "<blockquote>"substr($0,2,80)"</blockquote>";
  }

if ( $2 == "References:" ) {
  getline;
  if ( $2 != "{routine_references}" ) {
    print "<h2>References</h2><ul>";
    while ( NF != 0 ) {
      hpos=index($0," : ");
      print "<li><a href=\""substr($0,hpos+3,99)"\">"substr($0,5,hpos-5),"</a>";
      getline;
      }
    print "</ul><p>";
    }
  }


if ( $2 == "Description:" ) {
  getline;
  print "<h2>Description</h2>";
  print "<blockquote>";
  if ( $2 == "{routine_description}" ) 
    print "No description of this routine has been documented";
  else {
    while ( NF != 0 ) {
      if ( NF == 1 )
        print "<p>";
      else
        print substr($0,7,80);
      getline
      }
    }
  print "</blockquote>";
  }

if ( $2 == "Algorithm:" ) {
  getline;
  if ( $2 != "{algorithm_description}..." ) {
    gotalg=1;
    while ( NF != 0 ) {
      getline
      }
    }
  }

if ( $2 == "Arguments:" ) {
  getline;
  print "<h2>Arguments</h2>";
  print "<ul>";
  while ( NF != 0 ) {
    print "<li>";
    line = $0;
    lc = length(line);
    if ( $NF == "(given)" ) {
      line = substr(line,1,lc-6)"<em>given</em>)";
      }
    else if ( $NF == "(returned)" ) {
      line = substr(line,1,lc-9)"<em>returned</em>)";
      }
    else if ( $NF == "returned)" ) {
      line = substr(line,1,lc-19)"<em>given and returned</em>)";
      }

    print substr(line,7,80)"<br>";
    getline;
    while ( ($3 != "=") && (NF != 0) ) {
      print substr($0,7,80);
      getline;
      }
    }
  print "</ul>";
  }

if ( $2 == "Accuracy:" ) {
  getline;
  if ( $2 != "{routine_accuracy}" ) {
    print "<h2>Accuracy</h2>";
    print "<blockquote>";
    while ( NF != 0 ) {
      if ( NF == 1 )
        print "<p>";
      else
        print substr($0,7,80);
      getline
      }
    print "</blockquote>";
    }
  }

if ( NF == 2 && $2 == "=" )
  {
  iisub=0;
  for( x=1;x<=nsub && ! iisub;x++)
    if ( subname[x] == $1 )
      iisub = x;
  if ( iisub ) 
    isub = iisub;
  else
    {
    nsub++;
    isub = nsub;
    subname[nsub]=$1;
    }
  subval[isub]= "";
  getline;
  }

if ( contin )
  {
  if (substr($0,length,1) == "\\")
    contin = 1;
  else
    contin = 0;
  slen = length(subval[isub]);
  if ( slen == 0 ) 
    if ( adder && contin ) 
      subval[isub]= $0 "EOL\\";
    else
      subval[isub]= $0;
  else
    {
    if ( adder ) 
      subval[isub]= substr(subval[isub],1,slen-1) "EOR\\" "\n" $0;
    else
      subval[isub]= substr(subval[isub],1,slen-1) "EOL\\" "\n" $0;
    adder = 0;
    }
  }
else if ( $2 == "=" )
  {
  iisub=0;
  for( x=1;x<=nsub && ! iisub;x++)
    if ( subname[x] == $1 )
      iisub = x;
  if ( iisub ) 
    isub = iisub;
  else
    {
    nsub++;
    isub = nsub;
    subname[nsub]=$1;
    }
  epos=index($0,"=")
  subval[isub]= substr($0,epos+2,132);
  if (substr($0,length,1) == "\\")
    contin = 1;
  else
    contin = 0;
  }
else if ( $2 == "+=" )
  {
  iisub=0;
  for( x=1;x<=nsub && ! iisub;x++)
    if ( subname[x] == $1 )
      iisub = x;
  if ( iisub ) 
    isub = iisub;
  else
    {
    nsub++;
    isub = nsub;
    subname[nsub]=$1;
    }
  epos=index($0,"+=")
  slen = length(subval[isub]);
  if ( slen != 0 ) 
    subval[isub]= substr(subval[isub],1,slen) "\\\n" substr($0,epos+3,132);
  if (substr($0,length,1) == "\\")
    contin = 1;
  else
    contin = 0;
  adder = 1;
  }
}

END {
  if ( gotexa ) {
    print "<a href=\"Examples/"fname".html\"><IMG ALT=\"EXAMPLES\" SRC=\"but_exam.xbm\"></a>";
    }
  if ( gotalg ) {
    print "<a href=\"Algorithms/"fname".html\"><IMG ALT=\"ALGORITHM\" SRC=\"but_algm.xbm\"></a>";
    }
  flen = length(FILENAME);
  print "<a href=\"Code/"substr(FILENAME,1,flen-2)".html#"name"\"><IMG ALT=\"CODE\" SRC=\"but_code.xbm\"></a>";
  print "</body></html>";
  }
