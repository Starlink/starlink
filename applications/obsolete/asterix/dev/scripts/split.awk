BEGIN {nsub=0; isub=0; contin=0; adder=0; newl="\n";}
{
if ( NF == 0 )
  getline;

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
  subval[isub]= substr($0,epos+2,500);
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
    subval[isub]= substr(subval[isub],1,slen) "\\\n" substr($0,epos+3,500);
  if (substr($0,length,1) == "\\")
    contin = 1;
  else
    contin = 0;
  adder = 1;
  }
}
END {
  for(x=1;x<=nsub;x++)
    print "s%:"subname[x]":%"subval[x]"%";
  }
