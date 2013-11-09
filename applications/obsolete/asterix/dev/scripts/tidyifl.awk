BEGIN {
  spaces="               ";
  indent=0;
  cword="";
  }

/^ +(ENDINTERFACE|endinterface|EndInterface|ENDPARAMETER|EndPARAMETER|EndParameter|Endparameter|endparameter)/{
  indent -= 2;
  print substr(spaces,1,indent)$1;
  getline;
  }

/(INTERFACE|Interface|interface|PARAMETER|Parameter|parameter)/{
  print substr(spaces,1,indent)$1,$2;
  indent += 2;
  getline;
  }

/#/{
  print $0;
  getline;
  }

  {
  fc = index($0,$1);
  print substr(spaces,1,indent)substr($0,fc,132);
  }
