{ PROCEDURE SETPOLCOL: set the current settings of the colours for polax
proc setpolcol
  get plt2d pol_coltype (st1)
  compare (st1) "SAME" (value1)
  if value1 = 0
    print "Enter a COLOUR for FIRST vector range :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_col1 (st1)
    print "Enter a COLOUR for SECOND vector range :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_col2 (st1)
    print "Enter a COLOUR for THIRD vector range :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_col3 (st1)
    print "Enter a COLOUR for FOURTH vector range :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_col4 (st1)
    print "Enter a COLOUR for FIFTH vector range :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_col5 (st1)
  else
    print "Enter a COLOUR for the vector plot VECTORS :"
    askchar (st1) "Vectors (W,R,G,B,Y,P,C,S,N) ? \W\ : "
    send plt2d set pol_colvec (st1)
  end if
  print "Enter a COLOUR for the vector plot BORDER :"
  askchar (st1) "Border (W,R,G,B,Y,P,C,S,N) ? \W\ : "
  send plt2d set pol_colaxe (st1)
  print "Enter a COLOUR for the vector plot TICKS :"
  askchar (st1) "Ticks (W,R,G,B,Y,P,C,S,N) ? \W\: :"
  send plt2d set pol_coltic (st1)
  print "Enter a COLOUR for the vector plot NUMBERS :"
  askchar (st1) "Numbers (W,R,G,B,Y,P,C,S,N) ? \W\ : "
  send plt2d set pol_colnum (st1)
  print "Enter a COLOUR for the vector plot ANNOTATION :"
  askchar (st1) "Annotation (W,R,G,B,Y,P,C,S,N) ? \W\ : "
  send plt2d set pol_colann (st1)
end proc

