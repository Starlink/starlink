{ PROCEDURE GETCM : gets calculated max,min from last image display
proc getcm
  get plt2d calculated_min (min)
  get plt2d calculated_max (max)
  print "Calculate MAX,MIN from last image display:"
  print "  MAX = " (max)
  print "  MIN = " (min)
end proc
