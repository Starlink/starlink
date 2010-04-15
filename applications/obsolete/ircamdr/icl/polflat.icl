{ PROCEDURE POLFLAT : flat fields four polarization images
proc polflat
  askname (obj0)  "Enter name of    0 deg OBJECT image ? "
  askname (sky0)  "Enter name of    0 deg SKY    image ? "
  askname (obj45) "Enter name of   45 deg OBJECT image ? "
  askname (sky45) "Enter name of   45 deg SKY    image ? "
  askname (obj22) "Enter name of 22.5 deg OBJECT image ? "
  askname (sky22) "Enter name of 22.5 deg SKY    image ? "
  askname (obj67) "Enter name of 67.5 deg OBJECT image ? "
  askname (sky67) "Enter name of 67.5 deg SKY    image ? "
  sky0n = sky0&"_norm"
  sky45n = sky45&"_norm"
  sky22n = sky22&"_norm"
  sky67n = sky67&"_norm"
{ 0 deg
  print "Object image " (obj0)
  print "  calculating normalization factor for flat-field " (sky0)
  print "  N.B. Normalization area is the whole image ..."
  obeyw rapi2d STATS (sky0) 1 1 10000 10000 NO \
  getpar glob stats_median (medscal)
  print "  median of normalization area is " (medscal)
  print "  normalizing flatfield image " (sky0)
  obeyw rapi2d CDIV (sky0) (medscal) (sky0n)
  print "  flat-fielding image " (obj0) " with " (sky0n)
  out = obj0&"f"
  obeyw rapi2d DIV (obj0) (sky0n) (out)
  print " flat-fielded image output to : " (out)
  d1 = sky0n&".sdf"
  delfile (d1)
{ 45 deg
  print "Object image " (obj45)
  print "  calculating normalization factor for flat-field " (sky45)
  print "  N.B. Normalization area is the whole image ..."
  obeyw rapi2d STATS (sky45) 1 1 10000 10000 NO \
  getpar glob stats_median (medscal)
  print "  median of normalization area is " (medscal)
  print "  normalizing flatfield image " (sky45)
  obeyw rapi2d CDIV (sky45) (medscal) (sky45n)
  print "  flat-fielding image " (obj45) " with " (sky45n)
  out = obj45&"f"
  obeyw rapi2d DIV (obj45) (sky45n) (out)
  print " flat-fielded image output to : " (out)
  d1 = sky45n&".sdf"
  delfile (d1)
{ 22.5 deg
  print "Object image " (obj22)
  print "  calculating normalization factor for flat-field " (sky22)
  print "  N.B. Normalization area is the whole image ..."
  obeyw rapi2d STATS (sky22) 1 1 10000 10000 NO \
  getpar glob stats_median (medscal)
  print "  median of normalization area is " (medscal)
  print "  normalizing flatfield image " (sky22)
  obeyw rapi2d CDIV (sky22) (medscal) (sky22n)
  print "  flat-fielding image " (obj22) " with " (sky22n)
  out = obj22&"f"
  obeyw rapi2d DIV (obj22) (sky22n) (out)
  print " flat-fielded image output to : " (out)
  d1 = sky22n&".sdf"
  delfile (d1)
{ 67.5 deg
  print "Object image " (obj67)
  print "  calculating normalization factor for flat-field " (sky67)
  print "  N.B. Normalization area is the whole image ..."
  obeyw rapi2d STATS (sky67) 1 1 10000 10000 NO \
  getpar glob stats_median (medscal)
  print "  median of normalization area is " (medscal)
  print "  normalizing flatfield image " (sky67)
  obeyw rapi2d CDIV (sky67) (medscal) (sky67n)
  print "  flat-fielding image " (obj67) " with " (sky67n)
  out = obj67&"f"
  obeyw rapi2d DIV (obj67) (sky67n) (out)
  print " flat-fielded image output to : " (out)
  d1 = sky67n&".sdf"
  delfile (d1)
end proc
