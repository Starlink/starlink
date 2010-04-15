{ PROCEDURE FLAT2 : flat fields a pair of images.
proc flat2 f1 f2 no
  yn1 = undefined(f1)
  yn2 = undefined(f2)
  yn3 = undefined(no)
  if yn1
    print "Enter the name of 1st image "
    askname (flat1) "1st Image ? "
  else
    flat1 = f1
  end if
  if yn2
    print "Enter the name of 2nd image "
    askname (flat2) "2nd Image ? "
  else
    flat2 = f2
  end if
  flat1n = flat1&"_norm"
  flat2n = flat2&"_norm"
  if yn3
    print "Do you want the flatfield image normalized to unity ?"
    asklog (normy) "Normalize Flatfield (Yes or No) \Y\ ? "
  else
    normy = no
  end if
  if normy = 1
    print "Calculating the normalization factor for flatfield " (flat1)
    print "N.B. Normalization area is the whole image ..."
    obeyw rapi2d STATS (flat1) 1 1 10000 10000 NO \
    getpar glob stats_median (medscal)
    print "Median of normalization area is " (medscal)
    print "Normalizing flatfield image " (flat1) ": Output = " (flat1n)
    obeyw rapi2d CDIV (flat1) (medscal) (flat1n)
    print "Calculating the normalization factor for flatfield " (flat2)
    print "N.B. Normalization area is the whole image ..."
    obeyw rapi2d STATS (flat2) 1 1 10000 10000 NO \
    getpar glob stats_median (medscal)
    print "Median of normalization area is " (medscal)
    print "Normalizing flatfield image " (flat2) ": Output = " (flat2n)
    obeyw rapi2d CDIV (flat2) (medscal) (flat2n)
  else
    flat1n = flat1
    flat2n = flat2
  end if
  print "Flatfielding image " (flat1) " with " (flat2n)
  out = flat1 & "f"
  obeyw rapi2d DIV (flat1) (flat2n) (out)
  print "Flatfielded image output to : " (out)
  print "Flatfielding image " (flat2) " with " (flat1n)
  out = flat2 & "f"
  obeyw rapi2d DIV (flat2) (flat1n) (out)
  print "Flatfielded image output to : " (out)
  out2 = (out)&"_norm.sdf"
  d1 = flat1&"_norm.sdf"
  d2 = flat2&"_norm.sdf"
  delfile (d1)
  delfile (d2)
end proc
