{ PROCEDURE DAOFIND : plots crosses or circles on each star found
proc daofind
  print "Procedure to display positions of stars found by DAOPHOT"
  print " .. running icl fortran program daosymb ... "
  delfile daosymb.icl
  ! $LIRCAMDIR/daosymb
  defproc daosymb daosymb
  print " .. loading created icl procedure file daosymb.icl ..."
  load daosymb
  daosymb
end proc

