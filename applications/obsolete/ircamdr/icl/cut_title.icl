{ PROCEDURE CUT_TITLE : sets title for a cut plot
proc cut_title st1
  yn = undefined(st1)
  if yn
    print "Enter the title for the top of the CUT PLOT "
    askchar (st1) "Title \JUNK\ : "
  end if
  send plt2d set cut_title (st1)
end proc

