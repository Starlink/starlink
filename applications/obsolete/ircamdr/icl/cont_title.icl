{ PROCEDURE CONT_TITLE : sets title for a contour plot
proc cont_title titst
  yn = undefined(titst)
  if yn
    print "Enter the title for the top of the CONTOUR PLOT "
    askchar (tit) "Title \JUNK\ : "
  else
    tit = titst
  end if
  send plt2d set contour_title (tit)
end proc

