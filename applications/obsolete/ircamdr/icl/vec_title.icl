{ PROCEDURE VEC_TITLE : sets title for a vector plot
proc vec_title titst
  yn = undefined(titst)
  if yn
    print "Enter the title for the top of the POLARIZATION VECTOR PLOT "
    askchar (tit) "Title \JUNK\ : "
  else
    tit = titst
  end if
  send plt2d set poltitle (tit)
end proc

