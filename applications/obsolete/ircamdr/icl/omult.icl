{ PROCEDURE OMULT : procedure to run MULT rapi2d action
proc omult num1 cod1 num2 cod2
  get plt2d filetype (filetype)
  if filetype = 1
    testval2 (num1) (cod1)
    get_imagename (num1) 1 (name_out)
    name_image = name_out
    get_imagename (cod1) 1 (name_out)
  else
    testval4 (num1) (cod1) (num2) (cod2)
    get_imagename (num1) (cod1) (name_out)
    name_image = name_out
    get_imagename (num2) (cod2) (name_out)
  end if
  obeyw rapi2d MULT (name_image) (name_out)
end proc
