{ PROCEDURE OCDIV : procedure to run CDIV rapi2d action
proc ocdiv num_obsele code_image
  testval2 (num_obsele) (code_image)
  get_imagename (num_obsele) (code_image) (name_out)
  name_image = name_out
  obeyw rapi2d CDIV (name_image)
end proc

