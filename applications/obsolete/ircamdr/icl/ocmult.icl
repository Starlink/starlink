{ PROCEDURE OCMULT : procedure to run CMULT rapi2d action
proc ocmult num_obsele code_image
  testval2 (num_obsele) (code_image)
  get_imagename (num_obsele) (code_image) (name_out)
  name_image = name_out
  obeyw rapi2d CMULT (name_image)
end proc

