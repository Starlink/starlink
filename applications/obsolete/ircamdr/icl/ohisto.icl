{ PROCEDURE OHISTO : procedure to run HISTO rapi2d action
proc ohisto num_obsele code_image
  testval2 (num_obsele) (code_image)
  get plt2d name_image (last_image)
  get_imagename (num_obsele) (code_image) (name_out) (last_image)
  name_image = name_out
  obeyw rapi2d HISTO (name_image)
end proc

