{ PROCEDURE OSTATS : procedure to run STATS rapi2d action
proc ostats num_obsele code_image
  testval2 (num_obsele) (code_image)
  get plt2d name_image (last_image)
  get_imagename (num_obsele) (code_image) (name_out) (last_image)
  name_image = name_out
  obeyw rapi2d STATS (name_image)
end proc

