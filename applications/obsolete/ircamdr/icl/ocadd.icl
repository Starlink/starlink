{ PROCEDURE OCADD : procedure to run CADD rapi2d action
proc ocadd num_obsele code_image
  testval2 (num_obsele) (code_image)
  get_imagename (num_obsele) (code_image) (name_out)
  name_image = name_out
  obeyw rapi2d CADD (name_image)
end proc

