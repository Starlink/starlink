{ PROCEDURE CCUT : plots a cut using cursor defined points on current image
proc ccut num_obsele code_image
  get plt2d cut_flag (cut_flag)
  if cut_flag <> 1
    print "You MUST run SETCUT before you can plot a CUT/SLICE ..."
    return
  end if
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    yn = undefined(num_obsele)
    yn2 = undefined(code_image)
    if yn
      num_obsele2 = -999
      code_image2 = -999
    else
      num_obsele2 = num_obsele
      if yn2
        code_image2 = -999
      else
        code_image2 = code_image
      end if
    end if
    get plt2d name_image (last_image)
    get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
    name_image = name_out
    print "Define START and END of CUT with CURSOR ..."
    send plt2d set cursor_cross 'YES'
    obeyw plt2d curcut (name_image)
    last_line = 3
    send plt2d set last_line (last_line)
    send plt2d set cursor_image (name_image)
  else
    print "You CANNOT use a CURSOR on this workstation"
    print "Try routine CUT instead"
  end if
end proc

