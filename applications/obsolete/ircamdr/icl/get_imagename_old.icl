{ PROCEDURE GET_IMAGENAME : gets the user input and forms name
proc get_imagename num_obsele code_image name_out
  get plt2d filetype (filetype)
  if filetype <> 1
    if num_obsele < 0 or num_obsele > 200
      print "Which OBSERVATION (0 to enter FULL name) ?"
      asknum (num_obsele2) "Number \0\ : "
    else
      num_obsele2 = num_obsele
    end if
    if num_obsele2 = 0
      print "Give the FULL NAME of the IMAGE :"
      askname (name_out2) "Image Name \$LIRCAMDIR/logo\ : "
    else
      yn = undefined(code_image)
      if yn
        promptfor = 1
      else
        code_image2 = code_image
        if code_image2 < 0 or code_image2 > 4
          promptfor = 1
        else
          promptfor = 0
        end if
      end if
      if promptfor = 1
        print "Which IMAGE (Phase_A = 1, Phase_B = 2, KTC_A = 3, KTC_B = 4) ?"
        asknum (code_image2) "Image Code \1\ : "
      end if
      formname (num_obsele2) (code_image2) (name_out2)
    end if
  else
    if num_obsele < 0
      print "Which OBSERVATION (0 to enter FULL name) ?"
      asknum (num_obsele2) "Number \0\ : "
    else
      num_obsele2 = num_obsele
    end if
    if num_obsele2 = 0
      print "Give the FULL NAME of the IMAGE :"
      askname (name_out2) "Image Name \$LIRCAMDIR/logo\ : "
    else
      formname2 (num_obsele2) (name_out2)
    end if
  end if
  name_out = name_out2
end proc
