{ PROCEDURE SEE : calculates PSF and FWHM for stellar image
proc see num_obsele code_image
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
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
    name_out = " "
    get_imagename (num_obsele2) (code_image2) (name_out) (name_image)
    name_image = name_out
    last_plot = 2
    send plt2d set last_plot (last_plot)
    send plt2d set cursor_image (name_image)
    send plt2d set name_image (name_image)
    print "NSIGMA plot of " (name_image) " on " (worknam)
    obeyw plt2d nsigma (name_image)
    send plt2d set cursor_cross 'YES'
    get plt2d platscal (platscal)
    if platscal = 1.0
      print "Enter pixel scale in arcsec/pixel : "
      asknum (platscal) "Pixel Scale \0.286\ ? "
      send plt2d set platscal (platscal)
      send plt2d set arcsec_pixel (platscal)
    end if
    fclose_b
    delfile starpos.dat
    create bfile "starpos.dat"
    loop for numstars = 1 to 1000
      print "Select position of STAR using cursor"
      print "  click off image to terminate star selection"
      obeyw plt2d cursor
      get plt2d x_cur_pixel (xobj)
      get plt2d y_cur_pixel (yobj)
      get plt2d cursor_value (val)
      if val <> -9999
        print "Star position (pixels) = " (xobj) (yobj) " Value = " (val)
{        st1 = '""'&(xobj)&" "&(yobj)&'""'
        st1 = (xobj)&" "&(yobj)
        write bfile (st1)
      else
        print "Star selection terminated"
        break
      end if
    end loop
    fclose_b
    name_image = "@"&name_image
    psf in=(name_image) cofile=starpos.dat device=GKS_3800 clear=yes ~
      fwhm=(seeing) \
    send plt2d set gun_spec 'COLOUR'
    send plt2d set pen_colour "W"
    obeyw plt2d setcol 1
    afwhm = seeing*platscal
    print "PSF FWHM = " (afwhm) "arcsec"
    get plt2d last_lut (lastlut)
    print " "
    askname (dumm) "Hit RETURN to continue ... "
    obeyw plt2d coltab (lastlut)
{    delfile starpos.dat
  else
    print "Cannot display images on workstation " (worknam)
  end if
end proc
