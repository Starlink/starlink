proc psf_test
  loop for jj = 1 to 100
    print "Loop number " (jj)
    delfile $ADAM_USER/kappa_pm.sdf
    seeim = "@im_50dfzm"
    psf in=(seeim) cofile=starpos.dat device=! ~
        fwhm=(seeing) axisr=(rat) orient=(ori) gamma=(gam) \
  end loop
end proc
