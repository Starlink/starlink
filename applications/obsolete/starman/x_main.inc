CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C X_MAIN.INC
C
C   alan penny                 ral              1990-06-09

      logical	x_opdisp	!Temporary storage of "Opened display?"
      logical	x_gotimage	!Temporary storage of "Got an image?"
      logical	x_displayed	!Temporary storage of "Displayed present 
				!                      image?"

      logical	x_dum1		!Dummy for 4*n logicals


      common / scma_comb / x_opdisp, x_gotimage, x_displayed, x_dum1



