CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INTERACT.INC
C
C   alan penny                 ral              1990-06-09


      character*12 main_select  !Function chosen

      logical	opdisp		!Opened display?
      logical	gotimage	!Got an image?
      logical	displayed	!Displayed present image
      logical   work_load	!Put image into working array?

CX      logical	in_dum1		!Dummy for 4*n logicals


      common / scma_coma / opdisp, gotimage, displayed, work_load

      common / scma_comc / main_select


