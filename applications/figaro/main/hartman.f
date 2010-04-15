      subroutine hartman(wavep,delwave,line_count)
*+
* Name:
*    HARTMAN

* Invocation:
*    CALL HARTMAN(WAVEP,DELWAVE,LINE_COUNT)

* Purpose:
*   determine wavelength at cent(i) and difference between
*   wavep and wavec.

* Description:
*   determine wavelength at cent(i) and difference between
*   wavep and wavec.
*
* Arguments:
*    WAVEP(LINE_COUNT) = REAL ARRAY (Given)
*
*    DELWAVE(LINE_COUNT) = REAL ARRAY (Given)
*
*    LINE_COUNT = INTEGER (Given)
*
*-
      implicit none
      integer line_count
      real wavep(line_count)
      real delwave(line_count)
      integer status
      real zerox(2),zeroy(2),ymin,ymax
      character bss*2,bs*1
      data bss/'\\'/
      bs = bss(1:1)

      zeroy(1) = 0.0
      zeroy(2) = 0.0
*
*  plot delta lambda v lambda predicted
*
      call gr_spen(1)
      call gr_range(wavep,1,line_count,zerox(1),zerox(2),status)
      call gr_range(delwave,1,line_count,ymin,ymax,status)
      call pgwindow(zerox(1),zerox(2),ymin,ymax)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call gr_clab(bs//'gl (expected) ('//bs//'A)','Residuals ('//bs
     :     //'A)','Hartman Curve')
*
*  plot data
*
      call pgpoint(line_count,wavep,delwave,2)
      call pgline(2,zerox,zeroy)
      end
