      subroutine delta_fit(chans,wavecn,nchans)
*+
* Name:
*    DELTA_FIT

* Invocation:
*    CALL DELTA_FIT(CHANS,WAVECN,NCHANS)

* Purpose:
*   Plot dispersion curve for fit

* Description:
*   Plot dispersion curve for fit

* Arguments (given):-
*  CHANS(NCHANS) = INTEGER ARRAY (Given)
*        Channels (units channel number)
*  WAVCN(NCHANS) = INTEGER ARRAY (Given)
*        Wavelengths of channels
*  NCHANS = INTEGER (Given)
*        Number of channels

* History:
* Version using NCAR T.N.Wilkins/Manchester
*-
      implicit none
      integer nchans,status
      real chans(nchans),wavecn(nchans),ymin,ymax
      character bss*2,bs*1
      data bss/'\\'/
      bs = bss(1:1)
*
      call gr_spen(1)
      call gr_range(wavecn,1,nchans,ymin,ymax,status)
      call pgwindow(chans(1),chans(nchans),ymin,ymax)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call gr_clab('Channel Number','Dispersion ('//bs//'A/channel)',
     :        'Dispersion Curve')
      call pgline(nchans,chans,wavecn)
      end
