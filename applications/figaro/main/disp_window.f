      subroutine disp_window(left,right,line,sdata,sdens,in)
*+
* Name:
*    DISP_WINDOW

* Invocation:
*    CALL DISP_WINDOW(LEFT,RIGHT,LINE,SDATA,SDENS,IN)

* Purpose:
*       Display the current line profile window

* Description:
*       Display the current line profile window

* Arguments (given):-
*      SDATA(IN) = REAL ARRAY (Given)
*        X array
*      SDENS(IN) = REAL ARRAY (Given)
*        Intensity data to be displayed
*      IN = INTEGER (Given)
*        Dimensions of above
*      LEFT(LINE) = REAL ARRAY (Given)
*        Start of display in X
*      RIGHT(LINE) = REAL ARRAY (Given)
*        End of display in X
* Global variables:
*      TITLE = CHARACTER*60 (Given)
*        Title of plot (include file arc_dims)
*      XUNITS = CHARACTER*30 (Given)
*        Units of X array (include file arc_dims)
*      LEGEND(3) = CHARACTER*60 ARRAY (Given)
*        Legends fo plot (legend(3) not used here) (include file arc_dims)

* History:
* Re-written T.N.Wilkins Manchester
* PGPLOT version "       Cambridge 3/90
* Use LEFT/RIGHT arrays TNW "      3/9/91
*___________________________________________________________________
*-
      implicit none
      integer in,line
      real left(line),right(line),ymin,ymax
      real sdata(in)
      real sdens(in)
      integer rx2chn,is,ie
      integer status
      include 'arc_dims'
      include 'SAE_PAR'

      status = SAI__OK
      call gr_soft(status)
      is=rx2chn(sdata,in,left(line))
      ie=rx2chn(sdata,in,right(line))

* Draw axis

      call gr_range(sdens,is,ie,ymin,ymax,status)
      call gr_spen(1)
      call pgenv(left(line),right(line),ymin,ymax,0,0)
      call pglabel(xlabel//' '//xunits,' ',legend(1))

* Plot histogram-type line

      call pgbin(ie-is+1,sdata(is),sdens(is),.true.)

* Put labels at top of graph

      call pgmtext('T',3.0,0.5,0.5,title)
      call pgmtext('T',1.0,0.5,0.5,legend(2))
      end
