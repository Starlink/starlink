      subroutine window_scan(line,xstart,xend,ystart,yend,line_name,
     :     nwindx,nwindy)
*+
* Name:
*    WINDOW_SCAN

* Invocation:
*    CALL WINDOW_SCAN(LINE,XSTART,XEND,YSTART,YEND,LINE_NAME,
*          NWINDX,NWINDY)

*
* Purpose:
*      The data contained between the current X-sect" LIMITS" is
*      divided into a series of WINDOWS of width NWINDOW. Each
*      window may be viewed.
*
* Description:
*      The data contained between the current X-sect" LIMITS" is
*      divided into a series of WINDOWS of width NWINDOW. Each
*      window may be viewed.
*
* Arguments:
*    LINE = INTEGER (Given)
*        current line
*    XSTART = INTEGER (Given)
*        start xsect for analysis
*    XEND = INTEGER (Given)
*        end xsect for analysis
*    YSTART = INTEGER (Given)
*        start in Y for analysis
*    YEND = INTEGER (Given)
*        end in Y for analysis
*    LINE_NAME(LINE) = CHARACTER*10 ARRAY (Given)
*        Line names
*    NWINDX = INTEGER (Given)
*        Current WINDOW width in X
*    NWINDY = INTEGER (Given)
*        Current WINDOW width in Y
* History:
*   TNW, IoA Cambridge, 5/6/92 Made to work for 3-d data
*- --------------------------------------------------------------
      implicit none
      integer line
      character*10 line_name(line)
      integer xstart
      integer xend
      integer ystart
      integer yend
      integer nwindx,nwindy
* local

* PAR routine values

      real value,value1
      integer iblocks,iblocke,iblock,get_block,istarty,iendy
      integer jblocks,jblocke,jblock
      integer istartx,iendx !p.s. The values passed into WINDPLOT don't matter
      logical samblk
      integer status

* enquire for starting window

      call dsa_axis_range('data',2,' ',.false.,value,value1,istartx,
     :            iendx,status)
      call canaxlim(2)
      iblocks = get_block(xstart,nwindx,istartx)
      iblocke = get_block(xstart,nwindx,iendx)
      if(ystart.eq.yend) then
        jblocks = 1
        jblocke = 1
      else
        call dsa_axis_range('data',3,' ',.false.,value,value1,istarty,
     :            iendy,status)
        call canaxlim(3)
        jblocks = get_block(ystart,nwindy,istarty)
        jblocke = get_block(ystart,nwindy,iendy)
      endif

      do jblock = jblocks, jblocke
        do iblock = iblocks, iblocke
          call windplot(line,xstart,xend,ystart,yend,line_name,nwindx,
     :         nwindy,iblock,jblock,.true.,samblk,istartx,iendx,
     :         istarty,iendy,'W I N D O W',status)
        end do
      end do
      call sla_wait(3.0)
      end
