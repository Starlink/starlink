      subroutine window_width(ystart,yend,nwindow,nbls,set)
*+
* Name:
*    WINDOW_WIDTH

* Invocation:
*    CALL WINDOW_WIDTH(YSTART,YEND,NWINDOW,NBLS,SET)

* Purpose:
*   Get width of windows to use for blocking

* Description:
*   Set the width of the xsect window to be used in extracting data for
*   line profile fitting. Also calculate how many blocks there are.
*
* Arguments:
*    YSTART = INTEGER (Given and returned)
*        xsect limit
*    YEND = INTEGER (Given and returned)
*        xsect limit
*    SET = LOGICAL (Given and returned)
*        If to set blocking.
*    NWINDOW = INTEGER (Returned)
*        number of xsects in the window
*    NBLS = INTEGER (Returned)
*        number of blocks
*
*- --------------------------------------------------------------
      implicit none
      integer ystart
      integer yend
      logical set
      integer nwindow
      integer nbls
* local
      real x_window

* window width

      real value
      integer irem,len1
      integer iyr
      character*24 chars
      integer status

      if(set) then
        if(ystart.eq.yend) then
          nwindow = 1
        else
          x_window = real(yend-ystart+1)
          call par_rdval('yblock',1.0,x_window,1.0,'X-Sects',value)
          call par_cnpar('yblock')
          nwindow=nint(value)
        end if
      else
        nwindow = min(nwindow,(yend-ystart+1))
      end if
*
* Calculate how many blocks there are
*
      iyr=(yend-ystart+1)
      nbls=iyr/nwindow
      irem=iyr-nbls*nwindow
      if(irem.gt.0) then
        nbls=nbls+1
      end if
      call chr_fill(' ',chars)
      len1 = 0
      call chr_putc('Number of blocks = ',chars,len1)
      call chr_puti(nbls,chars,len1)
      call par_wruser(chars(:len1),status)
      end
