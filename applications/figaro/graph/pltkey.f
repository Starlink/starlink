      subroutine pltkey(key,ifline,mark,text)
*+
* Name:
*    PLTKEY

* Invocation:
*    CALL PLTKEY(KEY,IFLINE,MARK,TEXT)
* Purpose:
*  To draw a key, to signify which line/marker identifies which data.

* Description:
*  To draw a key, to signify which line/marker identifies which data.

* Arguments:-
*  KEY = INTEGER (Given)
*        Key number (+ve for within grid area)
*  IFLINE = LOGICAL (Given)
*        If to plot a line (of current SGS pen)
*  MARK = INTEGER (Given)
*        Marker (ignored if 0)
*  TEXT = CHARACTER*(*) (Given)
*        Text to be plotted
* Notes:
*  N.B. SGS pen 1 is used for the marker and the text.

* Authors:
*  T.N.Wilkins, Manchester
*-
      implicit none
      integer key,mark,pen,len1,ich_tidy
      logical ifline
      character*(*) text
      real y,y1,yp(2),xp(2),xl1,xl2,yl1,yl2,xtmp

      y=1.0 - key*0.06
      call pgqwin(xl1,xl2,yl1,yl2)
      y1 = yl1 + (yl2-yl1)*y

* Plot line if required

      if(ifline) then
        xp(1) = xl1 + (xl2-xl1)*0.5
        xp(2) = xl1 + (xl2-xl1)*0.55
        yp(1) = y1
        yp(2) = y1
        call pgline(2,xp,yp)
      end if

* Set pen to one for rest of routine (reset just prior to exiting)


* enquiry in gr_spen

      pen = -1
      call gr_spen(pen)
      call gr_spen(1)

* Plot marker if required

      if(mark.ne.0) then
        xtmp = xl1 + (xl2-xl1)*0.57
        call pgpoint(1,xtmp,y1,mark)
      end if

* Plot text, first removing any non-printable characters

      len1 = ich_tidy(text)
      xtmp = xl1 + (xl2-xl1)*0.6
      call pgptext(xtmp,y1,0.0,0.0,text(:len1))
      call gr_spen(pen)
      end
