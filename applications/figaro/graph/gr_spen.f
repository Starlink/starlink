      subroutine gr_spen(pen)
*+
* Name:
*    GR_SPEN

* Invocation:
*    CALL GR_SPEN(PEN)

* Purpose:
*  To set a pen value.

* Description:
*  To set a pen value. This is a separate colour if possible, otherwise
*  a separate line style. Because the CANON laser printer driver says is
*  has 32 colours when is hasn't, I assume that any hardcopy device
*  claiming to have more than 20 colours is being optimistic (a pen
*  plotter might have 8 say)!
*
* Arguments:
*   PEN = INTEGER (Given)
*      Pen number
* History:
*   T.N.Wilkins, Cambridge, 12-MAR-1990
*-
      implicit none
      integer pen
      integer c1,c2,savepn,len1
      character*3 value
      save savepn
      data savepn/-20/

* enquiry

      if(pen.lt.0) then
        pen = savepn
      else

*   Inquire colour capability

        call pgqcol(c1,c2)

*   Not a pen plotter

        if(c2.gt.20) then
          call pgqinf('hardcopy',value,len1)
          if(value.eq.'YES') c2=-1
        end if

*   Set colour index and line style

        if(c2.ge.pen) then
          call pgsci(pen)
          call pgsls(1)
        else
          call pgsci(1)
          call pgsls(pen)
        end if
        savepn = pen
      end if
      end
