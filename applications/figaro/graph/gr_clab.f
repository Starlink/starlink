      subroutine gr_clab(xlab,ylab,title)
*+
* Name:
*    GR_CLAB

* Invocation:
*    CALL GR_CLAB(XLAB,YLAB,TITLE)

* Purpose:
*  To put labels onto a plot using PGPLOT.

* Description:
*  To put labels onto a plot using PGPLOT. This performs the same
*  function as the PGPLOT routine PGLABEL, except that the labels are
*  put closer to the axis here-for use in confined spaces. Blank strings
*  are ignored.
*
* Arguments:
*   XLAB = CHARACTER*(*) (Given)
*      X label
*   YLAB = CHARACTER*(*) (Given)
*      Y label
*   TITLE = CHARACTER*(*) (Given)
*      Title of plot
* Subroutines/functions referenced:
*      PGMTEXT          : Put text at given position relative to axis
*      CHR_LEN = INTEGER (Workspace)
*        Get non-blank length of string

* Authors:
*   T.N.Wilkins, Cambridge, 20-MAR-1990
*-
      implicit none
      character*(*) xlab
      character*(*) ylab
      character*(*) title
      integer chr_len
*
      if(chr_len(xlab).gt.0) call pgmtext('B',2.0,0.5,0.5,xlab)
      if(chr_len(ylab).gt.0) call pgmtext('L',1.8,0.5,0.5,ylab)
      if(chr_len(title).gt.0) call pgmtext('T',0.8,0.5,0.5,title)

      end
