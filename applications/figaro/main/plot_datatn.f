      subroutine plot_datatn(n,x,y,labelx,labely,tittxt,titnum,w
     :  ,mode)
*+
* Name:
*    PLOT_DATATN

* Invocation:
*    CALL PLOT_DATATN(N,X,Y,LABELX,LABELY,TITTXT,TITNUM,W,MODE)

* Purpose:
*  To call plot_data with the title consiting of the text TITTXT
*  followed by the number TITNUM.

* Description:
*  To call plot_data with the title consiting of the text TITTXT
*  followed by the number TITNUM.
*
* Arguments:
*   N = INTEGER (Given)
*      Number of points
*   X(N) = DOUBLE PRECISION ARRAY (Given)
*      X array
*   Y(N) = DOUBLE PRECISION ARRAY (Given)
*      Y array
*   LABELX = CHARACTER*(*) (Given)
*      Label for X axis
*   LABELY = CHARACTER*(*) (Given)
*      Label for Y axis
*   TITTXT = CHARACTER*(*) (Given)
*      Text for title
*   TITNUM = INTEGER (Given)
*      Number for title
*   W(N) = DOUBLE PRECISION ARRAY (Given)
*      Weights
*   MODE = INTEGER (Given)
*      Mode of plotting

* Subroutines/functions referenced:
*      PLOT_DATA, CHR_FILL, CHR_PUTC, CHR_PUTI
*
* History:
*   T.N.Wilkins, Cambridge, 16-AUG-1990
*-
      implicit none
      integer n
      double precision x(n)
      double precision y(n)
      character*(*) labelx
      character*(*) labely
      character*(*) tittxt
      integer titnum
      double precision w(n)
      integer mode

*

      integer len1
      character*80 title

      len1 = 0
      call chr_fill(' ',title)
      call chr_putc(tittxt,title,len1)
      len1 = len1 + 1
      call chr_puti(titnum,title,len1)

      call plot_data(x,y,n,labelx,title(:len1),w,mode,labely)
      end
