      subroutine resid_v_ht(height,lincnt,delwave,height_er)
*+
* Name:
*    RESID_V_HT

* Invocation:
*    CALL RESID_V_HT(HEIGHT,LINCNT,DELWAVE,HEIGHT_ER)
* Purpose:
*   plot residuals v height

* Description:
*   plot residuals v height
*
* Arguments:
*    HEIGHT(LINCNT) = REAL ARRAY (Given)
*        Heights
*    LINCNT = INTEGER (Given)
*        Number of lines
*    DELWAVE(LINCNT) = REAL ARRAY (Given)
*        Residuals
*    HEIGHT_ER(LINCNT) = REAL ARRAY (Given)
*        Height errors
*
*-
      implicit none
      integer lincnt
      real height(lincnt),height_er(lincnt),delwave(lincnt)
* ----------------------------------------------------------------------
      real xmin,xmax,ymin,ymax
      integer status
      character*1 x
      data x/'x'/
      call gr_range(height,1,lincnt,xmin,xmax,status)
      call gr_range(delwave,1,lincnt,ymin,ymax,status)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call gr_clab('HEIGHT','RESIDUALS  (Ang)',
     :       'RESIDUALS v LINE STRENGTH')
      call pgpoint(lincnt,height,delwave,1)
*
*  X-error bar
*
      call gr_ebar(x,height,delwave,height_er,height_er,1.0,lincnt)
      end
