      subroutine resid_v_width(width,delwave,width_er,lincnt)
*+
* Name:
*    RESID_V_WIDTH

* Invocation:
*    CALL RESID_V_WIDTH(WIDTH,DELWAVE,WIDTH_ER,LINCNT)

* Purpose:
*  Plot residuals against line width

* Description:
*  Plot residuals against line width
*
* Arguments:
*    WIDTH(LINCNT) = REAL ARRAY (Given)
*        Widths
*    DELWAVE(LINCNT) = REAL ARRAY (Given)
*        Residuals
*    WIDTH_ER(LINCNT) = REAL ARRAY (Given)
*        Width errors
*    LINCNT = INTEGER (Given)
*        Line count
*
*- -----------------------------------------------------------------
      implicit none
      integer lincnt
      real width(lincnt),delwave(lincnt),width_er(lincnt)
      real xmin,xmax,ymin,ymax
      character*1 x
      integer status
      character bss*2,bs*1
      data bss/'\\'/
      data x/'X'/
      bs = bss(1:1)
      call gr_range(width,1,lincnt,xmin,xmax,status)
      call gr_range(delwave,1,lincnt,ymin,ymax,status)
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call gr_clab('WIDTH '//bs//'(2078)','RESIDUALS  '//bs//'(2078)',
     :        'RESIDUALS v LINE WIDTH')
      call pgpoint(lincnt,width,delwave,1)
      call gr_ebar(x,width,delwave,width_er,width_er,1.0,lincnt)
      end
