      subroutine multi_resid(sdata,sdens,m,resid,fit_total,redraw,
     :   simple,title,legend)
*+
* Name:
*    MULTI_RESID

* Invocation:
*    CALL MULTI_RESID(SDATA,SDENS,M,RESID,FIT_TOTAL,REDRAW,
*        SIMPLE,TITLE,LEGEND)

* Purpose:
*   Plot the residuals for a multiple gaussian fit.

* Description:
*   Plot the residuals for a multiple gaussian fit.

* Arguments:
*  SDATA(M) = REAL ARRAY (Given)
*        X array
*  SDENS(M) = REAL ARRAY (Given)
*        Intensity data
*  M = INTEGER (Given)
*        Dimension of above
*  FIT_TOTAL(M) = REAL ARRAY (Given)
*        Total value of summed fits
*  REDRAW = LOGICAL (Given)
*        If to redraw axis e.t.c.
*  SIMPLE = LOGICAL (Given)
*        If to draw a simple plot (no labels)
*  TITLE = CHARACTER*60 (Given)
*        Title of plot
*  LEGEND(3) = CHARACTER*60 ARRAY (Given)
*        Legends fo plot (legend(3) not used here)
*  RESID(M) = REAL ARRAY (Returned)
*        Residuals
* History:
*   Re-written T.N.Wilkins Manchester
*   Allow label suppression using IFLAB TNW 7/6/88
*   PGPLOT version TNW/Cambridge 3/90
*   Check for legend(2) being blank, TNW/Durham, 15/5/93
*-
*
      implicit none
      integer m
      real sdata(m),sdens(m)
      real fit_total(m),resid(m)
      logical redraw
      character*(*) title
      character*60 legend(2)

* local

      integer status
      logical simple,iflab
      integer lenleg1,chr_len,ilen
      real xw(2),yw(2),ymin,ymax
      character*80 leg1scr
      include 'gr_inc'

      call gr_spen(1)

* Subtract total of fit from data values to get residuals

      call gen_subaf(m,sdens,fit_total,resid)

      if(redraw) then

* Make diagram within current zone

        call gr_range(resid,1,m,ymin,ymax,status)
        call pgwindow(sdata(1),sdata(m),ymin,ymax)

* Draw axis and labels

        call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
        if((.not.simple).and.iflab()) then
          call pglabel(' ',' ',title)
          call greek_letters(legend(1)(:chr_len(legend(1))),leg1scr
     :       ,lenleg1,.false.)
          call pgmtext('T',1.0,0.5,0.5,leg1scr(:lenleg1))
          ilen = chr_len(legend(2))
          if(ilen.gt.0) call pgmtext('T',3.0,0.5,0.5,legend(2)(:ilen))
        end if
        call gr_spen(2)
        xw(1) = sdata(1)
        xw(2) = sdata(m)
        yw(1) = 0.0
        yw(2) = 0.0
        call pgline(2,xw,yw)
      end if
      call gr_spen(1)
      call pgbin(m,sdata,resid,.true.)
      end
