      subroutine splinexy_plot(in,sdata,sdens_old,sdens_new,
     :     label,xlabel,ylabel,ACTION,xmin,xmax,ymin,ymax,nplots)
*+
* Name:
*    SPLINEXY_PLOT

* Invocation:
*    CALL SPLINEXY_PLOT(IN,SDATA,SDENS_OLD,SDENS_NEW,
*               LABEL,XLABEL,YLABEL,ACTION,XMIN,XMAX,YMIN,YMAX,NPLOTS)


* Purpose:
*  Plotting routine

* Description:
*   This plots the data in arrays sdata (X positions) and
*   sdens (Y positions).
*   For the Y axis Scaling can be either automatic or within user
*    specified values
*  Arguments:
*   IN = INTEGER (Given)
*        dimension of sdata and sdens.
*   SDATA(IN) = REAL ARRAY (Given)
*        X data.
*   SDENS(IN) = REAL ARRAY (Given)
*        Y data.
*   LABEL = CHARACTER*(*) (Given)
*        Plot label
*   XLABEL = CHARACTER*(*) (Given)
*        X label
*   YLABEL = CHARACTER*(*) (Given)
*        Y label
*   ACTION        : which plot to use
*  Subroutines referenced:
*
*  Altered TNW 15/12/88 To allow longer labels if required
*  PGPLOT version, TNW/Cambridge 3/90
*-
      implicit none
      include 'SAE_PAR'
      integer RIGHT,TOP,BOTTOM
      Parameter ( RIGHT = 3)
      Parameter ( TOP   = 1)
      Parameter  ( BOTTOM = 2 )
      integer ACTION
      integer in,nplots
      real sdata(in),sdens_new(in),sdens_old(in)
      real ymin(nplots),ymax(nplots),xmin(nplots),xmax(nplots)
      integer status
*
*  Labels for plot.
*
      character*(*) label
      character*(*) xlabel
      character*(*) ylabel

* local
      real ymin_new,ymax_new,ymax_old,ymin_old

* If the ACTION = RIGHT then we are ploting the trams
* then we plot the old data inside the tram and superimpose
* the New data as well
      status = SAI__OK

      IF (ACTION.eq. RIGHT) then

        call gr_range(sdens_old,1,in,ymin_old,ymax_old,status)
        call gr_range(sdens_new,1,in,ymin_new,ymax_new,status)
        call gr_range(sdata,1,in,xmin(RIGHT),xmax(RIGHT),status)
        ymin(RIGHT) = min(ymin_old,ymin_new)
        ymax(RIGHT) = max(ymax_old,ymax_new)

* set the view port in normalized coordinates

        call pgvport(0.75,0.95,0.1,0.95)
        call pgwindow(xmin(RIGHT),xmax(RIGHT),ymin(RIGHT)
     :                                       ,ymax(RIGHT))

* clear the RIGHT plot each time

        call gr_zclear(0.75,0.95,0.1,0.95)

* and do the plotting of the old and new data inside the trams

        call pgbox('BTCN',0.0,0,'BCNST',0.0,0)
        call pglabel(xlabel,ylabel,label)
        call pgsls(2)
        call pgbin(in,sdata,sdens_old,.true.)
        call pgsls(1)
        call pgbin(in,sdata,sdens_new,.true.)

* now plot the current tram limits on the BOTTOM plot as well
* to keep user informed of were we are
        call pgvport(0.1,0.7,0.1,0.45)
        call pgwindow(xmin(BOTTOM),xmax(BOTTOM),ymin(BOTTOM)
     :                                         ,ymax(BOTTOM))

* Left tram
        call pgmove(xmin(RIGHT),ymin(BOTTOM))
        call pgdraw(xmin(RIGHT),ymax(BOTTOM))
* right tram
        call pgmove(xmax(RIGHT),ymin(BOTTOM))
        call pgdraw(xmax(RIGHT),ymax(BOTTOM))
      end if
* we are plotting the final data with all TRAMS replaced
* by there new values

      IF (ACTION.eq. TOP) then
        call gr_range(sdens_new,1,in,ymin(TOP),ymax(TOP),status)
        call gr_range(sdata,1,in,xmin(TOP),xmax(TOP),status)

* set the view port in normalized coordinates
        call pgvport(0.1,0.7,0.6,0.95)
        call pgwindow(xmin(TOP),xmax(TOP),ymin(TOP),ymax(TOP))
        call pgbox('BTCN',0.0,0,'BCNST',0.0,0)
        call pglabel(xlabel,ylabel,label)
        call pgbin(in,sdata,sdens_new,.true.)
      end if
* we are plotting the orignal data

      IF (ACTION.eq. BOTTOM) then
        call gr_range(sdens_old,1,in,ymin(BOTTOM),ymax(BOTTOM)
     :                                                 ,status)

* set the view port in normalized coordinates
        call pgvport(0.1,0.7,0.1,0.45)
        call pgwindow(xmin(BOTTOM),xmax(BOTTOM),ymin(BOTTOM)
     :                                           ,ymax(BOTTOM))
        call pgbox('BTCN',0.0,0,'BCNST',0.0,0)
        call pglabel(xlabel,ylabel,label)
        call pgbin(in,sdata,sdens_old,.true.)
      end if
      end
