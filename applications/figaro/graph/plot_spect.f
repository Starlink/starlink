      subroutine plot_spect(in,x,y,label,xlabel,ylabel)
*+
* Name:
*    PLOT_SPECT

* Invocation:
*    CALL PLOT_SPECT(IN,X,Y,LABEL,XLABEL,YLABEL)

* Purpose:
*     This plots the data in arrays x and y. Scaling is automatic.

* Description:
*     This plots the data in arrays x and y. Scaling is automatic.

* Arguments:
*   IN = INTEGER (Given)
*        dimension of x and y.
*   X(IN) = REAL ARRAY (Given)
*        X data.
*   Y(IN) = REAL ARRAY (Given)
*        Y data.
*   LABEL = CHARACTER*(*) (Given)
*        Plot label
*   XLABEL = CHARACTER*(*) (Given)
*        X label
*   YLABEL = CHARACTER*(*) (Given)
*        Y label

* History:
*  Altered TNW 15/12/88 To allow longer labels if required
*  PGPLOT version, TNW/Cambridge 3/90
*-
      implicit none
      integer in,status
      real x(in),y(in)
      real ymin,ymax
      include 'SAE_PAR'
*
*  Labels for plot.
*
      character*(*) label
      character*(*) xlabel
      character*(*) ylabel
*
* Plot the full spectrum.
*
      status = SAI__OK
      call gr_range(y,1,in,ymin,ymax,status)
      if(status.eq.SAI__OK) then
        call pgenv(x(1),x(in),ymin,ymax,0,0)
        call pglabel(xlabel,ylabel,label)
        call pgbin(in,x,y,.true.)
      end if
      end
