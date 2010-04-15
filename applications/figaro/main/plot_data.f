      subroutine plot_data(x,y,npts,labelx,title,w,mode,labely)
*+
* Name:
*    PLOT_DATA

* Invocation:
*    CALL PLOT_DATA(X,Y,NPTS,LABELX,TITLE,W,MODE,LABELY)

* Purpose:
*   Plot data

* Description:
*   Produce a plot in Histogram (or dots for MODE=3) style using PGPLOT
*   for several different type of input data. Produced in Chebyshev
*   Polynomial fitting to a spectrum.
*     The plots are Labeled according to the user suppled Labels
*   The main purpose of this routine is to Kid PGPLOT into designing
*   the correct Y axis for us . For the weighted fits we want to
*   suppress the points which are given low weights. We do this simply
*   by taking the product of the Y and W arrays.
*   NOte that this works fine for the data normally encountered by
*   the routine , which will tend to be in COUNTS or ADU with values
*   >= 1e-6 which is the value used to omitt points in the fits.
*   The fudge will however fail for data with extreme values.
*
* Arguments:
*    X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        Input X axis data
*    Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        Input Y axis data
*    NPTS = INTEGER (Given)
*        Number of input [x,y,] points
*    LABELX = CHARACTER (Given)
*        X-axis LAbel
*    TITLE = CHARACTER (Given)
*        Title for PLOT
*    W(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        Weights array used in CHeby Fit
*    MODE = INTEGER (Given)
*        Type of plot
*            - 0 = Data (i.e. scaled by data)
*            - 1 = FIT (scaled by data * weights)
*            - 2 = Weights (Y maximum multiplied by 1.1)
*            - 3 = Points
*         (note that the above refer to the Y axis scaling, except for
*         3, the data is assumed to be in ascending order of X)
*    LABELY = CHARACTER (Given)
*        Y-axis LAbel
*
* History:
*  Made to get internal workspace, TNW/CAVAD 16/8/90
*-
* import
*
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer npts
      character*(*) labely
      character*(*) title
      character*(*) labelx
      double precision x(npts)
      double precision y(npts)
      double precision w(npts)
      integer mode

* Symbolic constants used to

      integer WEIGHT

* decide which type of plot

      integer FIT

* is currently required.

      integer POINTS
      parameter (FIT = 1)
      parameter (WEIGHT = 2)
      parameter (POINTS = 3)
*
* local
*
      real ymin,ymax,xmin,xmax
      integer status,cnv_fmtcnv,nbad
      integer ypptr,xpptr,ywptr,slot,slot2,slot3
*
* ----------------------------------------------------------------------
*
* Get Virtual memory

      status = SAI__OK
      call dsa_get_work_array(npts,'float',xpptr,slot,status)
      call dsa_get_work_array(npts,'float',ypptr,slot2,status)
      call dsa_get_work_array(npts,'float',ywptr,slot3,status)
      if(status.ne.SAI__OK) return

* fill in data arrays
* For FIT mode, tO design the graph we set up an array
* DYNAMIC_MEM(YWPTR) which has the product of the weights and the
* actual data. Since the weights for excluded points are normally
* 1E-6 this means that we reduce such points to values vary close to
* ZERO for most reasonable data sets.

      status = cnv_fmtcnv('double','float',x,%VAL(CNF_PVAL(xpptr)),npts,
     :                    nbad)
      status = cnv_fmtcnv('double','float',y,%VAL(CNF_PVAL(ypptr)),npts,
     :                    nbad)
      if(mode.ne.FIT) then
        call gr_range(%VAL(CNF_PVAL(ypptr)),1,npts,ymin,ymax,status)
        if(mode.eq.weight) ymax = ymax * 1.1
      else
        status = cnv_fmtcnv('double','float',w,%VAL(CNF_PVAL(ywptr)),
     :                      npts,nbad)
        call gen_multaf(npts,%VAL(CNF_PVAL(ypptr)),
     :                  %VAL(CNF_PVAL(ywptr)),%VAL(CNF_PVAL(ywptr)))
        call gr_range(%VAL(CNF_PVAL(ywptr)),1,npts,ymin,ymax,status)
      end if
      if(mode.eq.POINTS) then
        call gr_range(%VAL(CNF_PVAL(xpptr)),1,npts,xmin,xmax,status)
      else
        xmin = real(x(1))
        xmax = real(x(npts))
      end if

* set the SGS pen number to 1

      call gr_spen(1)
*
* design diagram
*
      call pgwindow(xmin,xmax,ymin,ymax)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)

* plot labels and plot data with a histogram, or points if mode=points

      if(mode.eq.POINTS) then
        call gr_clab(labelx,labely,title)
        call pgpoint(npts,%VAL(CNF_PVAL(xpptr)),%VAL(CNF_PVAL(ypptr)),1)
      else
        call pglabel(labelx,labely,title)
        call pgbin(npts,%VAL(CNF_PVAL(xpptr)),%VAL(CNF_PVAL(ypptr)),
     :              .true.)
      end if
      call dsa_free_workspace(slot3,status)
      call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)
      end
