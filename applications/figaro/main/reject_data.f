      subroutine reject_data(x,npts,w,labelx,prompt1,prompt2,
     :   tram1,tram2,max_work)
*+
* Name:
*    REJECT_DATA

* Invocation:
*    CALL REJECT_DATA(X,NPTS,W,LABELX,PROMPT1,PROMPT2,
*        TRAM1,TRAM2,MAX_WORK)

* Purpose:
*    Exclude data from polynomial fitting.

* Description:
*   Exclude data from polynomial fitting by setting weights
*   to 1e-6. The regions to be rejcted or defined are selected
*   with the cursor on a plot of the data. If any data have been
*   rejected then a plot of Weights v position is made using NCAR
*   graphics.

* Arguments:
*    X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*          X axis data for plot
*    NPTS = INTEGER (Given)
*          Number of points in data (to be plotted)
*    LABELX = CHARACTER (Given)
*          Plot LAbel for X axis
*    PROMPT1 = CHARACTER (Given)
*          User prompt
*    PROMPT2 = CHARACTER (Given)
*          User prompt
*    MAX_WORK = INTEGER (Given)
*          Size of tram1 and tram2
*    W(NPTS) = DOUBLE PRECISION ARRAY (Given and returned)
*          Weights array generated
*    TRAM1(MAX_WORK) = REAL ARRAY (Workspace)
*          Workspace for cursor regions lower bound
*    TRAM2(MAX_WORK) = REAL ARRAY (Workspace)
*          Workspace for cursor regions upper bound
* Subroutines
*    PICK_REGIONS3, PLOT_DATA
* 27/2/89 DJA modified to get tram1 and tram2 as workspace from
* from higher order routine.
*-
      implicit none
      integer npts
      double precision x(npts)
      double precision w(npts)
      character*(*) labelx
      character*(*) prompt1
      character*(*) prompt2
      integer max_work
      real tram1(max_work)
      real tram2(max_work)
* local

* Composite User prompt

      character*72 prompt3

* do loop

      integer wind,i

* FIGARO status

      integer status

* true if data being rejected

      logical reject
      character*40 chars

* FIGARO question

      logical par_quest,qset

* number of regions rejected

      integer count

* Y axis LAbel

      character*20 labely

* Title for PLOt

      character*30 title

* symbolic constant

      integer WEIGHT_PLOT
      parameter (WEIGHT_PLOT =2)

* flag for type of plot

      integer plot_type
* ---------------------------------------------------------------------
      qset   =.true.
      count  = 0
      status = 0
      labely = 'Weight'
      title  = 'Weights v Position'
      prompt3='Exclude any '//prompt1//' from the fit?'

* ask if any data to be rejected

      reject = par_quest(prompt3,qset)
      do while(reject)

* prompt for regions to be rejected

        call par_wruser(prompt2,status)
        call par_wruser('to be rejected',status)
*
* Select regions to reject with the cursors.
*
        call pick_regions3(tram1,tram2,count,max_work)
        wind = 1

* loop over the number of regions defined with the cursor
* and set the weights arrayb to 1E-6 for every point in
* that region

        do i=1,count
          do wind=1,npts
            if ((x(wind).le.tram2(i)).and.
     :      (x(wind).ge.tram1(i))) then
              w(wind)=1d-6
            end if
          end do
        end do

* ask the user if any more points are to be rejected
        qset =.false.
        reject = par_quest(prompt3,qset)
      end do

* if points have been rejected then make a plot
* of the window function generated

      if (count.gt.0) then
        call gr_spen(1)

* plot the weights array v X

        plot_type = WEIGHT_PLOT
        call pgpage
        call plot_data(x,w,npts,labelx,title,w,plot_type,labely)

* free the workspace used for the plot and prompt the user to return

        call par_wruser('$Type <return> to Continue - ',status)
        call par_rduser(chars,status)
      end if
      end
