      subroutine new_arc(ifcomb,status)
*+
* Name:
*    NEW_ARC

* Invocation:
*    CALL NEW_ARC(IFCOMB,STATUS)

* Purpose:
*   Create results structure

* Description:
*   If a new spectrum is to be analysed then create the required data
*   structures and fill in their initial values and map them.  .RESULTS
*   structure for storage of RESULTS
*
* Arguments:
*    IFCOMB = LOGICAL (Given)
*        If comb structure to be produced
*    STATUS = INTEGER (Given and returned)
*        Error status
* History:
*   DSA version 22-23/8/88 T.N.Wilkins Manchester
*   Allow for variable number of parameters in LONGSLIT, T.N.Wilkins
*   Manchester, 13/1/89
*   STATUS added to argument list, TNW/CAVAD 25/9/89
*   Minor changes to flow, TNW 11/6/92
*
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
*-
      logical ifcomb
      integer status

* local

      integer pstat
      logical par_quest
      real value
      character ftype*4
      integer max_gauss

*  ---------------------------------------------------------------------
      if(ifcomb) then
        ftype = 'COMB'
      else
        ftype = 'RECT'
      end if
      if(status.ne.SAI__OK) return

* see if a .RESULTS strucure exists in DATA

      call accres('data','results','fi',2,0,' ',status)

* a new  analysis is being performed
* if the .RESULTS structure already exists then it may either
* be junk , if so delete it and re-create, or it may be NEW
* was choosen by accident . If NEW was a mistake then return to
* main routine and try again

      if(status.eq.SAI__OK) then
        call par_wruser('RESULTS structure already exists',pstat)
        if(batch) then
          status = SAI__ERROR
          return
        end if

* Try to persaude the user to use REFINE, but if they really want to
* let them delete the current results structure

        refine = .not.par_quest(
     :         'Do you wish to Delete this and start again?',.false.)
        if(.not.refine) refine = .not.par_quest('Are you Sure?',.true.)
        if(refine) then
          call par_wruser('Will use REFINE mode',pstat)
          return
        else
          call accres(' ',' ','de',0,0,' ',status)
        end if
      end if
      status = SAI__OK

*  No particular upper limit on this-except because of hardware etc.
*  1000 chosen just to be a large number-a bit excessive!

      call par_rdval('maxlines',1.0,1000.0,real(nyp),'Integer'
     :            ,value)
      nyp = nint(value)

* If we in LONGSLIT, then mxpars will have been set to maxparms. This gives
* a large results structure, so the user may prefer a smaller array
* The storage is as follows (in storage positions):
*
*   Position  1
*   Base:     1
*   Skew      1
*   Cauchy    1
*   Density scale 1
*   AIC       1
* Sub-total  6
* For 3-D case:
*   Position in 2nd spatial dimension 1
*   Number of points in fit           1
* Sub-total   2
*   Per line: 3 (Centre, width, height)

      if(mxpars.eq.maxparms) then
        max_gauss = maxparms/6 - 1
        call par_rdval('maxgauss',1.0,real(max_gauss),5.0,' ',value)
        mxpars = nint(value)*3 + 6
      end if

* For 3-D case:

      if(spdim2.gt.1) mxpars = mxpars + 2

* Read structure definition file and create structure

      call crres(ftype,status)
      end
