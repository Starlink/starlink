      subroutine addfit(i,j,results,line)
*+
* Name:
*    ADDFIT

* Invocation:
*    CALL ADDFIT(I,J,RESULTS,LINE)

* Purpose:
*    To plot the fit to a line profile over the line profile.

* Description:
*    To plot the fit to a line profile over the line profile. It is
*   assumed that the current SGS zone has the same limits as the data.
*   This is for use with arrays of profiles, and checks that no profiles
*   have been co-added for the fit.
*
* Subroutines/functions referenced:
C     CNF_PVAL       : Full pointer to dynamically allocated memory
*     DECODE_STATUS  : Decode fit status word
*     LINE_PLOT_SUB  : Plot fit onto profile
*
* Arguments:
*    I = INTEGER (Given)
*        Spatial X position of fit
*    J = INTEGER (Given)
*           "    Y    "     "   "
*    RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results block
*    LINE = INTEGER (Given)
*        Current line
*
* Author:
*    T.N.Wilkins Manchester 21/7/88
* History:
*    TNW 10/11/88, IFSGS set directly.
*    TNW 8/7/91 Changes for new results structure
*
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'opt_cmn'
      include 'gr_inc'
*-
      integer i,j,line
      integer status
      include 'status_inc'
      real results(mxpars,nyp,nxp,spdim2)
      integer ninfit,get_parnum
      integer stat
      real fit_parms(MAX_PARMS),value

      status = SAI__OK

* Get results into fit_parms array

      ninfit = 1
      if(spdim2.gt.1) then
        value = results(get_parnum('Pts_in_Fit'),line,i,j)
        if(value.ne.VAL__BADR) then
          ninfit = nint(value)
        end if
      end if
      if(ninfit.eq.1) then
        call getres(results,line,i,j,fit_parms,deccntr,value,
     :              %VAL(CNF_PVAL(staptr)),stat)
      else

*   Fit is to more than 1 point, so cannot be plotted here

        deccntr(FIT_STAT) = 10
      end if

      if((stat.eq.0).and.((deccntr(FIT_STAT).eq.1).or.
     :            (deccntr(FIT_STAT).eq.2))) then

*            Plot profile with fit

        call line_plot_sub(fit_parms,%VAL(CNF_PVAL(d_xptr)),0.0,
     :                     %VAL(CNF_PVAL(d_tlptr)),
     :                     %VAL(CNF_PVAL(d_trptr)),line,wavdim,
     :                     deccntr,.false.,0,0,max_parms,status)
        call gr_spen(1)
* fit a success or nag error

      end if
      end
