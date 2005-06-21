      subroutine opt_checkfit(fitpar,fiterr,res_store,minht,x,y,deccntr
     :     ,loop,bstaic,bestng,n,mstore,aic,fstat,status)
*+
* Name:
*    OPT_CHECKFIT

* Invocation:
*    CALL OPT_CHECKFIT(FITPAR,FITERR,RES_STORE,MINHT,X,Y,DECCNTR,LOOP,
*          BSTAIC,BESTNG,N,MSTORE,AIC,FSTAT,STATUS)

*
* Description:
*    To check whether a fit is to be accepted. This also controls the
*    selection of fits on the basis of AIC.
*
* Purpose:
*    To check whether a fit is to be accepted. This also controls the
*    selection of fits on the basis of AIC.
*
* Arguments:
*      FITPAR(*) = REAL ARRAY (Given)
*        Fit parameters
*      FITERR(*) = REAL ARRAY (Given)
*        Errors on fit parameters
*      RES_STORE(*) = REAL ARRAY (Given)
*        Results store
*      MINHT = REAL (Given)
*        Minimum value for height
*      X(M) = REAL ARRAY (Given)
*        X array being fitted to
*      Y(M) = REAL ARRAY (Given)
*        Y array being fitted to
*      DECCNTR = INTEGER (Given and returned)
*        Fit coding (model, type etc.)
*      LOOP = LOGICAL (Given and returned)
*        If to continue looping in FIT_LINE
*      BSTAIC = REAL (Given and returned)
*        Best value for AIC
*      BESTNG = INTEGER (Given and returned)
*        Number of components corresponding to BSTAIC
*      N = INTEGER (Given and returned)
*        Number of fit parameters
*      FSTAT = INTEGER (Given and returned)
*        Fit status
*      STATUS = INTEGER (Given and returned)
*        Error status
*      MSTORE = LOGICAL (Returned)
*        If to store current fit parameters
*      AIC = REAL (Returned)
*        AIC
*
* Global variables:
*      MAX_TIMES = INTEGER (Given)
*        Maximum number of guess/results sets which can be stored
*      MAX_CMP = INTEGER (Given)
*        Maximum number of components to fit
*      MPTS = INTEGER (Given)
*        Number of elements in arrays being fitted to
*      MINSIG = REAL (Given)
*        Minimum value for sigma
*      MAXSIG = REAL (Given)
*        Maximum value for sigma
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 11-SEP-1991
*
* History:
*    TNW 8th Sept 1992 max_cmp passed in common
*    TNW: 28th June 1993, reflect changes in opt_cmn
*    TNW: 29th June 1993, Remove mparms
*-
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'opt_cmn'
      include 'status_inc'
      include 'fit_coding_inc'
      real fitpar(*),fiterr(*)
      real res_store(*)
      real x(mpts),y(mpts)
      logical loop
      logical mstore
      real bstaic
      integer bestng
      integer n
      integer fstat
      integer status
      real aic

*

      integer ok

      integer ignore
      integer igauss

      real minht
      integer pstat
      integer dumi
      real dumr
      character dumc
      character*19 chars
      character*33 fitmen(3)
      data fitmen/
     :     'STORE : Store the fit in the cube',
     :     'REPEAT : Repeat fit',
     :     'QUIT : Abandon this fit'/

      if((deccntr(FIT_MAN).ne.MAN_ALTER).and.
     :            (deccntr(FIT_TYPE).eq.MULTIPLE)) then


* Evaluate AIC, and write value to terminal etc.

        call get_aic(fitpar,n,x,y,mpts,%VAL(CNF_PVAL(weightptr)),1,aic)
        write(chars,'(''AIC = '',f12.2)',iostat=ignore)aic
        call opt_wruser(chars,status)

* Check fit is ok (within tolerances etc.). If the fit fails then LOOP
* is set to true, and the number of components is reduced.

        call remove_cmp(fitpar,n,loop,deccntr(FIT_NCMP),minht,minsig,
     :       maxsig,nagerror)

* If using AIC checking then find lowest value of AIC

        if((deccntr(FIT_STST).eq.POSTAIC).and.
     :            (.not.loop)) then

          if(aic.lt.bstaic) then
            bstaic = aic
            bestng = deccntr(FIT_NCMP)
            call copr2r(n,fitpar,res_store)
            call copr2r(n,fiterr,res_store(n+1))
          end if

*     Check if 1 less component gives better fit, if no component
*     now then select already chosen value.

          loop = deccntr(FIT_NCMP).gt.0

          if(loop) then

            deccntr(FIT_NCMP) = deccntr(FIT_NCMP) - 1
            n = n - 3

          else

            deccntr(FIT_NCMP) = bestng
            n = deccntr(FIT_NCMP)*3+1

*         Copy stored fit into output arrays

            call copr2r(n,res_store,fitpar)
            call copr2r(n,res_store(n+1),fiterr)

*         Print out selected fit

            call opt_wruser('Selected fit:',pstat)
            if(prfits) call opt_wrfit(deccntr,fitpar,fiterr,.true.)

*         Set AIC to correct value for this fit

            aic = bstaic
            mstore = .true.
          end if
        end if

      else if(deccntr(FIT_MAN).eq.MAN_ALTER) then

*   Ask if this fit is ok

        call qmenu('Fit Ok',fitmen,3,1,dumr,dumc,ok,dumi,status)


*     store the result

        loop = ok.eq.2
        if(ok.eq.1) then
          mstore = .true.

*     refit the profile

        else if(loop) then

          call refit_it(times,%VAL(CNF_PVAL(guessptr)),res_store,igauss,
     :                  deccntr(FIT_NCMP),max_cmp,max_times,fitpar,
     :                  deccntr(FIT_MODEL),status)

          deccntr(FIT_NCMP) = igauss
          n = 3*deccntr(FIT_NCMP)+1

        end if
      end if
      if(loop) then
        fstat = 0

* Print out guesses

        if(prfits) then
          call opt_prguess(deccntr,fitpar)
        end if
      end if
      end
