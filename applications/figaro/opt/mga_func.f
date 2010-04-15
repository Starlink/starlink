      subroutine mga_func(iflag,m,n,xc,rc,ajc,ljc,miw,liw,wn,niw)
*+
* Name:
*    MGA_FUNC

* Invocation:
*    CALL MGA_FUNC(IFLAG,M,N,XC,RC,AJC,LJC,MIW,LIW,WN,NIW)

* Purpose:
*  Evaluate the residuals and their first derivatives for Gaussian fit

* Description
*  routine to evaluate the residuals and their first derivatives
*  for  Gaussian (unconstrained).
*  This routine is also suitable for use when E04FCV is used instead
*  of E04HEV as it can deal with IFLAG=0 as well as IFLAG=2
*  i.e. NO DERVATRIVES. This removed, 4/10/90.
*
* History:
*  TNW 25/10/90 Weighting added (had only applied to SK before!)
*   " 18/9/91  Separate function
*
* Arguments:
*   M = INTEGER (Given)
*        Number of data points
*   N = INTEGER (Given)
*        Number of parameters
*   XC(M) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters
*   LJC = INTEGER (Given)
*        =M
*   LIW = INTEGER (Given)
*
*   NIW = INTEGER (Given)
*
*   RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals of fit?
*   AJC(LJC,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
*  Not used:
*   IFLAG = INTEGER (Returned)
*        If not equal to 0, derivatives calculated
*   MIW(LIW) = INTEGER ARRAY (Returned)
*
*   WN(NIW) = DOUBLE PRECISION ARRAY (Returned)
*
*
      implicit none
      include 'opt_cmn'
      include 'CNF_PAR'          ! For CNF_PVAL function
*-
      integer m,n,liw,ljc,niw
      integer iflag
      double precision wn(niw),ajc(ljc,n)
      double precision rc(m),xc(n)
      integer miw(liw)

      call mga_fun(m,n,xc,rc,ajc,%VAL(CNF_PVAL(dataptr)),
     :             %VAL(CNF_PVAL(densptr)),%VAL(CNF_PVAL(weightptr)))
      end
