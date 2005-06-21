      subroutine lm_skf(m,n,xc,rc,fjac,ldfjac,iflag)
*+
* Name:
*    LM_SKF

* Invocation:
*    CALL LM_SKF(M,N,XC,RC,FJAC,LDFJAC,IFLAG)
*
* Description:
*  To calculate residuals or Jacobian for a SKEW fit model.
*
* Purpose:
*  To calculate residuals or Jacobian for a SKEW fit model.
*
* Arguments:
*     M = INTEGER (Given)
*        Number of data points
*     N = INTEGER (Given)
*        Number of variables
*     XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Function parameters
*     LDFJAC = INTEGER (Given)
*        Dimension of fjac
*     IFLAG = INTEGER (Given)
*        Flag to determine action of routine, 1 => fvec,
*                        2 => fjac
*     RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals
*     FJAC(LDFJAC,N) = DOUBLE PRECISION ARRAY (Returned)
*        Jacobian
* Subroutines/functions referenced:
*
* Author:
*   T.N.Wilkins, Cambridge,  14-OCT-1991 based on lm_mgf
*
* History:
*-
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer m
      integer n
      double precision xc(n)
      double precision rc(m)
      integer ldfjac
      double precision fjac(ldfjac,n)
      integer iflag
      include 'opt_cmn'

*


* Calculate residuals or derivative

      call lm_skf_s(iflag,m,n,xc,rc,fjac,ldfjac,%VAL(CNF_PVAL(densptr)),
     :              %VAL(CNF_PVAL(dataptr)),%VAL(CNF_PVAL(weightptr)))
      end
