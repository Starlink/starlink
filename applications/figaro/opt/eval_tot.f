      subroutine eval_tot(sdata,m,tot,fit_parms,n_gauss,funct)
*+
* Name:
*    EVAL_TOT

* Invocation:
*    CALL EVAL_TOT(SDATA,M,TOT,FIT_PARMS,N_GAUSS,FUNCT)

* Purpose:
*   Evaluate total value of components of a fit added together

* Description:
*   Evaluate total value of components of a fit added together

* Arguments:
*     SDATA(M) = REAL ARRAY (Given)
*        X array data
*     M = INTEGER (Given)
*        Number of pixels in range
*     FIT_PARMS(*) = REAL ARRAY (Given)
*        Fit parameters
*     N_GAUSS = INTEGER (Given)
*        Number of components
*     FUNCT = REAL (Given)
*        Function to evaluate model
*     TOT(M) = REAL ARRAY (Returned)
*        Total
* Subroutines/functions called:
*    CAUCHY        : Evaluate Cauchy function
*    GAUSSIAN      : Evaluate Gaussian function
*    HANDLER       : Condition handler (does not stop acceptance of fit)
*    SKEW          : Evaluate skew Gaussian function
*
*    GEN_ADDCAF    : Add constant to real array
*    GEN_CFILL     : Set real array to a constant value
*
*    ESTABLISH : Establish condition handler

* History:
*  Altered TNW 5/12/88 To use GEN_ functions
*  Altered TNW 9/12/88 to only use GEN_CFILL-last change made this not
*  work
*  T.N.Wilkins, Cambridge, 9-JUN-1992 Use fit_coding_inc
*  TNW 21/8/92 Changed so functions gaussian/lorentz passed around as
*         arguments (external)
*-
      implicit none
      integer m
      integer n_gauss
      real sdata(m)
      real tot(m)
      real fit_parms(*)
      real funct
      external funct
* ----------------------------------------------------------------------
      integer i,j
      integer k1
      real sg_parms(6)
      external handler
      call establish(handler)
*
      call gen_cfill(1,m,fit_parms(1),tot)
      sg_parms(1)=0.0
      sg_parms(5)=fit_parms(5)
      sg_parms(6)=fit_parms(6)
      do i=1,n_gauss
        k1=i*3-1
        sg_parms(2) = fit_parms(k1)
        k1=k1+1
        sg_parms(3) = fit_parms(k1)
        k1=k1+1
        sg_parms(4) = fit_parms(k1)
        do j=1,m
          tot(j)=tot(j)+funct(sdata(j),sg_parms)
        end do
      end do
      end
