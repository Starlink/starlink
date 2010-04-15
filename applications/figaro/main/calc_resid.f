      subroutine calc_resid(chosen_order,kp1,npts,residuals,x,y
     :     ,athree,max_kplus1, maxnpts)
*+
* Name:
*    CALC_RESID

* Invocation:
*    CALL CALC_RESID(CHOSEN_ORDER,KP1,NPTS,RESIDUALS,X,Y
*                    ,ATHREE,MAX_KPLUS1,MAXNPTS)

* Purpose:
*   Calculate the residuals between the Chebyshev polynomial
*   defined by chosen_order and the actual data Y.
*
* Description:
*   Calculate the residuals between the Chebyshev polynomial
*   defined by chosen_order and the actual data Y.
*
* Subroutines called:
*    E_CPOLY    : Evaluate a Chebyshev polynomial at a series of points
*    GEN_SUBAD  : Subtract 2 double precision arrays

* Arguments:
*   KP1 = INTEGER (Given)
*      Number of Coeffs for the chosen_order
*   NPTS = INTEGER (Given)
*      Number of [X,y] points
*   CHOSEN_ORDER(KP1) = DOUBLE PRECISION ARRAY (Given)
*      Coeffs for the chosen order
*   X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      X values
*   Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      Actual Y values
*   RESIDUALS(NPTS) = DOUBLE PRECISION ARRAY (Returned)
*      Residuals calculated
*   ATHREE (3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Workspace)
*     PDA fitting results array


* History:
*  Altered TNW 9/11/88 to use GEN_SUBAD
*  9/11/97 A.J.Holloway using PDA libs to replace NAG calls.

*-
* import
*
      implicit none
      integer kp1
      integer npts
      integer max_kplus1
      integer maxnpts
      double precision chosen_order(kp1)
      double precision x(npts)
      double precision y(npts)
      double precision athree(3*maxnpts+3*max_kplus1)
*
* export
*
      double precision residuals(npts)
*
      integer e_cpoly

      if(e_cpoly(x,residuals,chosen_order,kp1,npts,athree
     : ,max_kplus1,maxnpts).eq.0) then
        call gen_subad(npts,y,residuals,residuals)
      endif
      end
