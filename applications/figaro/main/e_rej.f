      logical function e_rej(h_tol,v_tol,w_tol,e_h,e_v,e_w)
*+
* Name:
*    E_REJ

* Invocation:
*   (LOGICAL) = E_REJ(H_TOL,V_TOL,W_TOL,E_H,E_V,E_W)

* Purpose:
*   Test if tolerances on errors are met

* Description:
*   Test if tolerances on errors are met

* Arguments:
*    H_TOL = REAL (Given)
*
*    V_TOL = REAL (Given)
*
*    W_TOL = REAL (Given)
*
*    E_H = REAL (Given)
*
*    E_V = REAL (Given)
*
*    E_W = REAL (Given)
*
*- -----------------------------------------------------------------
      implicit none
      real h_tol,v_tol,w_tol,e_h,e_v,e_w
      e_rej   = ( (( v_tol .gt. 0.0) .and. (v_tol .lt. e_v)) .or.
     :    ( ( h_tol .gt. 0.0) .and. (h_tol .lt. e_h)) .or.
     :    ( ( w_tol .gt. 0.0) .and. (w_tol .lt. e_w)))
      end
