      subroutine check_params(sg_parms,sg_error,istat,fails,tolerance,
     :     reject,model)
*+
* Name:
*    CHECK_PARAMS

* Invocation:
*    CALL CHECK_PARAMS(SG_PARMS,SG_ERROR,ISTAT,FAILS,TOLERANCE,
*          REJECT,MODEL)

* Purpose:
*    To check the parameters against the tolerances

* Description:
*    Istat is returned as 0 if the tolerances are satisfied, or as
*    > 0 otherwise.
*
* Arguments:
*    SG_PARMS(7) = REAL ARRAY (Given)
*        gaussian parameters
*    SG_ERROR(7) = REAL ARRAY (Given)
*        errors on parameters
*    TOLERANCE(MAXTOL) = REAL ARRAY (Given)
*        Tolerances
*    REJECT(MAXREJ) = LOGICAL ARRAY (Given)
*        Which tolerances to use
*    MODEL = INTEGER (Given)
*        Model for fit
*    ISTAT = INTEGER (Returned)
*        Fail status
*    FAILS(8) = LOGICAL ARRAY (Returned)
*        Which tolerances failed
*
*- -------------------------------------------------------------------
      implicit none
      real sg_parms(7)
      real sg_error(7)
      integer model

      logical fails(8)
      integer MAXREJ,MAXTOL
      parameter (MAXREJ  =    7,
     :            MAXTOL =   13)
      real tolerance(MAXTOL)
      logical reject(MAXREJ)
* local
      real h,w,v
      real e_h,e_w,e_v,e_s,e_c
      real h_to_e_h,w_to_e_w,h_s_n,c_tol,s_tol,w_s_n,h_min
      real h_max,v_max,v_min,v_tol,w_max,w_min,w_tol,h_tol
      integer istat
      integer i,itmp
*logical

* if parameter value test passed

      logical tol_rej

* if errors to large

      logical e_rej
      logical h_fail
      logical v_fail
      logical e_fail
      logical w_fail
* -------------------------------------------------------------------

      h = sg_parms(2)
      v = sg_parms(3)
      w = sg_parms(1)

* errors

      e_h = sg_error(2)
      e_v = sg_error(3)
      e_w = sg_error(1)
      if(model.eq.2) then
        e_s = sg_error(5)
      else if(model.eq.3) then
        e_c = sg_error(5)
      endif

* signal to noise ratios

      if(e_h .gt. 0.0) then
        h_to_e_h = abs( h/e_h )
      end if
      if(e_w .gt. 0.0) then
        w_to_e_w = abs( w/e_w )
      end if

* tolerences

      h_tol = tolerance(13)
      h_s_n = tolerance(10)
      c_tol = tolerance(11)
      s_tol = tolerance(12)
      w_s_n = tolerance(7)
      h_min = tolerance(9)
      h_max = tolerance(8)
      v_max = tolerance(2)
      v_min = tolerance(3)
      v_tol = tolerance(1)
      w_max = tolerance(5)
      w_min = tolerance(6)
      w_tol = tolerance(4)

*test heights

      if( reject(1)) then
        h_fail=  tol_rej(h,h_min,h_max)
        fails(1)=h_fail
      end if

* centres test

      if( reject(2)) then
        v_fail =  tol_rej(v,v_min,v_max)
        fails(2)=v_fail
      end if

* width test

      if( reject(3)) then
        w_fail = tol_rej(w,w_min,w_max)
        fails(4)=w_fail
      end if

* error clipping

      if( reject(4)) then
        e_fail = e_rej(h_tol,v_tol,w_tol,e_h,e_v,e_w)
        fails(3)=e_fail
      end if

* signal to noise clipping

      if(reject(5)) then
        if((( h_s_n .gt. 0.0 ) .and. (h_to_e_h.gt.0.0)) .and.
     :    (h_s_n . gt. h_to_e_h )) then
          fails(5)=.true.
        end if
        if((( w_s_n .gt. 0.0 ) .and. (w_to_e_w.gt.0.0)) .and.
     :    (w_s_n . gt. w_to_e_w )) then
          fails(6)=.true.
        end if
      end if


* I think this is correct!!

      if(reject(6)) then
        if((s_tol .gt. 0.0 ) .and. (e_s.gt.s_tol))  then
          fails(7)=.true.
        end if

        if(( c_tol .gt. 0.0 ) .and. (e_c .gt. c_tol) ) then
          fails(8)=.true.
        end if
      end if

      istat=0

      itmp = 1
      do i = 1, 8
        if(fails(i)) istat = istat + itmp
        itmp = itmp*2
      end do
      end
