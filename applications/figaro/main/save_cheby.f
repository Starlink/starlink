      subroutine save_cheby(ni,istart,iend,chosen_order,kp1,max_kplus1,
     :     store)
*+
* Name:
*    SAVE_CHEBY

* Invocation:
*    CALL SAVE_CHEBY(NI,ISTART,IEND,CHOSEN_ORDER,KP1,MAX_KPLUS1,
*          STORE)

* Purpose:
*  Store Chebyshev coefficients

* Description:
*   Save the cheby coefficients for the chosen order into
*   the .COEFFS structure of the .CONTINUUM structure for a 2D
*   data set.
* Arguments:
*   NI = INTEGER  (Given)
*       2nd dimension of data (Number of X-sects)
*   ISTART = INTEGER  (Given)
*       1st of these to recieve CHOSEN_ORDER
*   IEND = INTEGER  (Given)
*       Last of these to recieve CHOSEN_ORDER
*   KP1 = INTEGER  (Given)
*       NUmber of coeffs to store
*   MAX_KPLUS1 = INTEGER  (Given)
*       Maximum number of coeffs possible
*   CHOSEN_ORDER(MAX_KPLUS1)= DOUBLE PRECISION ARRAY(Given)
*       The cheby coeffs
*   STORE(MAX_KPLUS1,NI)= DOUBLE PRECISION ARRAY (Returned)
*       The array storing these as a function of NI
* History:
*   Orginal DJA 1/3/89    MAVAD::DJA
*   Minor changes TNW/CAVAD 17/8/90
*- -----------------------------------------------------------------
      implicit none
      integer ni
      integer istart
      integer iend
      integer max_kplus1
      integer kp1
      double precision chosen_order(max_kplus1)
      double precision store(max_kplus1,ni)

* local


* do loop

      integer i
      integer status

* check to see that kp1 is non zero and that it is LE MAx_kplus1

      if(kp1.le.0) then
        call par_wruser('Cannot store Zero Cheby Coefficients',status)
      else if(kp1.gt.max_kplus1) then
        call par_wruser('Number of coefficents is too Large',status)
        call par_wruser('SAVE_CHEBY> Aborting',status)
      else
        do i = istart,iend
          call copd2d(kp1,chosen_order,store(1,i))
        end do
      end if
      end
