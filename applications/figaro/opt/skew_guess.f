      subroutine skew_guess(guess,m,data,dens,absorption)
*+
* Name:
*    SKEW_GUESS

* Invocation:
*    CALL SKEW_GUESS(GUESS,M,DATA,DENS,ABSORPTION)

* Purpose:
*    Obtain guesses for skew fitting.

* Description:
*       Calculate first guess for skew parameter and halfwidth required
*       for a skew gaussian fit.
*
* Arguments:
*    N = INTEGER (Given)
*        number of free parameters
*    M = INTEGER (Given)
*        number of data points
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*
*    ABSORPTION = LOGICAL (Given)
*
*    GUESS(N) = REAL ARRAY (Given and returned)
*        first guesses
*
*  Change to evaluation of halfway, TNW/CAVAD 14/6/90
*-
      implicit none
      integer m
      real guess(*)
      double precision data(m),dens(m)
      logical absorption

* local
      integer k

* control loop over data

      logical loop

* if upper Half-max found

      logical found_high

* if lower half-max found

      logical found_low

* halfmax level for absorption line

      double precision dida

* guess to location of upper halfmax

      double precision xu

* guess to location of lower halfmax

      double precision xl

* base+1/2height

      double precision halfway
      double precision x0

* guess to skew parameter

      double precision skew

* guess to halfwidth of line

      double precision xhalf

* ---------------------------------------------------------------------
*
      x0         = dble(guess(4))
      halfway    = dble(guess(1) + 0.5*guess(3))
      loop       = .true.
      k          = 1
      found_low  = .false.
      found_high = .false.
*
* look for half-max point
*
      do while(loop)
        dida     = dens(k)-halfway
        if(absorption) then
          dida   = -dida
        end if
        if(.not.found_low) then
          if (dida.gt.0.0) then
            found_low = .true.
*
* check for exact x-value
*
            if(abs(dens(k)-dens(k-1)).lt.1.0d-3) then
              xl = 0.5d0*(data(k)+data(k-1))
            else
*
* linear interpolation to find halfmax point
*
              xl=(dens(k-1)-halfway)/(dens(k-1)-dens(k))
              xl=data(k-1)+abs(xl*(data(k)-data(k-1)))
            end if
          end if
        else if(found_low.and.(.not.found_high)) then
          if (dida.lt.0.0) then
            found_high = .true.
*
* again check for exact match
*
            if((abs(dens(k)-dens(k-1))).lt.1.0d-3)then
              xu = 0.5d0*(data(k)+data(k-1))
            else
              xu = (dens(k-1)-halfway)/(dens(k-1)-dens(k))
              xu = data(k-1)+abs(xu*(data(k-1)-data(k)))
            end if
          end if

*   not.found_low

        end if
        if(found_low.and.found_high) then
          loop=.false.
        else
          k=k+1
        end if

* loop

      end do
*
* call bget to get skew
*
      call bget(xu,xl,x0,skew,xhalf)
*
* Set return values
*
      guess(5) = real(skew)
      guess(2) = real(xhalf)
      end
