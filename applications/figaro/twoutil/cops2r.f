      subroutine cops2r(n,i2data,rldata)
*+
* Name:
*    COPS2R

* Invocation:
*    CALL COPS2R(N,I2DATA,RLDATA)

* Purpose:
*  The integer*2 array I2DATA is copied to the real array RLDATA.

* Description:
*  The integer*2 array I2DATA is copied to the real array RLDATA.
*
* Arguments:
*      N = INTEGER (Given)
*        Number of array elements
*      I2DATA(N) = INTEGER*2 ARRAY (Given)
*        Integer*2 data
*      RLDATA(N) = REAL ARRAY (Returned)
*        Real data

* History:
*   T.N.Wilkins, Cambridge, 20-NOV-1989
*        "          "       23-NOV-1989 renamed
*-
      implicit none
      integer n
      integer*2 i2data(n)
      real rldata(n)
      integer i

*
      do i = 1, n
        rldata(i) = real(i2data(i))
      end do

      end
