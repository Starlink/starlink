      subroutine s3R( y, N, change )
*+
* Name:
*    S3R

* Invocation:
*    CALL S3R( Y, N, CHANGE )

* Purpose:
*   compute repeated running  median of 3 on Y()

* Description:
*   compute repeated running  median of 3 on Y()
      implicit none

      integer n
      real y(n)
      logical change
*-

      change = .TRUE.
      Do while ( change)
        change  = .FALSE.
        call s3( y , n , change )
      end do
      call endpts( y , n)


      end
