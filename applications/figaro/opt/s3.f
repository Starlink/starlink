      subroutine s3( y, N, CHANGE )
*+
* Name:
*    S3

* Invocation:
*    CALL S3( Y, N, CHANGE )

* Purpose:
*  compute runnign median of 3 on Y()

* Description:
*  compute runnign median of 3 on Y()
*  sets CAHNGE .TRUE. if any change is made
      implicit none
      integer n
      real y(n)
      logical CHANGE
*-

      integer  i , nm1
      real y1 , y2, y3

      nm1 = n - 1

      Y2 = y(1)
      Y3 = y(2)

      do i = 2 , nm1
        y1  = y2
        y2 = y3
        y3  = y(I + 1)
        call medof3( y1 , y2 , y3 , y(i) , CHANGE )

      end do

      end
