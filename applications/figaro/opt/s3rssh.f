      subroutine s3rssh( y, N, err)
*+
* Name:
*    S3RSSH

* Invocation:
*    CALL S3RSSH( Y, N, ERR)

* Purpose:
*  Smooth Y() by 3RSSH twice

* Description:
*  Smooth Y() by 3RSSH twice
      implicit none
      integer n, err
      real y(n)
*-
      logical change

      call s3r( y, n ,change)
      call split( y, n, change )

      if ( change ) then
        call s3r( y, n, change)
        change = .false.
        call split( y , n , change )

        if ( change ) then
          call s3r( y , n, change)
        end if
      else
        call hann( y , n )
      end if
      end
