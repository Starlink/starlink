      subroutine s4253H( y, N, err)
*+
* Name:
*    S4253H

* Invocation:
*    CALL S4253H( Y, N, ERR)

* Purpose:
*   Smooth Y() by 4253H

* Description:
*   Smooth Y() by 4253H

      implicit none
      integer n, err
      real y(n)
*-
      integer NW, OK
      Parameter( OK = 0)
      Parameter (NW = 5)
      real endsav, work(NW), save(NW)
      logical CHANGE

      CHANGE = .false.
      call s4( y, n , endsav , work , save , NW , Err)

      if (err .eq. OK ) then
        call s2( y, n , endsav)
        call s5( y, n , work , save, NW, ERR)

        if (err .eq. OK ) then
          call s3( y, n , CHANGE)
          call endpts(y,n)
          call hann(y,n)
        end if
      end if
      end
