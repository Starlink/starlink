      subroutine pol1_stats( title, nel, data, status )
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      character title*(*)
      integer nel, status, i, n
      real data( nel ), sum1, sum2, mx, mn, mean, val

      if( status .ne. sai__ok ) return

      sum1 = 0
      sum2 = 0
      mx = val__minr
      mn = val__maxr
      n = 0

      do i = 1, nel
         val = data( i ) 
         if( val .ne. val__badr ) then
            n = n + 1
            sum1 = sum1 + val
            sum2 = sum2 + val*val
            mx = max( mx, val )
            mn = min( mn, val )
         end if
      end do

      call msg_out( ' ', title, status )
      if( n .ne. 0 ) then
         mean = sum1/n
         call msg_setr( 'm', mean )
         call msg_out( ' ', '   Mean  : ^m', status )
         call msg_setr( 'm', sqrt( max( 0.0, sum2/n - mean*mean ) ) )
         call msg_out( ' ', '   Sigma : ^m', status )
         call msg_setr( 'm', mx )
         call msg_out( ' ', '   Max   : ^m', status )
         call msg_setr( 'm', mn )
         call msg_out( ' ', '   Min   : ^m', status )
      end if

      call msg_seti( 'm', n )
      call msg_out( ' ',    '   Ngood : ^m', status )

      end
