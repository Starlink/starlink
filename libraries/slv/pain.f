      subroutine pain( status )

      include 'SAE_PAR'
      include 'PAR_ERR'
      integer status
      character * 30 in

 1    continue
      call err_mark()
      call par_get0c( 'IN', in, STATUS )
      if ( status .eq. PAR__NULL ) then
         call err_annul( status )
         call par_cancl( 'IN', status )
         goto 1
c      else if ( status .eq. PAR__ABORT ) then
c         call err_annul( status )
c         call par_cancl( 'IN', status )
c         goto 1
      endif
      call err_rlse

      call msg_setc( 'IN', in )
      call msg_out( ' ', 'in = ^IN', status )
      end
      
