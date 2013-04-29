      program testframeset
      implicit none

      include 'AST_PAR'
      include 'AST_ERR'
      include 'SAE_PAR'

      integer status, pfrm, ffrm, p2fmap, fs, p2fmap2, result, orig
      double precision ina(2), inb(2), outa(2), outb(2), xout, yout
      character text*100

c      call ast_watchmemory(100)


      status = sai__ok
      call err_mark( status )
      call ast_begin( status )


      pfrm = ast_frame( 2, "Domain=PIXEL", status )
      ffrm = ast_frame( 2, "Domain=FPLANE", status )

      ina( 1 ) = 1.0
      ina( 2 ) = 1.0
      inb( 1 ) = 100.0
      inb( 2 ) = 200.0

      outa( 1 ) = -2.5
      outa( 2 ) = -1.0
      outb( 1 ) = 2.5
      outb( 2 ) = 1.0
      p2fmap = ast_winmap( 2, ina, inb, outa, outb, ' ', status )

      fs = ast_frameset( pfrm, ' ', status )
      call ast_addframe( fs, AST__CURRENT, p2fmap, ffrm, status )


      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FPLANE' ) call stopit( status, 'Error 1' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FPLANE' ) call stopit( status, 'Error 2' )

      if( ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                     'Error 3' )


      call ast_addvariant( FS, ast__null, 'FP1', status )

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FP1' ) call stopit( status, 'Error 4' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FP1' ) call stopit( status, 'Error 5' )

      if( .not. ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                       'Error 6' )

      call ast_clear( fs, 'Variant', status )

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FPLANE' ) call stopit( status, 'Error 7' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FPLANE' ) call stopit( status, 'Error 8' )

      if( ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                     'Error 9' )

      call ast_addvariant( FS, ast__null, 'FP1', status )

      outa( 1 ) = 100.0
      outa( 2 ) = 100.0
      outb( 1 ) = 200.0
      outb( 2 ) = 200.0
      p2fmap2 = ast_winmap( 2, ina, inb, outa, outb, ' ', status )

      call ast_invert( p2fmap, status )
      call ast_addvariant( fs, ast_simplify(
     :                     ast_cmpmap( p2fmap, p2fmap2, 1, ' ',
     :                                 status ), status ),
     :                     'FP2', status )

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FP1 FP2' ) call stopit( status, 'Error 10' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FP2' ) call stopit( status, 'Error 11' )

      if( .not. ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                      'Error 12' )
      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 13' )

      call ast_setc( fs, 'Variant', 'FP1', status )
      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 0.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 0.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 14' )

      outa( 1 ) = -100.0
      outa( 2 ) = -100.0
      outb( 1 ) = -200.0
      outb( 2 ) = -200.0
      p2fmap2 = ast_winmap( 2, ina, inb, outa, outb, ' ', status )

      p2fmap = ast_getmapping( fs, AST__CURRENT, AST__BASE, status )
      call ast_addvariant( fs, ast_simplify(
     :                     ast_cmpmap( p2fmap, p2fmap2, 1, ' ',
     :                                 status ), status ),
     :                     'FP3', status )

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FP1 FP2 FP3' ) call stopit( status, 'Error 15' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FP3' ) call stopit( status, 'Error 16' )

      if( .not. ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                      'Error 17' )
      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout + 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout + 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 18' )

      call ast_setc( fs, 'Variant', 'FP2', status )
      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 19' )

      call checkdump( fs, result, status )

      text = ast_getc( result, 'AllVariants', status )
      if( text .ne. 'FP1 FP2 FP3' ) call stopit( status, 'Error 20' )

      text = ast_getc( result, 'Variant', status )
      if( text .ne. 'FP2' ) call stopit( status, 'Error 21' )

      call ast_tran2( result, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 22' )



      orig = ast_geti( fs, 'Current', status )
      call ast_addframe( fs, AST__CURRENT, AST_UNITMAP( 2, '', status ),
     :                   AST_FRAME( 2, "Domain=DSB", status ) )
      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 23' )

      if( status .eq. sai__ok ) then
         call ast_setc( fs, 'Variant', 'FP1', status )
         if( status .eq. ast__attin ) then
            call err_annul( status )
         else
            call err_flush( status )
            call stopit( status, 'Error 24' )
         end if
      end if

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'DSB' ) call stopit( status, 'Error 25' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'DSB' ) call stopit( status, 'Error 26' )

      if( ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                     'Error 27' )

      call ast_mirrorvariants( fs, orig, status )

      text = ast_getc( fs, 'AllVariants', status )
      if( text .ne. 'FP1 FP2 FP3' ) call stopit( status, 'Error 28' )

      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FP2' ) call stopit( status, 'Error 29' )

      if( .not. ast_test( fs, 'Variant', status ) ) call stopit( status,
     :                                                     'Error 30' )

      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 150.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 150.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 31' )

      call ast_set( fs, 'Variant=FP1', status )
      text = ast_getc( fs, 'Variant', status )
      if( text .ne. 'FP1' ) call stopit( status, 'Error 32' )

      call ast_tran2( fs, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 0.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 0.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 33' )

      call checkdump( fs, result, status )

      text = ast_getc( result, 'AllVariants', status )
      if( text .ne. 'FP1 FP2 FP3' ) call stopit( status, 'Error 34' )

      text = ast_getc( result, 'Variant', status )
      if( text .ne. 'FP1' ) call stopit( status, 'Error 35' )

      call ast_tran2( result, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 0.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 0.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 36' )

      result = ast_copy( fs, status )

      text = ast_getc( result, 'AllVariants', status )
      if( text .ne. 'FP1 FP2 FP3' ) call stopit( status, 'Error 37' )

      text = ast_getc( result, 'Variant', status )
      if( text .ne. 'FP1' ) call stopit( status, 'Error 38' )

      call ast_tran2( result, 1, 50.5D0, 100.5D0, .TRUE., xout, yout,
     :                status )
      if( abs( xout - 0.0D0 ) .gt. 1.0E-6 .OR.
     :    abs( yout - 0.0D0 ) .gt. 1.0E-6 )  call stopit( status,
     :                                                      'Error 39' )









      call ast_end( status )
      call err_rlse( status )

      call ast_activememory( 'testframeset' )
      call ast_flushmemory( 1 );

      if( status .eq. sai__ok ) then
         write(*,*) 'All FrameSet tests passed'
      else
         write(*,*) 'FrameSet tests failed'
      end if

      end



      subroutine stopit( status, text )
      implicit none
      include 'SAE_PAR'
      integer status
      character text*(*)

      if( status .ne. sai__ok ) return
      status = sai__error
      write(*,*) text

      end

      subroutine checkdump( obj, result, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character key*30,txt1*50,txt2*50
      integer obj, status, next, end, ch, result, ll, overlap, size,
     :        i, type,obj1,obj2,l1,l2,nl,nrow,nrowold
      external mysource, mysink
      character buf*400000

      common /ss1/ buf
      common /ss2/ next, end, ll, nl

      if( status .ne. sai__ok ) return

      ch = ast_channel( mysource, mysink, ' ', status )

      nl = 0
      ll = 110
      next = 1
      if( ast_write( ch, obj, status ) .ne.1 ) then
         call stopit( status, 'Cannot write supplied object to '//
     :                'channel' )
      end if

      next = 1
      nl = 0
      result = ast_read( ch, status )

      if( result .eq. ast__null ) then
         call stopit( status, 'Cannot read object from channel' )
      end if

      if( .not. ast_isaframeset( result, status ) ) then
         call stopit( status, 'Object read from channel is not a '//
     :                'FrameSet')
      end if

      end

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll, nl
      character buf*400000

      common /ss1/ buf
      common /ss2/ next, end, ll,nl

      if( status .ne. sai__ok ) return

      if( next .ge. end ) then
         call ast_putline( buf, -1, status )
      else

c         write(*,*) buf( next : next + ll - 1 )
         call ast_putline( buf( next : ), ll, status )
         nl = nl + 1
      endif

      next = next + ll

      end

      subroutine mysink( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll, nl
      character buf*400000
      character line*1000

      common /ss1/ buf
      common /ss2/ next, end, ll, nl

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf( next : ) = line( f : l )
      l = l - f + 1

      if( next + ll - 1 .ge. 400000 ) then
         write(*,*)
         call stopit( status, 'Buffer overflow in mysink!!' )
      else if( l .gt. ll ) then
         write(*,*)
         write(*,*) buf( next : next + l)
         write(*,*) 'Line length ',l
         call stopit( status, 'Line overflow in mysink!!' )
      else
         end = next + l
         buf( end : next + ll - 1 ) = ' '
         nl = nl + 1
      endif

      next = next + ll

      end


