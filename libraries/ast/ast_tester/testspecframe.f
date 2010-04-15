      program testspecframe
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      double precision rf, x, y
      integer status, sf, sf1, sf2, fs
      status = sai__ok

      sf = ast_specframe( 'system=freq,unit=Hz', status )
      if( ast_GetD( sf, 'SpecOrigin', status ) .ne. 0.0 ) then
         call stopit( status, 'Error 0' )
      end if

      rf = ast_GetD( sf, 'RestFreq', status )
      call ast_SetD( sf, 'SpecOrigin', rf*1.0D9, status )
      if( ast_GetD( sf, 'SpecOrigin', status ) .ne. rf*1.0D9 ) then
         call stopit( status, 'Error 1' )
      end if

      call ast_setc( sf, 'Unit(1)', 'GHz', status )
      if( ast_GetD( sf, 'SpecOrigin', status ) .ne. rf ) then
         call stopit( status, 'Error 2' )
      end if

      call checkdump( sf, 'Error 3', status )

      call ast_setc( sf, 'System', 'vrad', status )
      if( abs( ast_GetD( sf, 'SpecOrigin', status ) ) .gt. 1.0D-8 ) then
         write(*,*) ast_GetD( sf, 'SpecOrigin', status )
         call stopit( status, 'Error 4' )
      end if

      call ast_setc( sf, 'System', 'freq', status )
      call ast_setc( sf, 'Unit(1)', 'Hz', status )
      if( abs( ast_GetD( sf, 'SpecOrigin', status ) ) .ne.
     :            rf*1.0D9 ) then
         write(*,*) ast_GetD( sf, 'SpecOrigin', status )
         call stopit( status, 'Error 5' )
      end if

      call ast_setc( sf, 'StdOfRest', 'LSRD', status )
      if( abs( ast_GetD( sf, 'SpecOrigin', status ) -
     :                  rf*1.00000212890848D9 ) .gt. 10.0 ) then
         write(*,*) ast_GetD( sf, 'SpecOrigin', status )
         write(*,*) 'Should be ',rf*1.00000212890848D9
         call stopit( status, 'Error 6' )
      end if


      sf1 = ast_specframe( 'system=freq,unit=Hz', status )
      call ast_setd( sf1, 'SpecOrigin', 1.0D20, status )
      sf2 = ast_specframe( 'system=freq,unit=Hz', status )
      call ast_setd( sf2, 'SpecOrigin', 1.01D20, status )
      fs = ast_convert( sf1, sf2, "", status );

      x = 0.03D20
      call ast_tran1( fs, 1, x, .true., y, status )
      if( abs( y - 0.02D20 ) .gt. 0.0 ) then
         write(*,*) y, y - 0.02D20
         call stopit( status, 'Error 7' )
      end if

      if( ast_getl( sf1, 'AlignSpecOffset', status ) ) then
         call stopit( status, 'Error 8' )
      end if
      call ast_setl( sf1, 'AlignSpecOffset', .true., status )
      call ast_setl( sf2, 'AlignSpecOffset', .true., status )

      fs = ast_convert( sf1, sf2, "", status );

      x = 0.03D20
      call ast_tran1( fs, 1, x, .true., y, status )
      if( abs( y - x ) .gt. 0.0 ) then
         write(*,*) y, y - x
         call stopit( status, 'Error 9' )
      end if

      sf = ast_specframe( 'system=freq,unit=Hz', status )
      call ast_setc( sf, 'SourceVRF', 'LSRK', status )
      call ast_setd( sf, 'SourceVel', 1000.0D0, status )

      call ast_setc( sf, 'SourceVRF', 'BARY', status )
      call ast_setc( sf, 'SourceSys', 'ZOPT', status )

      if( abs( ast_getd(  sf, 'SourceVel', status ) -
     :         0.00334028336870307D0 ) .gt. 1.0D-10 ) then
         write(*,*) ast_getd(  sf, 'SourceVel', status )
         call stopit( status, 'Error 11' )
      end if

      call checkdump( sf, 'Error 10', status )
      call ast_setc( sf, 'SourceVRF', 'LSRK', status )
      call ast_setc( sf, 'SourceSys', 'VREL', status )

      if( abs( ast_getd(  sf, 'SourceVel', status ) -
     :         1000.0D0 ) .gt. 1.0D-6 ) then
         write(*,*) ast_getd(  sf, 'SourceVel', status )
         call stopit( status, 'Error 12' )
      end if

      if( status .eq. sai__ok ) then
         write(*,*) 'All SpecFrame tests passed'
      else
         write(*,*) 'SpecFrame tests failed'
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


      subroutine checkdump( obj, text, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*)
      integer obj, status, next, end, ch, result, ll, overlap
      external mysource, mysink
      character buf*25000

      common /ss1/ buf
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      ch = ast_channel( mysource, mysink, ' ', status )


      ll = 110
      next = 1
      if( ast_write( ch, obj, status ) .ne.1 ) then
         write(*,*) text
         call stopit( status, 'Cannot write supplied object to '//
     :                'channel' )
      end if

      next = 1
      result = ast_read( ch, status )
      if( result .eq. ast__null ) then
         write(*,*) text
         call stopit( status, 'Cannot read object from channel' )
      end if



      if( ast_getd( obj, 'specorigin', status ) .ne.
     :    ast_getd( result, 'specorigin', status ) ) then
         call ast_Show( obj, status )
         call ast_Show( result, status )
         write(*,*) text
         call stopit( status, 'Object has changed' )
      end if

      end

      subroutine sink1( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      logical fsfound, done
      common /sink1com/ fsfound, done

      integer status, l
      character line*200

      if( status .ne. sai__ok ) return
      call ast_getline( line, l, status )

      if( index( line( : l ),'Unc =' ) .GT. 0 ) then
         done = .true.

      else if( .not. done .and.
     :         index( line( : l ),'FrameSet' ) .GT. 0 ) then
         fsfound= .true.
      end if

      end

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll
      character buf*25000

      common /ss1/ buf
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      if( next .ge. end ) then
         call ast_putline( buf, -1, status )
      else
         call ast_putline( buf( next : ), ll, status )
      endif

      next = next + ll

      end

      subroutine mysink( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll
      character buf*25000
      character line*1000

      common /ss1/ buf
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf( next : ) = line( f : l )
      l = l - f + 1

      if( next + ll - 1 .ge. 25000 ) then
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
      endif

      next = next + ll

      end


