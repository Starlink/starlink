      program testflux
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      double precision xin, xout
      integer status, sf, ff, ff2, mp, fs, sf2
      status = sai__ok

      sf = ast_specframe( 'system=freq,unit=GHz', status )
      ff = ast_Fluxframe( 123.0D0, sf, ' ', status )

      if( ast_GetD( ff, 'specval', status ) .ne. 123.0D0 ) then
         call stopit( status, 'Error 1' )
      end if

      if( ast_Test( ff, 'specval', status ) ) then
         call stopit( status, 'Error 2' )
      end if

      call ast_setd( ff, 'specval', 333.3D0, status )
      if( ast_GetD( ff, 'specval', status ) .ne. 333.3D0 ) then
         call stopit( status, 'Error 3' )
      end if

      if( .not. ast_Test( ff, 'specval', status ) ) then
         call stopit( status, 'Error 4' )
      end if

      call ast_clear( ff, 'specval', status )

      if( ast_GetD( ff, 'specval', status ) .ne. 123.0D0 ) then
         call stopit( status, 'Error 5' )
      end if

      if( ast_Test( ff, 'specval', status ) ) then
         call stopit( status, 'Error 6' )
      end if


      call checkDump( ff, 'CheckDump 1', status )


      ff2 = ast_Fluxframe( 123.1D0, sf, ' ', status )
      fs = ast_convert( ff, ff2, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 8' )
      else
         mp = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
         if( .not. ast_isaunitmap( mp, status ) ) then
            call stopit( status, 'error 9' )
         end if
      end if





      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/Hz', status )
      if( ast_GetC( ff, 'System', status ) .ne. 'FLXDN' ) then
         write(*,*)  ast_GetC( ff, 'System', status )
         call stopit( status, 'error 10' )
      endif

      ff2 = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/GHz', status )
      if( ast_GetC( ff2, 'System', status ) .ne. 'FLXDN' ) then
         write(*,*)  ast_GetC( ff2, 'System', status )
         call stopit( status, 'error 11' )
      endif

      fs = ast_convert( ff2, ff, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 12' )
      else
         mp = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
         if( .not. ast_isazoommap( mp, status ) ) then
            call stopit( status, 'error 13' )
         else if( ast_getd( mp, 'Zoom', status ) .ne. 1.0D-9 ) then
            write(*,*) ast_getd( mp, 'Zoom', status )
            call stopit( status, 'error 14' )
         end if
      end if


      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/m', status )
      if( ast_GetC( ff, 'System', status ) .ne. 'FLXDNW' ) then
         write(*,*)  ast_GetC( ff, 'System', status )
         call stopit( status, 'error 15' )
      endif

      sf2 = ast_specframe( 'system=freq,unit=Hz', status )
      ff2 = ast_Fluxframe( 123.0D9, sf2, 'unit=W/m^2/Angstrom', status )
      if( ast_GetC( ff2, 'System', status ) .ne. 'FLXDNW' ) then
         write(*,*)  ast_GetC( ff2, 'System', status )
         call stopit( status, 'error 16' )
      endif

      fs = ast_convert( ff2, ff, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 17' )
      else
         mp = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
         if( .not. ast_isazoommap( mp, status ) ) then
            call stopit( status, 'error 18' )
         else if( ast_getd( mp, 'Zoom', status ) .ne. 1.0D10 ) then
            write(*,*) ast_getd( mp, 'Zoom', status )
            call stopit( status, 'error 19' )
         end if
      end if




      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/m', status )
      if( ast_GetC( ff, 'System', status ) .ne. 'FLXDNW' ) then
         write(*,*)  ast_GetC( ff, 'System', status )
         call stopit( status, 'error 20' )
      endif

      sf2 = ast_specframe( 'system=wave,unit=nm', status )
      ff2 = ast_Fluxframe( 2437337.06D0, sf2, 'unit=W/m^2/Angstrom',
     :                     status )
      if( ast_GetC( ff2, 'System', status ) .ne. 'FLXDNW' ) then
         write(*,*)  ast_GetC( ff2, 'System', status )
         call stopit( status, 'error 21' )
      endif

      fs = ast_convert( ff, ff2, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 22' )
      else
         mp = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
         if( .not. ast_isazoommap( mp, status ) ) then
            call stopit( status, 'error 23' )
         else if( ast_getd( mp, 'Zoom', status ) .ne. 1.0D-10 ) then
            write(*,*) ast_getd( mp, 'Zoom', status )
            call stopit( status, 'error 24' )
         end if
      end if


      sf = ast_specframe( 'system=freq,unit=GHz', status )
      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/Hz', status )
      sf2 = ast_specframe( 'system=wave,unit=nm', status )
      ff2 = ast_Fluxframe( 2437337.06D0, sf2, 'unit=W/m^2/m',
     :                     status )
      fs = ast_convert( ff, ff2, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 25' )
      else
         xin = 1.0D-13
         call ast_tran1( fs, 1,xin, 1,xout, status )
         if( abs( xout - 5.04649119D0 ) .gt. 1.0D-6 ) then
            call stopit( status, 'error 26' )
         end if
      end if


      sf = ast_specframe( 'system=freq,unit=GHz', status )
      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/Hz/arcsec**2',
     :                    status )
      if( ast_getc( ff, 'System', status ) .ne. 'SFCBR' )
     :       call stopit( status, 'error 27a' )

      sf2 = ast_specframe( 'system=wave,unit=nm', status )
      ff2 = ast_Fluxframe( 2437337.06D0, sf2, 'unit=W/m^2/m/deg**2',
     :                     status )
      if( ast_getc( ff2, 'System', status ) .ne. 'SFCBRW' )
     :       call stopit( status, 'error 27b' )

      fs = ast_convert( ff, ff2, ' ', status )
      if( fs .eq. ast__null ) then
         call stopit( status, 'error 27' )
      else
         xin = 1.0D-13
         call ast_tran1( fs, 1,xin, 1,xout, status )
         if( abs( xout - 65402525.8D0 ) .gt. 1.0 ) then
            write(*,*) xout - 65402525.8D0
            call stopit( status, 'error 28' )
         end if
      end if


      ff = ast_Fluxframe( 123.0D0, sf, 'unit=W/m^2/Hz/arcsec**2',
     :                    status )
      ff2 = ast_Fluxframe( 2437337.06D0, sf2, 'unit=W/m^2/m',
     :                     status )

      fs = ast_convert( ff, ff2, ' ', status )
      if( fs .ne. ast__null ) call stopit( status, 'error 29' )















      if( status .eq. sai__ok ) then
         write(*,*) 'All FluxFrame tests passed'
      else
         write(*,*) 'FluxFrame tests failed'
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



      if( ast_getd( obj, 'specval', status ) .ne.
     :    ast_getd( result, 'specval', status ) ) then
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


