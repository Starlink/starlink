      program testspecflux
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      double precision xin, xout,yin, yout
      integer status, sff, sff2, sf, ff, ff2, mp, fs, sf2,perm(2),csff
      status = sai__ok

      sf = ast_specframe( 'system=freq,unit=GHz', status )
      ff = ast_Fluxframe( 123.0D0, sf, 'Unit=Jy', status )
      sff = ast_specfluxframe( sf, ff, ' ', status )


      if( ast_GetC( sff, 'class', status ) .ne. 'SpecFluxFrame' ) then
         call stopit( status, 'Error 0' )
      end if

      if( ast_GetD( sff, 'specval', status ) .ne. 123.0D0 ) then
         call stopit( status, 'Error 1' )
      end if

      if( ast_Test( sff, 'specval', status ) ) then
         call stopit( status, 'Error 2' )
      end if

      call ast_setd( sff, 'specval', 333.3D0, status )
      if( ast_GetD( sff, 'specval', status ) .ne. 333.3D0 ) then
         call stopit( status, 'Error 3' )
      end if

      if( .not. ast_Test( sff, 'specval', status ) ) then
         call stopit( status, 'Error 4' )
      end if

      call ast_clear( sff, 'specval', status )

      if( ast_GetD( sff, 'specval', status ) .ne. 123.0D0 ) then
         call stopit( status, 'Error 5' )
      end if

      if( ast_Test( sff, 'specval', status ) ) then
         call stopit( status, 'Error 6' )
      end if


      call checkDump( sff, 'CheckDump 1', status )


      ff2 = ast_Fluxframe( 123.1D0, sf, 'System=flxdnw', status )

      if( ast_getc( ff2, 'unit', status ) .ne. 'W/m^2/Angstrom' )
     :    call stopit( status, 'Error 6B' )
      if( ast_getc( ff2, 'system', status ) .ne. 'FLXDNW' )
     :    call stopit( status, 'error 6C' )

      sff2 = ast_specfluxframe( sf, ff2, ' ', status )
      if( ast_GetC( sff2, 'class', status ) .ne. 'SpecFluxFrame' ) then
         call stopit( status, 'Error 7' )
      end if

      csff = ast_copy( sff,status )
      fs = ast_convert( sff, sff2, ' ', status )
      if( fs .eq. ast__null ) call stopit( status, 'error 8' )

      yin = 1.0D0
      xin = 2.0D0
      call ast_tran2( fs, 1, xin, yin, .true., xout, yout, status )

      if( abs(yout - 1.33425638D-26) .gt. 1.0D-32 )
     :    call stopit( status, 'error 9' )

      if( xout .ne. 2.0D0 ) call stopit( status, 'error 10' )

      perm(1)=2
      perm(2)=1
      call ast_PermAxes( sff2, perm, status )

      fs = ast_convert( sff, sff2, ' ', status )
      if( fs .eq. ast__null ) call stopit( status, 'error 11' )
      call ast_tran2( fs, 1, xin, yin, .true., xout, yout, status )

      if( abs(xout - 1.33425638D-26) .gt. 1.0D-32 )
     :    call stopit( status, 'error 12' )

      if( yout .ne. 2.0D0 ) call stopit( status, 'error 13' )

      perm(1)=2
      perm(2)=1
      call ast_PermAxes( sff, perm, status )

      fs = ast_convert( sff, sff2, ' ', status )
      if( fs .eq. ast__null ) call stopit( status, 'error 14' )

      yin = 2.0D0
      xin = 1.0D0
      call ast_tran2( fs, 1, xin, yin, .true., xout, yout, status )

      if( abs(xout - 1.33425638D-26) .gt. 1.0D-32 )
     :    call stopit( status, 'error 15' )

      if( yout .ne. 2.0D0 ) call stopit( status, 'error 16' )



      ff2 = ast_Fluxframe( AST__BAD, AST__NULL, 'Unit=log(W/m2/nm)',
     :                     status )
      if( ast_getc( ff2, 'system', status ) .ne. 'FLXDNW' )
     :    call stopit( status, 'error 17' )
      sff2 = ast_specfluxframe( sf, ff2, ' ', status )

      fs = ast_convert( csff, sff2, ' ', status )
      if( fs .eq. ast__null ) call stopit( status, 'error 18' )

      yin = 1.0D0
      xin = 2.0D0
      call ast_tran2( fs, 1, xin, yin, .true., xout, yout, status )

      if( abs(yout + 24.8747607 ) .gt. 0.000001 ) then
         write(*,*) yout + 24.8747607
         call stopit( status, 'error 19' )
      endif

      if( xout .ne. 2.0D0 ) call stopit( status, 'error 20' )


      call ast_tran2( fs, 1, xout, yout, .false., xin, yin, status )

      if( abs( xin - 2.0D0 ) .gt. 1.0D-9 ) call stopit( status,
     :                                                  'error 21' )
      if( abs( yin - 1.0D0 ) .gt. 1.0D-9 ) call stopit( status,
     :                                                  'error 22' )





      ff2 = ast_Fluxframe( AST__BAD, AST__NULL, 'Unit=log(W/m2/nm/sr)',
     :                     status )
      if( ast_getc( ff2, 'system', status ) .ne. 'SFCBRW' )
     :    call stopit( status, 'error 23' )
      sff2 = ast_specfluxframe( sf, ff2, ' ', status )

      sf = ast_specframe( 'system=freq,unit=GHz', status )
      ff = ast_Fluxframe( 123.0D0, sf, 'Unit=Jy/deg**2', status )
      if( ast_getc( ff, 'system', status ) .ne. 'SFCBR' )
     :    call stopit( status, 'error 24' )
      sff = ast_specfluxframe( sf, ff, ' ', status )

      fs = ast_convert( sff, sff2, ' ', status )
      if( fs .eq. ast__null ) call stopit( status, 'error 25' )

      yin = 1.0D0
      xin = 2.0D0
      call ast_tran2( fs, 1, xin, yin, .true., xout, yout, status )

      if( abs(yout + 21.3585154D0 ) .gt. 0.000001 )
     :    call stopit( status, 'error 26' )

      if( xout .ne. 2.0D0 ) call stopit( status, 'error 27' )

      call ast_tran2( fs, 1, xout, yout, .false., xin, yin, status )
      if( abs( xin - 2.0D0 ) .gt. 1.0D-9 ) call stopit( status,
     :                                                  'error 28' )
      if( abs( yin - 1.0D0 ) .gt. 1.0D-9 ) call stopit( status,
     :                                                  'error 29' )

















      if( status .eq. sai__ok ) then
         write(*,*) 'All SpecFluxFrame tests passed'
      else
         write(*,*) 'SpecFluxFrame tests failed'
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


