      program testtime
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      character txt*40
      double precision xin, xout, ct, ctl, origin
      integer status, tf, tf1, tf2, fs, n, chr_len, nc
      status = sai__ok

      call ast_begin( status )

c      call ast_SetWatchId( 740050 )

c
c Test default attribute values
c
      tf = ast_timeframe( ' ', status )

      if( ast_getc( tf, 'System', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'System', status )
         call stopit( status, 'error 1' )
      endif

      if( ast_getd( tf, 'TimeOrigin', status ) .ne. 0.0 ) then
         write(*,*)  ast_getd( tf, 'TimeOrigin', status )
         call stopit( status, 'error 2' )
      endif

      if( ast_getc( tf, 'ObsLon', status ) .ne. 'E0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLon', status )
         call stopit( status, 'error 3' )
      endif

      if( ast_getc( tf, 'ObsLat', status ) .ne. 'N0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLat', status )
         call stopit( status, 'error 4' )
      endif

      if( ast_getc( tf, 'TimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'TimeScale', status )
         call stopit( status, 'error 5' )
      endif

      if( ast_getc( tf, 'AlignTimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'AlignTimeScale', status )
         call stopit( status, 'error 6' )
      endif

      if( ast_geti( tf, 'naxes', status ) .ne. 1 ) then
         write(*,*)  ast_getc( tf, 'Naxes', status )
         call stopit( status, 'error 7' )
      endif

      if( ast_getd( tf, 'Epoch', status ) .ne. 2000.0 ) then
         write(*,*)  ast_getd( tf, 'Epoch', status )
         call stopit( status, 'error 8' )
      endif

      if( ast_getc( tf, 'Label', status ) .ne.
     :                              'Modified Julian Date' ) then
         write(*,*)  ast_getc( tf, 'Label', status )
         call stopit( status, 'error 9' )
      endif

      if( ast_getc( tf, 'Symbol', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'Symbol', status )
         call stopit( status, 'error 10' )
      endif

      if( ast_getc( tf, 'Title', status ) .ne.
     :                              'Modified Julian Date' ) then
         write(*,*)  ast_getc( tf, 'Title', status )
         call stopit( status, 'error 11' )
      endif

      if( ast_getc( tf, 'unit', status ) .ne. 'd' ) then
         write(*,*)  ast_getc( tf, 'unit', status )
         call stopit( status, 'error 12' )
      endif

      if( ast_getc( tf, '  domain ', status ) .ne. 'TIME' ) then
         write(*,*)  ast_getc( tf, '  domain ', status )
         call stopit( status, 'error 13' )
      endif

      if( ast_getc( tf, 'alignSystem', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'alignSystem', status )
         call stopit( status, 'error 14' )
      endif

c
c Test dependency of default attribute values on System
c
      call ast_setc( tf, 'system', 'jd', status )

      if( ast_getc( tf, 'System', status ) .ne. 'JD' ) then
         write(*,*)  ast_getc( tf, 'System', status )
         call stopit( status, 'error 1b' )
      endif

      if( ast_getd( tf, 'TimeOrigin', status ) .ne. 0.0 ) then
         write(*,*)  ast_getd( tf, 'TimeOrigin', status )
         call stopit( status, 'error 2b' )
      endif

      if( ast_getc( tf, 'ObsLon', status ) .ne. 'E0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLon', status )
         call stopit( status, 'error 3b' )
      endif

      if( ast_getc( tf, 'ObsLat', status ) .ne. 'N0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLat', status )
         call stopit( status, 'error 4b' )
      endif

      if( ast_getc( tf, 'TimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'TimeScale', status )
         call stopit( status, 'error 5b' )
      endif

      if( ast_getc( tf, 'AlignTimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'AlignTimeScale', status )
         call stopit( status, 'error 6b' )
      endif

      if( ast_geti( tf, 'naxes', status ) .ne. 1 ) then
         write(*,*)  ast_getc( tf, 'Naxes', status )
         call stopit( status, 'error 7b' )
      endif

      if( ast_getd( tf, 'Epoch', status ) .ne. 2000.0 ) then
         write(*,*)  ast_getd( tf, 'Epoch', status )
         call stopit( status, 'error 8b' )
      endif

      if( ast_getc( tf, 'Label', status ) .ne.
     :                              'Julian Date' ) then
         write(*,*)  ast_getc( tf, 'Label', status )
         call stopit( status, 'error 9b' )
      endif

      if( ast_getc( tf, 'Symbol', status ) .ne. 'JD' ) then
         write(*,*)  ast_getc( tf, 'Symbol', status )
         call stopit( status, 'error 10b' )
      endif

      if( ast_getc( tf, 'Title', status ) .ne.
     :                              'Julian Date' ) then
         write(*,*)  ast_getc( tf, 'Title', status )
         call stopit( status, 'error 11b' )
      endif

      if( ast_getc( tf, 'unit', status ) .ne. 'd' ) then
         write(*,*)  ast_getc( tf, 'unit', status )
         call stopit( status, 'error 12b' )
      endif

      if( ast_getc( tf, '  domain ', status ) .ne. 'TIME' ) then
         write(*,*)  ast_getc( tf, '  domain ', status )
         call stopit( status, 'error 13b' )
      endif

      if( ast_getc( tf, 'alignSystem', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'alignSystem', status )
         call stopit( status, 'error 14b' )
      endif



      call ast_setc( tf, 'system', 'jepoch', status )

      if( ast_getc( tf, 'System', status ) .ne. 'JEPOCH' ) then
         write(*,*)  ast_getc( tf, 'System', status )
         call stopit( status, 'error 1c' )
      endif

      if( ast_getd( tf, 'TimeOrigin', status ) .ne. 0.0 ) then
         write(*,*)  ast_getd( tf, 'TimeOrigin', status )
         call stopit( status, 'error 2c' )
      endif

      if( ast_getc( tf, 'ObsLon', status ) .ne. 'E0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLon', status )
         call stopit( status, 'error 3c' )
      endif

      if( ast_getc( tf, 'ObsLat', status ) .ne. 'N0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLat', status )
         call stopit( status, 'error 4c' )
      endif

      if( ast_getc( tf, 'TimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'TimeScale', status )
         call stopit( status, 'error 5c' )
      endif

      if( ast_getc( tf, 'AlignTimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'AlignTimeScale', status )
         call stopit( status, 'error 6c' )
      endif

      if( ast_geti( tf, 'naxes', status ) .ne. 1 ) then
         write(*,*)  ast_getc( tf, 'Naxes', status )
         call stopit( status, 'error 7c' )
      endif

      if( ast_getd( tf, 'Epoch', status ) .ne. 2000.0 ) then
         write(*,*)  ast_getd( tf, 'Epoch', status )
         call stopit( status, 'error 8c' )
      endif

      if( ast_getc( tf, 'Label', status ) .ne.
     :                              'Julian Epoch' ) then
         write(*,*)  ast_getc( tf, 'Label', status )
         call stopit( status, 'error 9c' )
      endif

      if( ast_getc( tf, 'Symbol', status ) .ne. 'JEP' ) then
         write(*,*)  ast_getc( tf, 'Symbol', status )
         call stopit( status, 'error 10c' )
      endif

      if( ast_getc( tf, 'Title', status ) .ne.
     :                              'Julian Epoch' ) then
         write(*,*)  ast_getc( tf, 'Title', status )
         call stopit( status, 'error 11c' )
      endif

      if( ast_getc( tf, 'unit', status ) .ne. 'yr' ) then
         write(*,*)  ast_getc( tf, 'unit', status )
         call stopit( status, 'error 12c' )
      endif

      if( ast_getc( tf, '  domain ', status ) .ne. 'TIME' ) then
         write(*,*)  ast_getc( tf, '  domain ', status )
         call stopit( status, 'error 13c' )
      endif

      if( ast_getc( tf, 'alignSystem', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'alignSystem', status )
         call stopit( status, 'error 14c' )
      endif


      call ast_setc( tf, 'system', 'bepoch', status )

      if( ast_getc( tf, 'System', status ) .ne. 'BEPOCH' ) then
         write(*,*)  ast_getc( tf, 'System', status )
         call stopit( status, 'error 1d' )
      endif

      if( ast_getd( tf, 'TimeOrigin', status ) .ne. 0.0 ) then
         write(*,*)  ast_getd( tf, 'TimeOrigin', status )
         call stopit( status, 'error 2d' )
      endif

      if( ast_getc( tf, 'ObsLon', status ) .ne. 'E0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLon', status )
         call stopit( status, 'error 3d' )
      endif

      if( ast_getc( tf, 'ObsLat', status ) .ne. 'N0:00:00.00' ) then
         write(*,*)  ast_getc( tf, 'ObsLat', status )
         call stopit( status, 'error 4d' )
      endif

      if( ast_getc( tf, 'TimeScale', status ) .ne. 'TT' ) then
         write(*,*)  ast_getc( tf, 'TimeScale', status )
         call stopit( status, 'error 5d' )
      endif

      if( ast_getc( tf, 'AlignTimeScale', status ) .ne. 'TAI' ) then
         write(*,*)  ast_getc( tf, 'AlignTimeScale', status )
         call stopit( status, 'error 6d' )
      endif

      if( ast_geti( tf, 'naxes', status ) .ne. 1 ) then
         write(*,*)  ast_getc( tf, 'Naxes', status )
         call stopit( status, 'error 7d' )
      endif

      if( ast_getd( tf, 'Epoch', status ) .ne. 2000.0 ) then
         write(*,*)  ast_getd( tf, 'Epoch', status )
         call stopit( status, 'error 8d' )
      endif

      if( ast_getc( tf, 'Label', status ) .ne.
     :                              'Besselian Epoch' ) then
         write(*,*)  ast_getc( tf, 'Label', status )
         call stopit( status, 'error 9d' )
      endif

      if( ast_getc( tf, 'Symbol', status ) .ne. 'BEP' ) then
         write(*,*)  ast_getc( tf, 'Symbol', status )
         call stopit( status, 'error 10d' )
      endif

      if( ast_getc( tf, 'Title', status ) .ne.
     :                              'Besselian Epoch' ) then
         write(*,*)  ast_getc( tf, 'Title', status )
         call stopit( status, 'error 11d' )
      endif

      if( ast_getc( tf, 'unit', status ) .ne. 'yr' ) then
         write(*,*)  ast_getc( tf, 'unit', status )
         call stopit( status, 'error 12d' )
      endif

      if( ast_getc( tf, '  domain ', status ) .ne. 'TIME' ) then
         write(*,*)  ast_getc( tf, '  domain ', status )
         call stopit( status, 'error 13d' )
      endif

      if( ast_getc( tf, 'alignSystem', status ) .ne. 'MJD' ) then
         write(*,*)  ast_getc( tf, 'alignSystem', status )
         call stopit( status, 'error 14d' )
      endif

c
c Test dump and load
c
      call checkDump( tf, 'CheckDump 1', status )

c
c Test CurrentTime method.
c
      call ast_set( tf, 'system=jepoch,unit=yr,timescale=utc,'//
     :              'timeorigin=0', status )
      n = 0

      write(*,*) '   Testing astCurrentTime: approx 1 second pause '//
     :           'following...'
      ctl = ast_currenttime( tf, status ) + 1.0D0/(86400.0D0*365.25D0)
      do while( ast_currenttime( tf, status ) .lt. ctl )
         n = n + 1
         if( n .gt. 2000000 ) then
            call stopit( status, 'error 15' )
            return
         end if
      end do
      write(*,*) '   1 second pause finished.'


c
c Test behaviour of TimeOrigin attribute
c
      tf = ast_timeframe( 'timescale=utc', status )
      origin = ast_currenttime( tf, status )
      call ast_setd( tf, 'TimeOrigin', origin, status )
      write(*,*) '   Testing TimeOrigin: approx 1 second pause '//
     :           'following...'
      n = 0
      do while( ast_currenttime( tf, status ) .lt.
     :          1.0D0/(86400.0D0*364.25D0) )
         n = n + 1
         if( n .gt. 2000000 ) then
            call stopit( status, 'error 16' )
            return
         end if
      end do
      write(*,*) '   1 second pause finished.'

      call ast_set( tf, 'unit=s', status )
      if( abs( ast_getd( tf, 'TimeOrigin', status ) -
     :         origin*86400.0D0 ) .gt. 0.01 ) then
         write(*,*) abs( ast_getd( tf, 'TimeOrigin', status ) -
     :                   origin*86400.0D0 )
         call stopit( status, 'error 17' )
      end if


c
c  Test conversions between basic systems with arbitrary offsets
c
      tf1 = ast_timeframe( 'system=mjd,timeorigin=53000', status )
      tf2 = ast_timeframe( 'system=jd,timeorigin=2453000.5', status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 18' )
      else if( .not. ast_isaunitmap( ast_getMapping( fs, AST__BASE,
     :                                         AST__CURRENT,
     :                                         status ), status ) ) then
         call stopit( status, 'error 19' )
      end if



      tf1 = ast_timeframe( 'system=mjd,timescale=UTC,timeorigin=53000',
     :                     status )
      tf2 = ast_timeframe( 'system=bepoch,timeorigin=2004', status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 20' )
      else
         xin = 100.0D0
         call ast_tran1( fs, 1, xin, .true., xout, status )
         if( abs( xout - 0.2600974092354136D0 ) .gt. 1.0D-10 ) then
            call stopit( status, 'error 21' )
         end if
         call ast_tran1( fs, 1, xout, .false., xin, status )
         if( abs( xin  - 100.0D0 ) .gt. 1.0D-6 ) then
            call stopit( status, 'error 21b' )
         end if
      end if


      tf1 = ast_timeframe( 'system=bepoch,timeorigin=0', status )
      if( status .eq.sai__OK ) then
         call err_mark
         call ast_set( tf1, 'TimeScale=TAI', status )
         if( status .eq. AST__ATTIN ) then
            call err_annul( status )
         else
            call stopit( status, 'error 21b' );
         endif

         call ast_set( tf1, 'Unit=s', status )
         if( status .eq. AST__ATTIN ) then
            call err_annul( status )
         else
            call stopit( status, 'error 21c' );
         endif
         call err_rlse
      endif

      tf2 = ast_timeframe( 'system=jepoch,timescale=tai,'//
     :                     'timeorigin=100.0', status )
      call ast_set( tf2, 'unit=d', status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 22' )
      else
         xin = 100.0D0
         call ast_tran1( fs, 1, xin, .true., xout, status )
         if( abs( xout - 14.35534169996282 ) .gt. 1.0D-6 ) then
            call stopit( status, 'error 23' )
         end if
         call ast_tran1( fs, 1, xout, .false., xin, status )
         if( abs( xin  - 100.0D0 ) .gt. 1.0D-6 ) then
            call stopit( status, 'error 23b' )
         end if
      end if

c Besselian epoch offset from B2000 [TT, yr]
      call ast_set( tf1, 'timeorigin=2000', status )

c Julian date offset from 2450000.5 days [TDB, h]
      call ast_set( tf2, 'system=JD,timescale=TDB,unit=h,'//
     :              'timeorigin=2450000.5 d', status )

      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 24' )
      else
         xin = 0.1
         call ast_tran1( fs, 1, xin, .true., xout, status )
         if( abs( xout - 37933.38284478387D0) .gt. 1.0D-5 ) then
            call stopit( status, 'error 25' )
         end if
         call ast_tran1( fs, 1, xout, .false., xin, status )
         if( abs( xin  - 0.1 ) .gt. 1.0D-10 ) then
            call stopit( status, 'error 25b' )
         end if
      end if


c
c Test Formatting and unformatting
c
      tf1 = ast_timeframe( 'system=jepoch,timeorigin=2005.0', status )

      txt = ast_format( tf1, 1, 100.0D0, status )
      if( txt .ne. '100' ) then
         write(*,*) ast_format( tf1, 1, 100.0D0, status )
         call stopit( status, 'error 26' )
      end if
      nc = ast_unformat( tf1, 1, txt, xout, status )
      if( nc .ne. len( txt ) .or. xout .ne. 100.0D0 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 26b' )
      end if



      call ast_set( tf1, 'format=iso', status )
      txt = ast_format( tf1, 1, 1.0D0, status )
      if( txt .ne. '2006-01-01' ) then
         write(*,*) ast_format( tf1, 1, 1.0D0, status )
         call stopit( status, 'error 27' )
      end if

      nc = ast_unformat( tf1, 1, txt, xout, status )
      if( nc .ne. len( txt ) .or. xout .ne. 1.0D0 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 27b' )
      end if



      call ast_set( tf1, 'format=iso.0', status )
      txt = ast_format( tf1, 1, 1.0D0, status )
      if( txt .ne. '2006-01-01 00:00:00' ) then
         write(*,*) ast_format( tf1, 1, 1.0D0, status )
         call stopit( status, 'error 28' )
      end if

      nc = ast_unformat( tf1, 1, txt, xout, status )
      if( nc .ne. len( txt ) .or. xout .ne. 1.0D0 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 28b' )
      end if



      call ast_set( tf1, 'unit=s,format=iso.2', status )
      txt = ast_format( tf1, 1, 10.0D0, status )
      if( txt .ne. '2004-12-31 18:00:10.00' ) then
         write(*,*) ast_format( tf1, 1, 10.0D0, status )
         call stopit( status, 'error 29' )
      end if

      nc = ast_unformat( tf1, 1, txt, xout, status )
      if( nc .ne. len( txt ) .or.
     :    abs( xout - 10.0D0 ) .gt. 1.0E-3 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 29b' )
      end if



      txt = ast_format( tf1, 1, 10.12D0, status )
      if( txt .ne. '2004-12-31 18:00:10.12' ) then
         write(*,*) ast_format( tf1, 1, 10.12D0, status )
         call stopit( status, 'error 30' )
      end if

      nc = ast_unformat( tf1, 1, txt, xout, status )
      if( nc .ne. len( txt ) .or.
     :    abs( xout - 10.12D0 ) .gt. 1.0E-3 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 30b' )
      end if


      call ast_set( tf1, 'timescale=utc', status )
      xin = ast_currenttime( tf1, status )
      txt = ast_format( tf1, 1, xin, status )
      write(*,*) '   Current system time (UTC): ',
     :           txt( : chr_len( txt ) )
      nc = ast_unformat( tf1, 1, txt(:20), xout, status )
      if( nc .ne. 20 .or. abs( xout - xin ) .gt. 1.0E-3 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 30c' )
      end if

      tf1 = ast_timeframe( 'system=jepoch,timeorigin=2005.0', status )
      nc = ast_unformat( tf1, 1, 'J2005.0', xout, status )
      if( nc .ne. 7 .or. xout .ne. 0.0D0 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 31' )
      end if

      nc = ast_unformat( tf1, 1, 'J2010.0', xout, status )
      if( nc .ne. 7 .or. xout .ne. 5.0D0 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 32' )
      end if

      nc = ast_unformat( tf1, 1, '2005-jun-1 12:30 lunch time', xout,
     :                   status )
      if( nc .ne. 17 .or. abs( xout - 0.415525896D0 ) .gt. 1.0E-7 ) then
         write(*,*) nc, xout
         call stopit( status, 'error 33' )
      end if

      call ast_set( tf1, 'timescale=utc', status )
      nc = ast_unformat( tf1, 1, 'B2001.5 lunch time', xout,
     :                   status )
      if( nc .ne. 8 .or.
     :    abs( xout + 3.50131054408916D0 ) .gt. 1.0E-10 ) then
         write(*,*) nc, xout, abs( xout + 3.50131054408916D0 )
         call stopit( status, 'error 34' )
      end if






      tf1 = ast_timeframe( 'system=mjd,timescale=tai', status )
      nc = ast_unformat( tf1, 1, "1977-01-01 00:00:00", xin, status )


      tf2 = ast_timeframe( 'system=mjd,timescale=tai,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 35' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1977-01-01 00:00:00.000000' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 36' )
         end if
      end if

      tf2 = ast_timeframe( 'system=mjd,timescale=utc,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 37' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1976-12-31 23:59:45.000000' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 38' )
         end if
      end if

      tf2 = ast_timeframe( 'system=mjd,timescale=tt,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 39' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1977-01-01 00:00:32.184000' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 40' )
         end if
      end if

      tf2 = ast_timeframe( 'system=mjd,timescale=tdb,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 41' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1977-01-01 00:00:32.183935' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 42' )
         end if
      end if

      tf2 = ast_timeframe( 'system=mjd,timescale=tcb,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 43' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1977-01-01 00:00:32.184000' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 44' )
         end if
      end if

      tf2 = ast_timeframe( 'system=mjd,timescale=tcg,format=iso.6',
     :                     status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 45' )
      else
         call ast_tran1( fs, 1, xin, .true., xout, status )
         txt = ast_format( tf2, 1, xout, status )
         if( txt .ne. '1977-01-01 00:00:32.184000' ) then
            write(*,*) txt( :chr_len(txt) )
            call stopit( status, 'error 46' )
         end if
      end if




      tf1 = ast_timeframe( 'system=mjd,timescale=gmst,ObsLon=90,'//
     :                     'ObsLat=0,timeorigin=53000.0', status )
      tf2 = ast_timeframe( 'system=mjd,timescale=lmst,ObsLon=90,'//
     :                     'ObsLat=0,timeorigin=53000.0', status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 47' )
      else
         xin = 1.0D0
         call ast_tran1( fs, 1, xin, .true., xout, status )
         if( xout .ne. 1.25D0 ) then
            write(*,*) xout
            call stopit( status, 'error 48' )
         end if
         call ast_tran1( fs, 1, xout, .false., xin, status )
         if( xin .ne. 1.0D0 ) then
            write(*,*) xin
            call stopit( status, 'error 48b' )
         end if
      end if


*  Test use of DUT1
      tf1 = ast_timeframe( 'system=mjd,timescale=tdb,dut1=0.1', status )
      tf2 = ast_timeframe( 'system=mjd,timescale=last,dut1=0.1',
     :                      status )
      fs = ast_convert( tf1, tf2, ' ', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'error 49' )
      else
         xin = 53991.675D0
         call ast_tran1( fs, 1, xin, .true., xout, status )
         if( abs(xout - 53998.65344633732D0) .gt. 1.0D-8 ) then
            write(*,*) xout
            call stopit( status, 'error 50' )
         end if
         call ast_tran1( fs, 1, xout, .false., xin, status )
         if( abs( xin - 53991.675D0 ) .gt. 1.0D-8 ) then
            write(*,*) xin
            call stopit( status, 'error 51' )
         end if
      end if





      call ast_end( status )
c      call ast_listissued( 'testtime' )



      if( status .eq. sai__ok ) then
         write(*,*) 'All timeFrame tests passed'
      else
         write(*,*) 'timeFrame tests failed'
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



      if( ast_getd( obj, 'timeorigin', status ) .ne.
     :    ast_getd( result, 'timeorigin', status ) .or.
     :    ast_getc( obj, 'timescale', status ) .ne.
     :    ast_getc( result, 'timescale', status ) .or.
     :    ast_getc( obj, 'ObsLon', status ) .ne.
     :    ast_getc( result, 'ObsLon', status ) .or.
     :    ast_getc( obj, 'ObsLat', status ) .ne.
     :    ast_getc( result, 'ObsLat', status ) ) then
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


