      program teststcschan
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status

      status = sai__ok

      call ast_begin( status )

c      call ast_watchmemory( 209814 );

      call test2( status )
      call test1( status )




      call ast_end( status )
c      call ast_activememory( ' ' )
c      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         call msg_out( ' ', 'All StcsChan tests passed', status )
      else
         call err_rep( ' ', 'StcsChan tests failed', status )
      end if

      end





      subroutine test1( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'
      include 'PRM_PAR'

      integer iwrite
      character buff(30)*300
      common /bbb/ iwrite, buff

      integer iread, idoc
      common /aaa/ iread, idoc

      integer status, ch, obj, km, i, sb, iobj, nobj
      external source, sink

      double precision lbnd(4), ubnd(4)
      if( status .ne. sai__ok ) return

      call ast_begin( status )

      ch = ast_stcschan( source, sink, 'ReportLevel=3', status )


      idoc = 4
      iread = 0
      obj = ast_read( ch, status )

      idoc = 1
      iread = 0

      call err_mark
      obj = ast_read( ch, status )
      if( status .eq. AST__BADIN ) then
         call err_annul( status )
         call err_rlse
      else 
         call err_rlse
         call error( 'Failed to report error about "fred"', status )
      end if


      idoc = 2
      iread = 0
      obj = ast_read( ch, status )

      km = ast_warnings( ch, status )
      if( km .eq. AST__NULL ) call error( 'No Warnings keymap', status )
      call asserti( 'Warnings mapsize', ast_mapsize( km, status ), 4,
     :              status ) 

      call asserta( obj, 'Class', 'Prism', status )
      call asserta( obj, 'Naxes', '4', status )
      call asserta( obj, 'Label(1)', 'Modified Julian Date offset '//
     :              'from 1900-01-01', status )
      call asserta( obj, 'Label(2)', 'Right ascension', status )
      call asserta( obj, 'Label(3)', 'Declination', status )
      call asserta( obj, 'Label(4)', 'Wavelength', status )
      call asserta( obj, 'Unit(1)', 'd', status )
      call asserta( obj, 'Unit(4)', 'Angstrom', status )

      call ast_GetRegionBounds( obj, lbnd, ubnd, status )
      call ast_setc( obj, 'Format(1)', 'iso.2', status )
      call assertd( 'Time upper bounds', ubnd(1), VAL__MAXD, status )
      call assertc( 'Time lower bound', 
     :              ast_format( obj, 1, lbnd(1), status ),
     :              '1900-01-01 00:00:00.00', status )
      call assertd( 'RA lower bound', lbnd(2), 2.50080939227851D0, 
     :              status )
      call assertd( 'RA upper bound', ubnd(2), 2.6967811201606D0,
     :              status )
      call assertd( 'Dec lower bound', lbnd(3), 1.171115928088195D0,
     :              status )
      call assertd( 'Dec upper bound', ubnd(3), 1.24091013301998D0,
     :              status )
      call assertd( 'Wavelength lower bound', lbnd(4), 4000.0D0,
     :              status )
      call assertd( 'Wavelength upper bound', ubnd(4), 7000.0D0,
     :              status )



      idoc = 3
      iread = 0
      obj = ast_read( ch, status )

      call readast( 'stcschan-test1-doc3.ast', sb, status )
      if( .not. ast_equal( obj, sb, status ) ) then
         call error( 'Object read from doc3 is not equal to the '//
     :               'object read from file stcschan-test1-doc3.ast.', 
     :               status )
      end if


      call ast_setl( ch, 'StcsCoords', .true., status )
      call ast_setl( ch, 'StcsProps', .true., status )

      idoc = 3
      iread = 0
      obj = ast_read( ch, status )

      call asserta( obj, 'Class', 'KeyMap', status )
      call assert( 'Has PROPS entry', AST_MAPHASKEY( obj, 'PROPS', 
     :             status ), status )
      call assert( 'Has COORDS entry', AST_MAPHASKEY( obj, 'COORDS', 
     :             status ), status )

      if( ast_mapget0a( obj, 'AREA', iobj, status ) ) then
         call readast( 'stcschan-test1-doc3.ast', sb, status )
         if( .not. ast_equal( iobj, sb, status ) ) then
            call error( 'AREA read from doc3 is not equal to the '//
     :                  'object read from file stcschan-test1-doc3.ast', 
     :                  status )
         end if
      else
         call error( 'No AREA entry found', status )
      end if      

      if( ast_mapget0a( obj, 'PROPS', iobj, status ) ) then
         call readast( 'stcschan-test1-doc3-props.ast', sb, status )
         if( .not. ast_equal( iobj, sb, status ) ) then
            call error( 'PROPS read from doc3 is not equal to the '//
     :            'object read from file stcschan-test1-doc3-props.ast', 
     :            status )
         end if
      else
         call error( 'No PROPS entry found', status )
      end if      



      idoc = 5
      iread = 0

      call ast_setl( ch, 'StcsIndent', .true., status )

      obj = ast_read( ch, status )

      iwrite = 0
      nobj = ast_write( ch, obj, status )
      call asserti( 'N obj', nobj, 1, status ) 

      call assertc( 'line 1 3', buff(1), 
     :              'TimeInterval TT geocenter 1996-01-01T00:00:00 '//
     :              '1996-01-01T00:30:00', status )
      call assertc( 'line 2 3', buff(2), 
     :              '   Time MJD 50814.0 Error 1.2 Resolution 0.8 '//
     :              'PixSize 1024.0', status )
      call assertc( 'line 3 3', buff(3), 
     :              'Union ICRS GEOCENTER ( ', status )
      call assertc( 'line 4 3', buff(4), 
     :              '      Circle 180 10 20 ', status )
      call assertc( 'line 5 3', buff(5), 
     :              '      Circle 190 10 20 ', status )
      call assertc( 'line 6 3', buff(6), 
     :              '      Intersection ( ', status )
      call assertc( 'line 7 3', buff(7), 
     :              '         Circle 120 -10 20 ', status )
      call assertc( 'line 8 3', buff(8), 
     :              '         Difference ( ', status )
      call assertc( 'line 9 3', buff(9), 
     :              '            Circle 130 -10 20 ', status )
      call assertc( 'line 10 3', buff(10), 
     :              '            Circle 115 -10 10 ', status )
      call assertc( 'line 11 3', buff(11), 
     :              '         ) ', status )
      call assertc( 'line 12 3', buff(12), 
     :              '      ) ', status )
      call assertc( 'line 13 3', buff(13), 
     :              '   ) ', status )
      call assertc( 'line 14 3', buff(14), 
     :              '   Position 179.0 -11.5 Error 0.000889 0.000889 '//
     :              'Resolution 0.001778', status )
      call assertc( 'line 15 3', buff(15), 
     :              '   Size 0.000333 0.000278 PixSize 0.000083 '//
     :              '0.000083', status )
      call assertc( 'line 16 3', buff(16), 
     :              'Spectral BARYCENTER 1420.4 unit MHz Resolution '//
     :              '10.0 ', status )
      call assertc( 'line 17 3', buff(17), 
     :              'RedshiftInterval BARYCENTER VELOCITY OPTICAL '//
     :              '200 2300 Redshift 300', status )

      call ast_end( status )

      if( status .ne. sai__ok ) call err_rep( ' ', 'test1 failed.', 
     :                                        status )

      end


      subroutine test2( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'
      include 'PRM_PAR'

      integer iwrite
      character buff(30)*300
      common /bbb/ iwrite, buff

      integer iread, idoc
      common /aaa/ iread, idoc



      external source, sink

      integer status, ch, sf, unc, reg, nobj, obj, chr_len
      double precision p1(2), p2(2), p3(3), lbnd(2), ubnd(2)

      if( status .ne. sai__ok ) return


      call ast_begin( status )
      ch = ast_stcschan( source, sink, 'ReportLevel=3',status )
      call ast_setl( ch, 'StcsIndent', .true., status )
      call ast_seti( ch, 'StcsLength', 60, status )

      sf = ast_skyframe( ' ', status );
      p1( 1 ) = 0.0
      p1( 2 ) = 1.3
      p2( 1 ) = 0.01

      unc = ast_circle( sf, 1, p1, p2, AST__NULL, ' ', status )

      p1( 1 ) = 1.3
      p1( 2 ) = 0.5
      p2( 1 ) = 0.3
      p2( 2 ) = 0.1
      p3( 1 ) = 1.0
      reg = ast_ellipse( sf, 1, p1, p2, p3, unc, ' ', status )

      iwrite = 0
      nobj = ast_write( ch, reg, status )
      call asserti( 'N obj', nobj, 1, status ) 
      call asserti( 'iwrite', iwrite, 2, status ) 
      call assertc( 'line 1', buff(1), 'Ellipse ICRS TOPOCENTER '//
     :              '74.48451 28.64789 17.18873 5.729578', status )
      call assertc( 'line 2', buff(2), '   57.29578 Error 0.5729514 '//
     :              '0.5726735', status )

      call ast_set( ch, 'StcsCoords=1,StcsProps=1', status )

      idoc = 3
      iread = 0
      obj = ast_read( ch, status )

      if( obj .ne. AST__NULL ) then
         iwrite = 0
         nobj = ast_write( ch, obj, status )
         call asserti( 'N obj 2', nobj, 1, status ) 

         call assertc( 'line 1 2', buff(1), 'TimeInterval TT '//
     :                 'GEoCENTER 1996-01-01T00:00:00', status )

         call assertc( 'line 2 2', buff(2), '   1996-01-01T00:30:00 '//
     :                 'Time MJD 50814.0 Error 1.2', status );

         call assertc( 'line 3 2', buff(3), '   Resolution 0.8 '//
     :                 'PixSize 1024.0', status )

         call assertc( 'line 4 2', buff(4), 'Circle ICRS GEOCENTER '//
     :                 '179.0 -11.5 0.5 Position 179.0 -11.5', status )

         call assertc( 'line 5 2', buff(5), '   Error 0.000889 '//
     :                 '0.000889 Resolution 0.001778', status )

         call assertc( 'line 6 2', buff(6), '   Size 0.000333 '//
     :                 '0.000278 PixSize 0.000083 0.000083', status )

         call assertc( 'line 7 2', buff(7), 'Spectral BARYCENTER '//
     :                 '1420.4 unit MHz Resolution 10.0', status )

         call assertc( 'line 8 2', buff(8), 'RedshiftInterval '//
     :                 'BARYCENTER VELOCITY OPTICAL 200 2300 '//
     :                 'Redshift 300', status )

         call assertc( 'line 9 2', buff(9), '   Resolution 0.7 '//
     :                 'PixSize 0.3', status )

      else      
         write(*,*) 'No object read from doc 3'
      end if


      call ast_end( status )

      if( status .ne. sai__ok ) call err_rep( ' ', 'test2 failed.', 
     :                                        status )

      end














      subroutine source( status )
      implicit none

      integer iread, idoc
      common /aaa/ iread, idoc

      logical done
      integer status, l, chr_len
      character c*80

      c = ' '
      done = .false.

      if( idoc .eq. 1 ) then
         if( iread .eq. 0 ) then
            c = 'StartTime 1900-01-01 Circle ICRS 148.9 69.1 2.0 fred'
         else if( iread .eq. 1 ) then
            c = 'SpectralInterval 4000 7000 unit Angstrom'
         else
            done = .true.
         end if

      else if( idoc .eq. 2 ) then
         if( iread .eq. 0 ) then
            c = 'StartTime 1900-01-01 Circle ICRS 148.9 69.1 2.0 '
         else if( iread .eq. 1 ) then
            c = 'SpeCtralInterval 4000 7000 unit Angstrom'
         else
            done = .true.
         end if

      else if( idoc .eq. 3 ) then
         if( iread .eq. 0 ) then
            c = 'TimeInterVal TT GEoCENTER'
         else if( iread .eq. 1 ) then
            c = '1996-01-01T00:00:00 1996-01-01T00:30:00'
         else if( iread .eq. 2 ) then
            c = 'Time MJD 50814.0 Error 1.2'
         else if( iread .eq. 3 ) then
            c = 'Resolution 0.8 PixSize 1024.0'
         else if( iread .eq. 4 ) then
            c = 'Circle ICRS GEOCENTER 179.0 -11.5 0.5'
         else if( iread .eq. 5 ) then
            c = 'Position 179.0 -11.5 Error 0.000889'
         else if( iread .eq. 6 ) then
            c = 'Resolution 0.001778 Size 0.000333 0.000278'
         else if( iread .eq. 7 ) then
            c = 'PixSIZE 0.000083 0.000083'
         else if( iread .eq. 8 ) then
            c = 'Spectral BARYCENTER 1420.4 unit MHz'
         else if( iread .eq. 9 ) then
            c = 'Resolution 10.0'
         else if( iread .eq. 10 ) then
            c = 'RedshiftInterval BARYCENTER VELOCITY OPTICAL'
         else if( iread .eq. 11 ) then
            c = '200.0 2300.0 Redshift 300.0'
         else if( iread .eq. 12 ) then
            c = 'Resolution 0.7 PixSize 0.3'
         else
            done = .true.
         end if

      else if( idoc .eq. 4 ) then
         if( iread .eq. 0 ) then
            c = 'TimeInterval TT GEOCENTER'
         else if( iread .eq. 1 ) then
            c = '1996-01-01T00:00:00 1996-01-01T00:30:00'
         else if( iread .eq. 2 ) then
            c = 'Time mjd 50814.0 ERROR 1.2'
         else if( iread .eq. 3 ) then
            c = 'Resolution 0.8 PixSize 1024.0'
         else if( iread .eq. 4 ) then
            c = 'Spectral barycenter 1420.4 UNIT MHz'
         else if( iread .eq. 5 ) then
            c = 'Resolution 10.0'
         else
            done = .true.
         end if

*  Like doc 3 but with a compound spatial region
      else if( idoc .eq. 5 ) then
         if( iread .eq. 0 ) then
            c = 'tIMEiNTERVAL tt geocenter'
         else if( iread .eq. 1 ) then
            c = '1996-01-01T00:00:00 1996-01-01T00:30:00'
         else if( iread .eq. 2 ) then
            c = 'Time MJD 50814.0 Error 1.2'
         else if( iread .eq. 3 ) then
            c = 'Resolution 0.8 PixSize 1024.0'
         else if( iread .eq. 4 ) then
            c = ' '
         else if( iread .eq. 5 ) then
            c = 'Union ICRS GEOCENTER'
         else if( iread .eq. 6 ) then
            c = '      (Circle 180 10 20'
         else if( iread .eq. 7 ) then
            c = '       Circle 190 10 20'
         else if( iread .eq. 8 ) then
            c = '       Intersection ('
         else if( iread .eq. 9 ) then
            c = '          cIRCLE 120 -10 20 dIFFERENCE '
         else if( iread .eq. 10 ) then
            c = '          ( Circle 130 -10 20 '
         else if( iread .eq. 11 ) then
            c = '            Circle 115 -10 10 '
         else if( iread .eq. 12 ) then
            c = '          )'
         else if( iread .eq. 13 ) then
            c = '          Not (Circle 118 -8 3)'
         else if( iread .eq. 14 ) then
            c = '       )'
         else if( iread .eq. 15 ) then
            c = '      )'
         else if( iread .eq. 16 ) then
            c = 'Position 179.0 -11.5 Error 0.000889'
         else if( iread .eq. 17 ) then
            c = 'Resolution 0.001778 Size 0.000333 0.000278'
         else if( iread .eq. 18 ) then
            c = 'PixSize 0.000083 0.000083'
         else if( iread .eq. 19 ) then
            c = 'Spectral BARYCENTER 1420.4 unit MHz'
         else if( iread .eq. 20 ) then
            c = 'rESOLUTION 10.0'
         else if( iread .eq. 21 ) then
            c = 'rEDSHIFTiNTERVAL barycenter velocity optical'
         else if( iread .eq. 22 ) then
            c = '200.0 2300.0 rEDSHIFT 300.0'
         else if( iread .eq. 23 ) then
            c = 'Resolution 0.7 PixSize 0.3'
         else
            done = .true.
         end if

      end if

      l = max( chr_len( c ), 1 )
      if( .not. done ) then
         call ast_putline( c, l, status )
         iread = iread + 1
      else
         call ast_putline( ' ', -1, status )
      end if

      end        







      subroutine sink( status )
      implicit none

      integer iwrite
      character buff(30)*300
      common /bbb/ iwrite, buff

      integer status, l
      character line*300

      call ast_getline( line, l, status ) 
      if( l .gt. 0 ) then
         if( iwrite .lt. 0 ) then
            write(*,*) line( : l ) 
         else
            iwrite = iwrite + 1
            buff( iwrite ) = ' '
            buff( iwrite ) = line( : l )
         end if
      end if

      end      


      subroutine asserta( obj, anam, asb, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer obj, status
      character anam*(*), asb*(*), aval*80

      aval = ast_GetC( obj, anam, status )

      if( aval .ne. asb ) then
         call msg_setc( 'A', anam )
         call msg_setc( 'B', aval )
         call msg_setc( 'C', asb )
         call error( '^A (^B) should be "^C".', status )
      end if

      end

      subroutine assertc( name, val, sb, status )
      implicit none
      include 'SAE_PAR'
      integer status, i
      character name*(*), val*(*), sb*(*)
      character blank*500

      if( val .ne. sb .and. status .eq. sai__ok ) then
         call msg_setc( 'A', name )
         call msg_setc( 'B', val )
         call error( '^A (^B) should be:', status )

         i = 1
         blank = ' '
         do while( val( i : i ) .eq. sb( i : i ) ) 
            i = i + 1
         end do
         blank( i : i ) = '^'

         write(*,*) sb
         write(*,*) blank( : i + 2 )

      end if

      end

      subroutine asserti( name, val, sb, status )
      implicit none
      include 'SAE_PAR'
      integer status
      character name*(*)
      integer val, sb

      if( val .ne. sb ) then
         call msg_setc( 'A', name )
         call msg_seti( 'B', val )
         call msg_seti( 'C', sb )
         call error( '^A (^B) should be ^C.', status )
      end if

      end

      subroutine assert( name, val, status )
      implicit none
      include 'SAE_PAR'
      integer status
      character name*(*)
      logical val

      if( .not. val ) then
         call msg_setc( 'A', name )
         call error( '^A is not true.', status )
      end if

      end

      subroutine assertd( name, val, sb, status )
      implicit none
      include 'SAE_PAR'
      integer status
      character name*(*)
      double precision val, sb

      if( abs( val - sb ) .gt. 0.5E-8*( val + sb ) ) then
         call msg_setc( 'A', name )
         call msg_setd( 'B', val )
         call msg_setd( 'C', sb )
         call error( '^A (^B) should be ^C.', status )
      end if

      end

      subroutine error( text, status )
      implicit none
      include 'SAE_PAR'
      integer status
      character text*(*)

      if( status .eq. sai__ok ) then
         status = sai__error
         call err_rep( ' ', text, status )
      end if

      end


      subroutine readast( file, obj, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer obj, status, channel
      character file*(*)
      external rsource

      if( status .ne. sai__ok ) return

      open( 10, file=file, status='old' )
      channel = ast_channel( rsource, ast_null, ' ', status )
      obj = ast_read( channel, status )
      call ast_annul( channel, status )
      close( 10 )

      end


      subroutine rsource( status )
      integer status
      character buffer*200

      read( 10, '(a)', end = 99 ) buffer
      call ast_putline( buffer, len( buffer ), status )
      return

 99   call ast_putline( buffer, -1, status )      
      end


