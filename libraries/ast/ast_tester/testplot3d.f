      program testplot3d
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer fset, plot3d, status, pgbeg
      real gbox(6), lbnd(3), ubnd(3)
      double precision bbox(6)
      integer readtest
      logical ok, pg3d_autocamera
      character device*30

      status = sai__ok

c      call ast_watchmemory( 130836 )
      call ast_begin( status )

      lbnd( 1 ) = -1.0
      lbnd( 2 ) = -1.0
      lbnd( 3 ) = -1.0
      ubnd( 1 ) = 1.0
      ubnd( 2 ) = 1.0
      ubnd( 3 ) = 1.0

      if( iargc() .gt. 0 ) then
         call getarg( 1, device )
      else
         device = '/XSERVE'
      end if
      ok = ( pgbeg( 0, device, 1, 1 ) .eq. 1 )

      if( .not. ok ) write(*,*) 'PGPLOT OPEN FIALED'

      call pgask( .false. )

      call pgpage
      call pgwnad( 0.0, 1.0, 0.0, 1.0 )

      ok = pg3d_autocamera( lbnd, ubnd )
c
      gbox(1) = lbnd(1)
      gbox(2) = lbnd(2)
      gbox(3) = lbnd(3)
      gbox(4) = ubnd(1)
      gbox(5) = ubnd(2)
      gbox(6) = ubnd(3)

      bbox(1) = -1.0
      bbox(2) = -1.0
      bbox(3) = -1.0
      bbox(4) = 1.0
      bbox(5) = 1.0
      bbox(6) = 1.0

      plot3d = ast_plot3d( AST__NULL, gbox, bbox, 'minticklen=0',
     :                     status )
      call checkdump( plot3d, 'CheckDump test 1', status )
      call ast_annul( plot3d, status )
      bbox(1) = 0.5
      bbox(2) = 0.5
      bbox(3) = 0.5
      bbox(4) = 155.5
      bbox(5) = 107.5
      bbox(6) = 1640.5
      plot3d = ast_plot3d( readtest( "plot3d-test1.ast", status ), gbox,
     :                      bbox, ' ' , status )
c      call checkdump( plot3d, 'CheckDump test 2', status )
c      call ast_set( plot3d, "System(1)=galactic,system(3)=freq",
c     :              status )
c      call ast_grid( plot3d, status )
      call explore( plot3d, status )
      call pgend
      call ast_end( status )
      call ast_activememory( 'testplot3d' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All Plot3D tests passed'
      else
         write(*,*) 'Plot3D tests failed'
      end if

      end

      subroutine checkdump( obj, text, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*)
      integer obj, status, next, end, ch, result, ll, overlap
      external mysource, mysink, mysink2
      character buf*400000,buf2*400000
      common /ss1/ buf
      common /ss2/ next, end, ll
      common /ss3/ buf2
      if( status .ne. sai__ok ) return
      call ast_begin( status )
      ch = ast_channel( mysource, mysink, ' ', status )
      ll = 200
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

      ll = 200
      next = 1
      ch = ast_channel( AST_NULL, mysink2, ' ', status )
      if( ast_write( ch, obj, status ) .ne.1 ) then
         write(*,*) text
         call stopit( status, 'Cannot write copied object to '//
     :                'channel' )
      end if
      if( buf .ne. buf2  ) then
         call ast_Show( obj, status )
         call ast_Show( result, status )
         write(*,*) text
         call stopit( status, 'Object has changed' )
      end if
      call ast_end( status )
      end
      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll
      character buf*400000
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
      character buf*400000
      character line*1000
      common /ss1/ buf
      common /ss2/ next, end, ll
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
         write(*,*) line( f : l )
         write(*,*) 'Line length ',l
         call stopit( status, 'Line overflow in mysink!!' )
      else
         end = next + l
         buf( end : next + ll - 1 ) = ' '
      endif
      next = next + ll
      end
      subroutine mysink2( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll
      character buf2*400000
      character line*1000
      common /ss3/ buf2
      common /ss2/ next, end, ll
      if( status .ne. sai__ok ) return
      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf2( next : ) = line( f : l )
      l = l - f + 1
      if( next + ll - 1 .ge. 400000 ) then
         write(*,*)
         call stopit( status, 'Buffer overflow in mysink2!!' )
      else if( l .gt. ll ) then
         write(*,*)
         write(*,*) buf2( next : next + l)
         write(*,*) 'Line length ',l
         call stopit( status, 'Line overflow in mysink2!!' )
      else
         end = next + l
         buf2( end : next + ll - 1 ) = ' '
      endif
      next = next + ll
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

      integer function readtest( name, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character name*(*)
      integer status, ch
      external mysource3
      readtest = AST__NULL
      if( status .ne. sai__ok ) return
      open( unit=1, file=name, status='old' )
      ch = ast_channel( mysource3, ast_null, ' ', status )
      readtest = ast_read( ch, status )
      call ast_annul( ch, status )
      close(1)
      end

      subroutine mysource3( status )
      integer status
      character buffer*200
      read( 1, '(A)', end = 99 ) buffer
      call ast_putline( buffer, len( buffer ), status )
      return
 99   call ast_putline( buffer, -1, status )

      end


      subroutine explore( plot3d, status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      real common_par
      integer common_map
      logical use_common
      double precision common_x, common_y
      common /ss4/ common_par,common_x,common_y,common_map,use_common

      real pi
      parameter( pi = 3.1415927 )
      integer diag, plot3d, status, iopt, i, iclose
      logical more, draw, ok, pg3d_findnearest, lval, pg3d_seteye,
     :        pg3d_setup
      character settings*40, attr*20, corn(8)*3, text*40,just*4
      real xc(8), yc(8), zc(8)
      double precision pos(3),x,y
      real up(3), eye(3)
      integer type, map
      integer readtest
      data xc /-1, 1, -1, 1, -1, 1, -1, 1 /
      data yc /-1, -1, 1, 1, -1, -1, 1, 1 /
      data zc /-1, -1, -1, -1, 1, 1, 1, 1 /
      data corn/ 'lll', 'ull', 'lul', 'uul',
     :           'llu', 'ulu', 'luu', 'uuu' /
      if( status .ne. sai__ok ) return
      eye( 1 ) = -3
      eye( 2 ) = 4
      eye( 3 ) = 3
      ok = pg3d_seteye( eye )
      up( 1 ) = 0.0
      up( 2 ) = -0.03
      up( 3 ) = 1.0
      ok = pg3d_setup( up )
      diag = 1
      common_x = AST__BAD
      common_y = AST__BAD
      common_map = AST__NULL
      use_common = .false.

      call drawit( diag, plot3d, status )
      more = .true.
      do while( more )

         write(*,*) '  0 - exit'
         write(*,*) '  1 - move forward'
         write(*,*) '  2 - move backwards'
         write(*,*) '  3 - rotate right'
         write(*,*) '  4 - rotate left'
         write(*,*) '  5 - rotate up'
         write(*,*) '  6 - rotate down'
         write(*,*) '  7 - rotate right continuous'
         write(*,*) '  8 - set attribute values'
         write(*,*) '  9 - get an attribute value'
         write(*,*) '  10 - clear an attribute value'
         write(*,*) '  11 - test an attribute value'
         write(*,*) '  12 - show the plot3d (big!)'
         write(*,*) '  13 - Draw a border'
         write(*,*) '  14 - Draw a marker'
         write(*,*) '  15 - Draw a text string'
         write(*,*) '  16 - Draw az-el diagram'
         write(*,*) '  17 - Draw grid-box diagram'
         write(*,*) '  18 - Vary parameter continuously'
         write(*,*) '  19 - Next figure'
         write(*,*) '  20 - Set Mapping'
         write(*,*) '  21 - Mark mapped position'
         write(*,'(A,$)' ) 'Option: '
         read(*,*) iopt

         draw = .true.
         if( iopt .eq. 0 ) then
            draw = .false.
            more = .false.
         else if( iopt .eq. 1 ) then
            call pg3d_forward( 0.1 )
         else if( iopt .eq. 2 ) then
            call pg3d_forward( -0.1 )
         else if( iopt .eq. 3 ) then
            call pg3d_rotateeye( 4, 7.0 )
         else if( iopt .eq. 4 ) then
            call pg3d_rotateeye( 3, 7.0 )
         else if( iopt .eq. 5 ) then
            call pg3d_rotateeye( 1, 7.0 )
         else if( iopt .eq. 6 ) then
            call pg3d_rotateeye( 2, 7.0 )
         else if( iopt .eq. 7 ) then
            do i = 0, 360, 7
               call pg3d_rotateeye( 4, 7.0 )
               call pgpage
               call drawit( diag, plot3d, status )
               call pause
            end do
            draw = .false.
         else if( iopt .eq. 8 ) then
            write( *, '(A,$)' ) 'Attribute settings: '
            read( *, '(A)' ) settings
            call ast_set( plot3d, settings, status )
         else if( iopt .eq. 9 ) then
            write( *, '(A,$)' ) 'Attribute name to get: '
            read( *, '(A)' ) attr
            write(*,*) ast_getc( plot3d, attr, status )
            draw = .false.
         else if( iopt .eq. 10 ) then
            write( *, '(A,$)' ) 'Attribute name to clear: '
            read( *, '(A)' ) attr
            call ast_clear( plot3d, attr, status )
         else if( iopt .eq. 11 ) then
            write( *, '(A,$)' ) 'Attribute name to test: '
            read( *, '(A)' ) attr
            write(*,*) ast_test( plot3d, attr, status )
            draw = .false.
         else if( iopt .eq. 12 ) then
            call ast_show( plot3d, status )
            draw = .false.
         else if( iopt .eq. 13 ) then
            lval = ast_border( plot3d, status )
            draw = .false.
         else if( iopt .eq. 14 ) then
            write(*,'(A,$)') 'Marker X coord: '
            read(*,*) pos(1)
            write(*,'(A,$)') 'Marker Y coord: '
            read(*,*) pos(2)
            write(*,'(A,$)') 'Marker Z coord: '
            read(*,*) pos(3)
            write(*,'(A,$)') 'Marker type: '
            read(*,*) type
            call ast_mark( plot3d, 1, 3, 1, pos, type, status )
            draw = .false.
         else if( iopt .eq. 15 ) then
            write(*,'(A,$)') 'Text X coord: '
            read(*,*) pos(1)
            write(*,'(A,$)') 'Text Y coord: '
            read(*,*) pos(2)
            write(*,'(A,$)') 'Text Z coord: '
            read(*,*) pos(3)
            write(*,'(A,$)') 'Text: '
            read(*,*) text
            write(*,'(A,$)') 'Justification: '
            read(*,*) just
            write(*,'(A,$)') 'Up X: '
            read(*,*) up(1)
            write(*,'(A,$)') 'Up Y: '
            read(*,*) up(2)
            write(*,'(A,$)') 'Up Z: '
            read(*,*) up(3)
            call ast_text( plot3d, text, pos, up, just, status )
            draw = .false.
         else if( iopt .eq. 16 ) then
            diag = 2
         else if( iopt .eq. 17 ) then
            diag = 1
         else if( iopt .eq. 18 ) then
            use_common = .true.
            do i = 0, 90, 1
               common_par = pi*i/180.0;
               call pgpage
               call drawit( diag, plot3d, status )
               call pause
            end do
            draw = .false.
            use_common = .false.
         else if( iopt .eq. 19 ) then
            diag = diag + 1
            write(*,*) 'Drawing diag ',diag

         else if( iopt .eq. 20 ) then
            write(*,'(A,$)') 'Supply a 2-in, 2-out Mapping (outputs '//
     :                       'are (long,lat)):'
            read(*,*) text
            common_map = readtest( text, status )
            draw = .false.

         else if( iopt .eq. 21 ) then
            write(*,'(A,$)') 'Value for input 1: '
            read(*,*) common_x
            write(*,'(A,$)') 'Value for input 2: '
            read(*,*) common_y

         else
            draw = .false.
            write(*,*) 'Bad option ',iopt
         end if

         if( draw ) then
            call pgpage
c            ok = pg3d_findnearest( 8, xc, yc, zc, iclose )
c            call ast_setc( plot3d, 'RootCorner', corn( iclose + 1 ),
c     :                     status )
            call drawit( diag, plot3d, status )
         end if

         if( status .ne. sai__ok ) call err_flush( status )
      end do

      end

      subroutine pause
      implicit none
      integer i
      double precision a
      a = 1.0D0
      do i = 1, 2000000
         a = tan( a )
      end do
      end

      subroutine drawit( diag, plot3d, status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'
      integer NP
      parameter ( NP = 50 )
      integer w1, w2
      real rd, pi
      parameter( w1 = 6, w2 = 3, rd = 1.5, pi = 3.1415927 )

      real common_par
      integer common_map
      logical use_common
      double precision common_x, common_y
      common /ss4/ common_par,common_x,common_y,common_map,use_common

      double precision dlat, dlon, up, north
      integer diag, plot3d, status, i, j, azelmap
      real x(NP), y(NP), z(NP), axlen, a1, a2, r(3), u(3), n(3),
     :     ang, k, cen(3), norm(3), start(3), lon, lat, origin(3),
     :     theta,phi
      if( status .ne. sai__ok ) return

      if( diag .eq. 1 ) then
         call ast_grid( plot3d, status )

      else
         axlen = 1.8
         a1 = 0.1
         a2 = a1*0.6
         lon = pi*210.0/180.0
         if( use_common ) then
            lat = common_par
         else
            lat = pi*25.0/180.0
         endif

         do while( lat .gt. pi )
            lat = lat - 2*pi
         end do

         do while( lat .lt. -pi )
            lat = lat + 2*pi
         end do

         if( lat .gt. pi/2 ) then
            lat = pi - lat
            lon = lon + pi

         else if( lat .lt. -pi/2 ) then
            lat = -pi - lat
            lon = lon + pi
         end if

         do while( lon .gt. 2*pi )
            lon = lon - 2*pi
         end do

         do while( lon .lt. 0 )
            lon = lon + 2*pi
         end do


c+  Fig1  (part a)
         if( diag .eq. 2 ) then
             call pgslw( w2 )
             cen(1) = 0.0
             cen(2) = 0.0
             cen(3) = 0.0
             call s2c( lon - pi/2, 0.0, 1.0, norm )
             call s2c( lon, 0.0, rd*0.6, start )
             call pgsci( 10 )
             call pgscr( 10, 0.0, 0.0, 1.0 )
             call arc( cen, norm, start, -lat, -rd*0.1 )

             u(1)=0
             u(2)=0
             u(3)=1
             call s2c( lon, lat/2.2, rd*0.7, r )
             call g3dtext( 'el', r, 'CL', u, norm )

             norm(1)=0
             norm(2)=0
             norm(3)=-1
             start(1)=rd*0.6
             start(2)=0
             start(3)=0
             call arc( cen, norm, start, lon, rd*0.1 )

             call s2c( 110.0/180.0*PI, 0.0, rd*0.7, r )
             call g3dtext( 'az', r, 'TC', r, norm )

             call pgsci( 1 )
         endif

         origin(1) = 0.0
         origin(2) = 0.0
         origin(3) = 0.0
         call axes( origin, axlen, '(north) u', '(east) v',
     :              '(zenith) w', w1, w2, 'X', 0.0, 0.0, 0.0 )

*  Quadrant from (0,0,1) to (-1,0,0)
         cen(1) = 0.0
         cen(2) = 0.0
         cen(3) = 0.0
         call s2c( (int(2*lon/pi)-1)*pi/2, 0.0, 1.0, norm )
         start(1) = 0.0
         start(2) = 0.0
         start(3) = rd
         call pgslw( w2 )
         call arc( cen, norm, start, pi/2, 0.0 )

*  Quadrant from (0,0,1) to (0,-1,0)
         call s2c( int(2*lon/pi)*pi/2, 0.0, 1.0, norm )
         call arc( cen, norm, start, pi/2, 0.0 )

*  Quadrant from (-1,0,0) to (0,-1,0)
         norm(1) = 0.0
         norm(2) = 0.0
         norm(3) = 1.0
         call s2c( (int(2*lon/pi)+1)*pi/2, 0.0, rd, start )
         call arc( cen, norm, start, pi/2, 0.0 )

*  Dashed red quadrant at az = lon degs
         call s2c( lon-pi/2, 0.0, 1.0, norm )
         start(1) = 0.0
         start(2) = 0.0
         start(3) = rd
         call pgsci( 10 )
         call pgscr( 10, 1.0, 0.0, 0.0 )
         call pgsls( 2 )
         call pgslw( 2 )
         call arc( cen, norm, start, pi/2, 0.0 )

*  Dashed red line of latitude at el = lat degs
         cen(1) = 0.0
         cen(2) = 0.0
         cen(3) = sin(lat)*rd
         norm(1) = 0.0
         norm(2) = 0.0
         norm(3) = 1.0
         call s2c( (int(2*lon/pi)+1)*pi/2, lat, rd, start )
         call arc( cen, norm, start, pi/2, 0.0 )

*  Dashed red line from centre to az=lon el=0
         x( 1 ) = 0.0
         y( 1 ) = 0.0
         z( 1 ) = 0.0
         x( 2 ) = cos(lon)*rd
         y( 2 ) = sin(lon)*rd
         z( 2 ) = 0.0
         call g3dline( 2, x, y, z )

*  Solid red line from centre to az=210 el=45
         x( 1 ) = 0.0
         y( 1 ) = 0.0
         z( 1 ) = 0.0
         call s2c( lon, lat, rd, origin )
         x( 2 ) = origin(1)
         y( 2 ) = origin(2)
         z( 2 ) = origin(3)
         call pgsls( 1 )
         call g3dline( 2, x, y, z )

c+ Fig1 - (part b)
         if( diag .eq. 2 ) then
            call pgscr( 10, 0.0, 0.6, 0.0 )
            call s2c( lon, lat, rd, origin )
            call axes( origin, 0.2*axlen, 'daz', 'del', ' ', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), pi/2 )
         endif


c+ Fig2
         if( diag .eq. 3 ) then
            call pgscr( 10, 0.6, 0.6, 0.0 )
            call s2c( lon, lat, rd, origin )
            call axes( origin, 0.4*axlen, 'UP', 'N', ' ', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), lat )


            call pgscr( 10, 0.0, 0.0, 1.0 )
            call s2c( lon+0.22, lat, rd, start )
            call s2c( lon, lat, rd, cen )
            call s2c( lon, lat, rd, norm )
            call arc( cen, norm, start, -(lat-0.05), -rd*0.02 )

            call s2c( lon+0.25, lat*1.1, rd, r )
            norm(1) = 0
            norm(2) = 0
            norm(3) = 0
            u(1) = 0
            u(2) = 0
            u(3) = 1.0
            call g3dtext( 'el', r, 'CR', u, norm )

         endif


c+ Fig3
         if( diag .eq. 4 ) then
            call pgscr( 10, 0.0, 0.0, 1.0 )
            call s2c( lon, lat, rd, origin )
            do i = 1, 4
               theta = i*5.0*pi/180.0
               cen(1) = cos(theta)*origin(1)
               cen(2) = cos(theta)*origin(2)
               cen(3) = cos(theta)*origin(3)
               call s2c( lon + theta, lat, rd, start )
               call arc( cen, cen, start, 2*pi, 0.05 )
            end do


            cen(1)=0
            cen(2)=0
            cen(3)=0

            call s2c( pi, pi/2-pi/2, rd, start )
            call s2c( pi+pi/2, 0.0, 1.0, norm )

            call rotvec( 'ZYZ', lon, pi/2-lat, lat, norm )
            call rotvec( 'ZYZ', lon, pi/2-lat, lat, start )
            call arc( cen, norm, start, pi/2, 0.0 )


            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, rd, origin )
            call axes( origin, 0.2*axlen, 'y', 'x', ' ', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), lat )
         endif


c+ Fig4
         if( diag .eq. 5 ) then
            call pgscr( 10, 0.0, 0.0, 1.0 )
            call s2c( lon, lat, rd, origin )
            do i = 1, 4
               theta = i*5.0*pi/180.0
               cen(1) = cos(theta)*origin(1)
               cen(2) = cos(theta)*origin(2)
               cen(3) = cos(theta)*origin(3)
               call s2c( lon + theta, lat, rd, start )
               call arc( cen, cen, start, 2*pi, 0.05 )
            end do


            cen(1)=0
            cen(2)=0
            cen(3)=0

            call s2c( pi, pi/2-pi/2, rd, start )
            call s2c( pi+pi/2, 0.0, 1.0, norm )

            call rotvec( 'ZYZ', lon, pi/2-lat, lat, norm )
            call rotvec( 'ZYZ', lon, pi/2-lat, lat, start )
            call arc( cen, norm, start, pi/2, 0.0 )


            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'b', 'a', 'c', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), pi/2 + lat )
         endif

c+ Fig5
         if( diag .eq. 6 ) then
            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'b', 'a', 'c', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), pi/2 + lat )


            call pgscr( 10, 0.0, 0.0, 1.0 )
            call axes( origin, rd, 'b''', 'a''', 'c''', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), pi/2 )

            call pgsci( 1 )
            call s2c( lon, lat, 1.0, norm )
            start(1) = 0.0
            start(2) = 0.0
            start(3) = rd*0.5
            call s2c( lon, lat, 0.0, cen )
            call arc( cen, norm, start, -lat, 0.0 )

            call pgsci( 1 )
            call s2c( 0.5*( lon - pi/2), lat/2+0.2, rd*0.55, r )
            norm(1) = 0
            norm(2) = 0
            norm(3) = 0
            u(1) = 0
            u(2) = 0
            u(3) = 1.0
            call g3dtext( 'el', r, 'CR', u, norm )
         endif

c+ Fig6
         if( diag .eq. 7 ) then
            call s2c( lon, lat, 0.0, origin )

            call pgscr( 10, 0.0, 0.0, 1.0 )
            call axes( origin, rd, 'b''', 'a''', 'c''', w1, w2,
     :                 'ZYZ', lon, (pi/2 - lat), pi )

            call pgscr( 10, 0.0, 1.0, 0.0 )
            call axes( origin, rd, 'b''''', 'a''''', 'c''''', w1, w2,
     :                 'ZYZ', lon, 0.0, pi )

         endif





         if( diag .eq. 8 ) then
            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'a', 'b', 'c', w1, w2,
     :                 'ZYZ', 0.0, 0.0, 0.0 )
         end if

         if( diag .eq. 9 ) then
            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'a', 'b', 'c', w1, w2,
     :                 'ZYZ', lon, 0.0, 0.0 )
         end if

         if( diag .eq. 10 ) then
            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'a', 'b', 'c', w1, w2,
     :                 'ZYZ', lon, pi/2-lat, 0.0 )
         end if

         if( diag .eq. 11 ) then
            call pgscr( 10, 0.0, 1.0, 1.0 )
            call s2c( lon, lat, 0.0, origin )
            call axes( origin, rd, 'a', 'b', 'c', w1, w2,
     :                 'ZYZ', lon, pi/2-lat, lat )
         end if











c         if( common_map .ne. AST__NULL .and.
c     ;       common_x .ne. AST__BAD .and.
c     :       common_y .ne. AST__BAD ) then
c            call ast_tran2( common_map, 1, common_x, common_y, .true.,
c     :                      dlon, dlat, status )
c            call s2c( real(dlon), real(dlat), rd, cen )
c            call pgsci( 10 )
c            call pgscr( 10, 1.0, 0.0, 1.0 )
c            call g3dmark( cen, 2 )
c         end if

         call pgsci( 1 )
         call pgslw( 4 )
         call pgsls( 1 )

      end if

      end

      subroutine arc( cen, normal, start, ang, arrow )
      implicit none
      real cen(3), normal(3), start(3), ang, arrow, ut(3), ur(3)
      integer np
      parameter( NP = 50 )
      integer i
      real v1(3), v2( 3 ), v1l, v2l, x(NP), y(NP), z(NP), a, ca, sa,
     :     t1(3), t2(3), n(3)
      call copy( normal, n )
      call norm( n )
      call sub( start, cen, v1 )
      call cross( n, v1, t1 )
      call cross( t1, n, v1 )
      call mod( v1, v1l )
      call cross( v1, n, v2 )
      call mod( v2, v2l )
      do i = 1, 3
         v1( i ) = v1(i)/v1l
         v2( i ) = v2(i)/v2l
      end do

      do i = 1, NP
         a = ang*( i - 1 )/( NP - 1 )
         ca = cos(a)
         sa = sin(a)
         x(i) = v1l*( v1(1)*ca + v2(1)*sa ) + cen(1)
         y(i) = v1l*( v1(2)*ca + v2(2)*sa ) + cen(2)
         z(i) = v1l*( v1(3)*ca + v2(3)*sa ) + cen(3)
      end do
      call g3dline( NP, x, y, z )
      if( arrow .ne. 0.0 ) then
         ca = cos(ang)
         sa = sin(ang)
         do i = 1, 3
            ut(i) = sa*v1(i)-ca*v2(i)
            ur(i) = ca*v1(i)+sa*v2(i)
         end do
         x(1) = x(NP) + arrow*( ut(1) - 0.5*ur(1) )
         y(1) = y(NP) + arrow*( ut(2) - 0.5*ur(2) )
         z(1) = z(NP) + arrow*( ut(3) - 0.5*ur(3) )
         x(2) = x(NP)
         y(2) = y(NP)
         z(2) = z(NP)
         x(3) = x(NP) + arrow*( ut(1) + 0.5*ur(1) )
         y(3) = y(NP) + arrow*( ut(2) + 0.5*ur(2) )
         z(3) = z(NP) + arrow*( ut(3) + 0.5*ur(3) )
         call g3dline( 3, x, y, z )
      end if
      end
      subroutine s2c( lon, lat, r, p )
      implicit none
      real lon, lat, r, p(3), k
      p( 3 ) = r*sin(lat)
      k = r*cos(lat)
      p( 1 ) = k*cos(lon)
      p( 2 ) = k*sin(lon)
      end

      subroutine axes( origin, axlen, tx, ty, tz, w1, w2, order, phi,
     :                 theta, psi )
      implicit none
      character order*(*),tx*(*), ty*(*), tz*(*)
      real phi, theta, psi
      double precision rmat(3,3),va,vb
      real axlen, a1, a2, mat(3,3),x(10),y(10),z(10),r(3),u(3),n(3),
     :     origin(3), sn
      integer w1, w2
      a1 = 0.05
      a2 = a1*0.6
      sn = 0.0
      call deuler( order, dble(phi), dble(theta), dble(psi), rmat )
      mat(1,1) = rmat(1,1)
      mat(1,2) = rmat(1,2)
      mat(1,3) = rmat(1,3)
      mat(2,1) = rmat(2,1)
      mat(2,2) = rmat(2,2)
      mat(2,3) = rmat(2,3)
      mat(3,1) = rmat(3,1)
      mat(3,2) = rmat(3,2)
      mat(3,3) = rmat(3,3)
cc* X axis with arrow and label
      if( tx .ne. ' ' ) then
         x(1) = axlen
         y(1) = 0.0
         z(1) = 0.0
         x(2) = -axlen
         y(2) = 0.0
         z(2) = 0.0
         call mxv( mat, 2, x, y, z, origin )
         call pgslw( w1 )
         call g3dline( 2, x, y, z )
         x(1) = axlen - a1
         y(1) = -a2
         z(1) = 0.0
         x(2) = axlen
         y(2) = 0.0
         z(2) = 0.0
         x(3) = axlen - a1
         y(3) = a2
         z(3) = 0.0
         call mxv( mat, 3, x, y, z, origin )
         call g3dline( 3, x, y, z )
         r(1) = axlen + a1
         r(2) = 0.0
         r(3) = a2
         u(1) = 0.0
         u(2) = 0.0
         u(3) = 1.0
         n(1) = 0.0
         n(2) = sn
         n(3) = 0.0

         call pgslw( w2 )
         call mxv2( mat, r, .true., origin )
         call g3dtext( tx, r, 'BR', u, n )
         call pgslw( w1 )
      end if
c* Y axis with arrow and label
      if( ty .ne. ' ' ) then
         x(1) = 0.0
         y(1) = -axlen
         z(1) = 0.0
         x(2) = 0.0
         y(2) = axlen
         z(2) = 0.0
         call mxv( mat, 2, x, y, z, origin )
         call g3dline( 2, x, y, z )
         x(1) = -a2
         y(1) = axlen - a1
         z(1) = 0.0
         x(2) = 0.0
         y(2) = axlen
         z(2) = 0.0
         x(3) = a2
         y(3) = axlen - a1
         z(3) = 0.0
         call mxv( mat, 3, x, y, z, origin )
         call g3dline( 3, x, y, z )
         r(1) = 0.0
         r(2) = axlen + a1
         r(3) = a2
         u(1) = 0.0
         u(2) = 0.0
         u(3) = 1.0
         n(1) = sn
         n(2) = 0.0
         n(3) = 0.0

         call pgslw( w2 )
         call mxv2( mat, r, .true., origin )
         call g3dtext( ty, r, 'BR', u, n )
         call pgslw( w1 )
      end if
c* Z axis with arrow and label
      if( tz .ne. ' ' ) then
         x(1) = 0.0
         y(1) = 0.0
         z(1) = axlen
         x(2) = 0.0
         y(2) = 0.0
         z(2) = 0.0
         call mxv( mat, 2, x, y, z, origin )
         call g3dline( 2, x, y, z )
         x(1) = -a2
         y(1) = 0.0
         z(1) = axlen - a1
         x(2) = 0.0
         y(2) = 0.0
         z(2) = axlen
         x(3) = a2
         y(3) = 0.0
         z(3) = axlen - a1
         call mxv( mat, 3, x, y, z, origin )
         call g3dline( 3, x, y, z )
         r(1) = 0.0
         r(2) = 0.0
         r(3) = axlen + a1
         u(1) = 0.0
         u(2) = 0.0
         u(3) = 1.0
         n(1) = 0.0
         n(2) = 0.0
         n(3) = 0.0

         call pgslw( w2 )
         call mxv2( mat, r, .true., origin )
         call g3dtext( tz, r, 'CR', u, n )
         call pgslw( w1 )
      end if
      end

      subroutine mxv( mat, n, x, y, z, origin )
      implicit none
      integer n, i
      real x(n), y(n), z(n), mat(3,3), va(3),vb(3),origin(3)
      do i = 1, n
         va(1)= x(i)
         va(2)= y(i)
         va(3)= z(i)
         call sla_mxv( mat, va, vb )
         x(i) = vb(1) + origin(1)
         y(i) = vb(2) + origin(2)
         z(i) = vb(3) + origin(3)
      end do
      end

      subroutine mxv2( mat, r, move, origin )
      implicit none
      real r(3), mat(3,3), vb(3), origin(3)
      logical move
      call sla_mxv( mat, r, vb )
      if( move ) then
         r(1) = vb(1) + origin(1)
         r(2) = vb(2) + origin(2)
         r(3) = vb(3) + origin(3)
      else
         r(1) = vb(1)
         r(2) = vb(2)
         r(3) = vb(3)
      end if
      end

      subroutine deuler( order, phi, theta, psi, rmat )
      implicit none
      character order*(*)
      double precision phi, theta, psi, rmat(3,3), t(3), smat(3,3)
      double precision ang, v(3), ux(3), uy(3), uz(3), tmat(3,3),
     :                 axvec(3)
      integer n, i, j
      n = len( order )
      do i = 1, 3
         ux(i) = 0.0D0
         uy(i) = 0.0D0
         uz(i) = 0.0D0
         do j = 1, 3
            rmat(i,j) = 0.0
         end do
         rmat(i,i) = 1.0
      end do
      ux(1) = 1.0D0
      uy(2) = 1.0D0
      uz(3) = 1.0D0
      do i = 1, 3
         if( i .le. n ) then
            if( i .eq. 1 ) then
               ang = phi
            else if( i .eq. 2 ) then
               ang = theta
            else
               ang = psi
            end if
            if( order( i : i ) .eq. 'X' ) then
               do j = 1, 3
                  axvec(j) = -ux(j)*ang
               end do
            else if( order( i : i ) .eq. 'Y' ) then
               do j = 1, 3
                  axvec(j) = -uy(j)*ang
               end do
            else if( order( i : i ) .eq. 'Z' ) then
               do j = 1, 3
                  axvec(j) = -uz(j)*ang
               end do
            else
               write(*,*) 'Bad axis label (',order(i:i),') in deuler!!!'
            end if
            call sla_dav2m( axvec, tmat )
            do j = 1, 3
               smat(1,j) = rmat(1,j)
               smat(2,j) = rmat(2,j)
               smat(3,j) = rmat(3,j)
            end do
            call sla_dmxm( tmat, smat, rmat )
            call sla_dmxv( tmat, ux, v )
            ux(1) = v(1)
            ux(2) = v(2)
            ux(3) = v(3)
            call sla_dmxv( tmat, uy, v )
            uy(1) = v(1)
            uy(2) = v(2)
            uy(3) = v(3)
            call sla_dmxv( tmat, uz, v )
            uz(1) = v(1)
            uz(2) = v(2)
            uz(3) = v(3)
         end if
      end do
      end

      subroutine rotvec( order, phi, theta, psi, v )
      implicit none
      character order*(*)
      real phi, theta, psi,v(3),vt(3)
      double precision rmat(3,3)
      real mat(3,3)
      call deuler( order, dble(phi), dble(theta), dble(psi), rmat )
      mat(1,1) = rmat(1,1)
      mat(1,2) = rmat(1,2)
      mat(1,3) = rmat(1,3)
      mat(2,1) = rmat(2,1)
      mat(2,2) = rmat(2,2)
      mat(2,3) = rmat(2,3)
      mat(3,1) = rmat(3,1)
      mat(3,2) = rmat(3,2)
      mat(3,3) = rmat(3,3)
      call sla_mxv( mat, v, vt )
      v(1) = vt(1)
      v(2) = vt(2)
      v(3) = vt(3)
      end

      subroutine cross( a, b, c )
      implicit none
      real a(3), b(3), c(3)
      c(1) = a(2)*b(3) - a(3)*b(2)
      c(2) = - a(1)*b(3) + a(3)*b(1)
      c(3) = a(1)*b(2) - a(2)*b(1)
      end

      subroutine sub( a, b, c )
      implicit none
      real a(3), b(3), c(3)
      c(1) = a(1) - b(1)
      c(2) = a(2) - b(2)
      c(3) = a(3) - b(3)
      end

      subroutine add( a, b, c )
      implicit none
      real a(3), b(3), c(3)
      c(1) = a(1) + b(1)
      c(2) = a(2) + b(2)
      c(3) = a(3) + b(3)
      end

      subroutine dot( a, b, c )
      implicit none
      real a(3), b(3), c
      c = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      end

      subroutine mod( a, c )
      implicit none
      real a(3), c
      c = sqrt( a(1)*a(1) + a(2)*a(2) + a(3)*a(3) )
      end

      subroutine copy( a, b )
      implicit none
      real a(3), b(3)
      b(1) = a(1)
      b(2) = a(2)
      b(3) = a(3)
      end

      subroutine norm( a )
      implicit none
      real a(3), b
      call mod( a, b )
      if( b .ne. 0.0 ) then
         a(1) = a(1)/b
         a(2) = a(2)/b
         a(3) = a(3)/b
      end if
      end

      subroutine g3dtext( t, r, j, u, n )
      implicit none
      character*(*) t, j
      real r(3), u(3), n(3), r2(3),u2(3),n2(3)
      integer junk, ast_g3dtext

      call copy( r, r2 )
      call copy( u, u2 )
      call copy( n, n2 )
      r2(2) = -r2(2)
      u2(2) = -u2(2)
      n2(2) = -n2(2)

      junk = ast_g3dtext( t, r2, j, u2, n2 )
      end

      subroutine g3dline( n, x, y, z )
      implicit none
      integer i, n
      logical junk, ast_g3dline
      real x(n), y(n), z(n)

      do i = 1, n
         y(i)=-y(i)
      end do

      junk = ast_g3dline( n, x, y, z )

      do i = 1, n
         y(i)=-y(i)
      end do

      end

      subroutine g3dmark( pos, type )
      implicit none
      real pos(3)
      integer type
      logical junk, ast_g3dmark

      pos(2) = -pos(2)
      junk = ast_g3dmark( 1, pos(1), pos(2), pos(3), type, pos )
      pos(2) = -pos(2)

      end





      subroutine maketanmap( new, lon, lat, rot, azelmap, status )
      implicit none

      include 'SAE_PAR'
      include 'AST_PAR'

      logical new
      double precision lon, lat, rot
      integer azelmap, status

      double precision mmt( 2 ), mat(9), mat1(9), mat2(3)
      integer tanmap, tmap, sphmap, wcsmap, m1, m2, m3, c0, c1, matmap1,
     :        matmap2
      double precision pi, piby2

      if( status .ne. sai__ok ) return

      pi = 3.1415927D0
      piby2 = pi/2.0

      c0 = ast_SphMap( 'UnitRadius=1', status )
      wcsmap = ast_WcsMap( 2, AST__TAN, 1, 2, ' ', status )
      call ast_Invert( wcsmap, status )
      call ast_Invert( c0, status )
      c1 = ast_CmpMap( wcsmap, c0, .true., ' ', status )
      call ast_Invert( c0, status )

      if( new ) then
         mmt( 1 ) = -1.0
         mmt( 2 ) = -1.0
         tmap = ast_MatrixMap( 2, 2, 1, mmt, ' ', status )

*  Note, sla_deuler groups columns in the returned matrix, but slaDeuler
*  groups rows. AST always expects rows to be grouped, so transpose the
*  matrices returned by sla_deuler.
         call sla_Deuler( 'Z', -rot, 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m1 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )

         call sla_Deuler( 'Y', (PIBY2-lat), 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m2 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )

         call sla_Deuler( 'Z', (lon-PI), 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m3 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )

         matmap1 = ast_CmpMap( m1, ast_CmpMap( m2, m3, .true., ' ',
     :                                         status ),
     :                         .TRUE., ' ', status )

         mat2( 1 ) = 1.0
         mat2( 2 ) = -1.0
         mat2( 3 ) = 1.0
         matmap2 = ast_MatrixMap( 3, 3, 1, mat2, ' ', status )

         m1 = ast_CmpMap( c1, matmap1, .true., ' ', status )
         m2 = ast_CmpMap( m1, matmap2, .true., ' ', status )
         tanmap = ast_CmpMap( m2, c0, .true., ' ', status )
         azelmap = ast_CmpMap( tmap, tanmap, .true., ' ', status )

      else

*  Note, sla_deuler groups columns in the returned matrix, but slaDeuler
*  groups rows. AST always expects rows to be grouped, so transpose the
*  matrices returned by sla_deuler.
         call sla_Deuler( 'Z', rot, 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m1 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )

         call sla_Deuler( 'Y', -(PIBY2-lat), 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m2 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )

         call sla_Deuler( 'Z', -lon, 0.0D0, 0.0D0, mat1 )
         call trans( mat1 )
         m3 = ast_MatrixMap( 3, 3, 0, mat1, ' ', status )


         matmap1 = ast_CmpMap( m1, ast_CmpMap( m2, m3, .true., ' ',
     :                                         status ),
     :                         .TRUE., ' ', status )

         m1 = ast_CmpMap( c1, matmap1, .true., ' ', status )






         mmt( 1 ) = -1.0
         mmt( 2 ) = 1.0
         tmap = ast_MatrixMap( 2, 2, 1, mmt, ' ', status )
         m2 = ast_CmpMap( tmap, m1, .true., ' ', status )






         azelmap = ast_CmpMap( m2, c0, .true., ' ', status )

      end if

      end

      subroutine trans( mat )
      implicit none
      double precision t, mat(9)

      t = mat(8)
      mat(8) = mat(6)
      mat(6) = t

      t = mat(3)
      mat(3) = mat(7)
      mat(7) = t

      t = mat(2)
      mat(2) = mat(4)
      mat(4) = t

      end
