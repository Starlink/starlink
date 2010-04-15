      program testxmlchan
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'testxmlchan_com'

      integer status, obj, i, ifmt, nfmt
      character fmt(2)*30
      logical ok

      data nfmt /2/,
     :     fmt / 'native', 'quoted' /

      status = sai__ok

      call ast_begin( status )

*
*  Create an AST object.
*
      call makeobject( obj, status )

*
*  Create a normal Channel and write the object out to file1
*
      call chanwrite( obj, 1, status )

*
*  Test each XML format in turn.
      do ifmt = 1, nfmt

*
*  Create an XmlChan and write the object out to file2 using the current
*  format.
*
         call xmlwrite( obj, 2, fmt( ifmt ), status )

*
*  Use a new XmlChan to read an object from file2
*
         call xmlread( 2, obj, status )

*
*  Write this object out to file 3 using a simple Channel.
*
         call chanwrite( obj, 3, status )

*
*  Report an error if the contents of files 1 and 3 differ.
*
         ok = .true.
         if( filelen( 1 ) .ne. filelen( 3 ) ) then
            write(*,*) 'TestXmlChan: files 1 and 3 have different '//
     :                 'lengths (',filelen( 1 ),',',filelen( 3 ),').'
            ok =.false.
         else
            do i = 1,  filelen( 1 )
               if( files( 1, i ) .ne. files( 3, i ) ) then
                  write(*,*) 'TestXmlChan: Line ',i,' differs in '//
     :                       'files 1 and 3:'
                  write(*,*) files( 1, i )
                  write(*,*) files( 3, i )
                  ok = .false.
                  go to 10
               end if
            end do
         end if

 10      continue

         if( .not. ok ) then
            write(*,*) 'TestXmlChan: Test failed on XmlFormat ''',
     :                 fmt(ifmt),'''.'
            go to 20
         end if

      end do

 20   continue

      call ast_end( status )

      if( ok ) then
         write(*,*) 'All XmlChan tests passed'
      else
         write(*,*) 'XmlChan tests failed'
      end if

      end

*
*  Reads line "iline" from internal file "ifile" and returns it to AST using
*  the AST_PULINE routine. Then increments "iline" ready for next time.
*
      subroutine source( status )
      implicit none

      include 'testxmlchan_com'

      integer status, l, chr_len

      if( iline .le. filelen( ifile ) ) then
         l = chr_len( files(ifile,iline) )
         call ast_putline( files(ifile,iline), l, status )
         iline = iline + 1
      else
         call ast_putline( ' ', -1, status )
      end if

      end

*
*  Append a line obtained using ast_getline function to the end of the
*  internal file indicated by "ifile", and increment the file length.
*
      subroutine sink( status )
      implicit none

      include 'testxmlchan_com'

      integer status, l
      character line*(linelen)

      call ast_getline( line, l, status )
      if( l .gt. 0 ) then

         if( filelen( ifile ) .ge. mxline ) then
            stop 'TestXmlChan: Too many lines sent to sink function'

         else if( l .gt. linelen ) then
            stop 'TestXmlChan: Text truncated in sink function'

         else
            filelen( ifile ) = filelen( ifile ) + 1
            files( ifile, filelen( ifile ) ) = line(:l)
         end if

      end if

      end

*
*  Create an AST object to be used as the test object.
*
      subroutine makeobject( obj, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer obj, sf, f, m, status

      obj = ast__null
      if( status .ne. sai__ok ) return

      sf = Ast_SkyFrame( ' ', status )
      f = Ast_Frame( 2, ' ', status )
      call ast_setc( f, 'title',  'A new title', status )
      m = ast_UnitMap( 2, ' ', status )
      obj = ast_FrameSet( f, ' ', status )
      call ast_addFrame( obj, 1, m, sf, status )

      end

*
*  Write the supplied object out to the specified internal file using a
*  basic Channel.
*
      subroutine chanwrite( obj, ifil, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      include 'testxmlchan_com'

      external sink
      integer obj, ifil, status, ch

      if( status .ne. sai__ok ) return

      ifile = ifil
      filelen( ifil ) = 0

      ch = ast_channel( ast_null, sink, ' ', status )
      if( ast_write( ch, obj, status ) .ne. 1 ) then
         stop 'TestXmlChan: Failed to write object to Channel.'
      end if
      call ast_annul( ch, status )

      end

*
*  Write the supplied object out to the specified internal file using an
*  XmlChan.
*
      subroutine xmlwrite( obj, ifil, fmt, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      include 'testxmlchan_com'

      external sink
      integer obj, ifil, status, ch
      character fmt*(*)

      if( status .ne. sai__ok ) return

      ifile = ifil
      filelen( ifil ) = 0

      ch = ast_xmlchan( ast_null, sink, 'indent=1,comment=1',
     :                  status )
      call ast_seti( ch, 'xmllength', linelen, status )
      call ast_setc( ch, 'xmlformat', fmt, status )
      if( ast_write( ch, obj, status ) .ne. 1 ) then
         stop 'TestXmlChan: Failed to write object to XmlChan.'
      end if
      call ast_annul( ch, status )

      end

*
*  Read an object out of the specified internal file using an XmlChan.
*
      subroutine xmlread( ifil, obj, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      include 'testxmlchan_com'

      external source
      integer obj, ifil, status, ch

      if( status .ne. sai__ok ) return

      ifile = ifil
      iline = 1

      ch = ast_xmlchan( source, ast_null, ' ', status )
      obj = ast_read( ch, status )
      if( obj .eq. ast__null ) then
         stop 'TestXmlChan: Failed to read object from XmlChan.'
      end if
      call ast_annul( ch, status )

      end
