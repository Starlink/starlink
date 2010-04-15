CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   This is STARFLIB_ADAM.F
C
C  This contains the subroutines which the programmer uses to
C  access ADAM specific cpabilities:-
C
C*********************************
C Subroutines sorted by function:-
C
C   Command line/parameter system:
C
C FILE_IS     Does this HDS file exist?
C FILE_PAR    Writes the name of an HDS file to a parameter
C GET1C       Get a character string from the CL
C GETNC       Get a number of character strings from the CL
C GETG(BIR)   Get a number of booleans:integers:reals from the CL
C PRINTO      Write a line out to the CL
C PUTG(IR)    Put (1:2:3) (integer/real) number(s) to the CL
C
C   Image/table input/output:
C
C CANPAR      Cancel parameter name connection - actually cancel associtation
C             to a NDF and parameter
C OPIMGR      Open an input 2d read-only generic image
C OPIMGW      Open an output 2d write-only generic image
C OPIM4G(RW)  Open an in/output 4d read-only/write-only generic image
C OPTAB(RW)   Open an input read-only/output write-only  table
C
C   Work space:
C
C GTWRKG      Open computer work space
C WRKCAN      Cancel work space name connection
C
C   Internal image/table handling:
C
C (IT)COPDES  Copy all headers from one file to next for image/table
C DLDES       Delete a descriptor from a file
C GTDESC_GEN  Get a character string from a file defined extension descriptor
C GTDES(CIR)  Get a (character string:integer:real) from a file descriptor
C GTDESN      Get the name of the 'nth' descriptor of a file
C GTDESN_GEN  Get the name of the 'nth' descriptor of a file extension
C GTIM(IRS)D  Get descriptors (BSCALE,BZERO,(R)INVAL,TITLE) from image
C PTDESCN     Write character strings into the header area of an opened file
C PTDES(CIR)  Write a character string/integer/real into the header area of an opened file
C
C   Misc:
C
C AJRAN         Random number generator
C AJSEED        Set seed for Random number generator
C AJDATE        Return a string with date and time
C CHARTO(ILR)   Convert character string to (integer:logical:real)
C FILEBEG       Start up file access system (ndf space common block)
C FIND_SPACE    Find gap in work space common block
C ISBSWOP       Is the computer swopping bytes on data read?
C ISVMS         Is the computer running VMS?
C STARMAN_END   Close down Starman controls
C STARMAN_START Set up Starman defaults
C SPACE_NDF     Find gap in ndf space common block
C WRKBEG        Set up computer work space information
C
C**********************************
C Subroutines in alpbetical order:-
C
C AJRAN       Random number generator
C AJSEED      Set seed for Random number generator
C AJDATE      Return a string with date and time
C CANPAR      Cancel parameter name connection - actually cancel associtation
C             to a NDF and parameter
C CHARTO(ILR) Convert character string to (integer:logical:real)
C (IT)COPDES  Copy all headers from one file to next for image/table
C DLDES       Delete a descriptor from a file
C FILEBEG     Start up file access system (ndf space common block)
C FILE_IS     Does this HDS file exist?
C FILE_PAR    Writes the name of an HDS file to a parameter
C FIND_SPACE  Find gap in work space common block
C GET1C       Get a character string from the CL
C GETNC       Get a number of character strings from the CL
C GETG(BIR)   Get a number of booleans:integers:reals from the CL
C GTDESC_GEN  Get a character string from a file defined extension descriptor
C GTDES(CIR)  Get a (character string:integer:real) from a file descriptor
C GTDESN      Get the name of the 'nth' descriptor of a file
C GTDESN_GEN  Get the name of the 'nth' descriptor of a file extension
C GTIM(IRS)D  Get descriptors (BSCALE,BZERO,(R)INVAL,TITLE) from image
C GTWRKG      Open computer work space
C ISBSWOP     Is the computer swopping bytes on data read?
C ISVMS       Is the computer running VMS?
C OPIMGR      Open an input 2d read-only generic image
C OPIMGW      Open an output 2d write-only generic image
C OPIM4G(RW)  Open an in/output 4d read-only/write-only generic image
C OPTAB(RW)   Open an input read-only/output write-only  table
C PRINTO      Write a line out to the CL
C PTDESCN     Write character strings into the header area of an opened file
C PTDES(CIR)  Write a character string/integer/real into the header area of an opened file
C PUTG(IR)    Put (1:2:3) (integer/real) number(s) to the CL
C SPACE_NDF   Find gap in ndf space common block
C STARMAN_END  Close down Starman controls
C STARMAN_START Set up Starman defaults
C WRKBEG      Set up computer work space information
C WRKCAN      Cancel work space name connection


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AJRAN -- (FN) Random number generator - uniform between 0 and 1
C
C   a j penny                    ral                 1991 Nov

      real function ajran ( rvd )

      implicit none
      include 'NDF_PAR'
      include 'SAE_PAR'

      real    rvd	!i: dummy number, not used
C--
      real rv
      integer status, nran, maxn
Cbegin


      status = SAI__OK
      call psx_rand ( nran, maxn, rv, status )
      ajran = rv


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AJSEED -- Set seed for Random number generator
C
C   a j penny                    ral                 1991 Nov

      subroutine ajseed ( nran )

      implicit none
      include 'NDF_PAR'
      include 'SAE_PAR'
      include 'ST_LIMITS_INC'

      integer	nran	!i: Seed
C--
      integer status
Cbegin


      status = SAI__OK
      call psx_srand ( nran, status )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AJDATE -- Return character string with date and time
C
C   a j penny                    ral                 1991 Nov

      subroutine ajdate ( date_time )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)    date_time	!o: Date and time in format
					!   Wed Apr 17 09:01:04 1991
					!   (needs up to 24 characters)
C--
      integer status, nticks
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK

      call psx_time ( nticks, status )
      call psx_ctime ( nticks, date_time, status )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CANPAR -- Cancel parameter name connection - actually cancel associtation
C           to a NDF and parameter
C
C  alan penny              ral               1990 Jan

      subroutine canpar ( text )

      implicit none
      include 'NDF_PAR'
      include 'SAE_PAR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*) text	!i: Name of parameter to cancel
C--
      integer istat, n, kl, lens
      logical found, chr_equal
      external chr_equal, lens
Cbegin


      if ( ST_FAILED ) return

      if ( text.eq.' ' ) then

         do n = 1, NDF_LIMIT                				!Cancel all current files
            istat = SAI__OK
            if ( NDF_NAME(n).ne.'-' ) then
               if ( SNDF(n).ne.NDF__NOID ) then
                  call ndf_annul ( SNDF(n), istat )
                  call par_cancl ( NDF_NAME(n), istat )
               else
                  call printo ( 'INVALID NDF identifier ')
               endif
               NDF_NAME(n) = '-'
               NDF_COUNTER = NDF_COUNTER - 1
            endif
         enddo

      else

         istat = SAI__OK
         kl = min(50,lens(text))
         n = 1
         found = .false.
         do while ( .not.found )
            found = chr_equal ( text(1:kl), NDF_NAME(n) )
            if ( found ) then
               if ( SNDF(n).ne.NDF__NOID ) then
                  call ndf_annul ( SNDF(n), istat )
               else
                  call printo ( 'INVALID NDF identifier ')
               endif
               NDF_NAME(n) = '-'
               NDF_COUNTER = NDF_COUNTER - 1
            else
               n = n + 1
               if ( n.gt.NDF_LIMIT ) then
                  found = .true.
                  kl = min(45,kl)
                  call msg_setc('NDF', text(1:kl) )
                  call printo ( 'Cant find NDF parameter to'//
     +                          ' annul: ^NDF' )
               endif
            endif
         enddo

         call par_cancl ( text, istat )

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHARTOI -- Convert character string to integer
C
C    a j penny                 ral                  1991 Dec

      subroutine chartoi ( ch, iv, istat )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)      ch      !i: Character string
      integer            iv      !o: Integer
      integer            istat   !o: Error flag (0=ok, 1=bad)
C--
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      ierr = SAI__OK
      call chr_ctoi ( ch, iv, ierr )

      istat = 0
      if ( ierr.ne.SAI__OK ) istat = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHARTOL -- Convert character string to logical
C
C    a j penny                 ral                  1991 Dec

      subroutine chartol ( ch, bv, istat )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)      ch      !i: Character string
      logical            bv      !o: Integer
      integer            istat   !o: Error flag (0=ok, 1=bad)
C--
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      ierr = SAI__OK
      call chr_ctol ( ch, bv, ierr )

      istat = 0
      if ( ierr.ne.SAI__OK ) istat = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHARTOR -- Convert character string to real
C
C    a j penny                 ral                  1991 Dec

      subroutine chartor ( ch, rv, istat )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)      ch      !i: Character string
      real               rv      !o: Real
      integer            istat   !o: Error flag (0=ok, 1=bad)
C--
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      ierr = SAI__OK
      call chr_ctor ( ch, rv, ierr )


      istat = 0
      if ( ierr.ne.SAI__OK ) istat = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ICOPDES -- Copy all headers from one file to next for image
C
C    a j penny                 ral                  1990-07-09

      subroutine icopdes ( ifile, ofile, istat )

      implicit none
      include 'DAT_PAR'
      include 'NDF_PAR'
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)     ifile           !i: Parameter name for input file
      character*(*)     ofile           !i: Parameter name for output file
      integer           istat           !o: Error flag (0=ok;1=bad)
C--
      integer istata, indf, ondf, i, ncomp
      character*80 title,label,dscr
      character*(DAT__SZLOC) iloc,oloc,cloc, oloc2
      logical there, outth
Cbegin


      if ( ST_FAILED ) return

      istat = 0
      istata = SAI__OK

      call ndf_exist ( ifile, 'READ', indf, istata )
      if ( indf.eq.NDF__NOID ) then
         istat = 1
         return
      endif
      call ndf_exist ( ofile, 'UPDATE', ondf, istata )
      if ( ondf.eq.NDF__NOID ) then
         call ndf_annul ( indf, istata )
         istat = 1
         return
      endif

      call ndf_xstat ( indf, 'FITS', there, istata )    		!Check there is input FITS
      if ( there ) then
         call ndf_loc ( ondf, 'UPDATE', oloc, istata )
         call dat_find (oloc,'MORE',oloc2,istata)

         call ndf_xstat ( ondf, 'FITS', outth, istata )  		!See if FITS there already
         if ( outth ) call dat_erase ( oloc2, 'FITS', istata ) 		!If it is delete it

         call ndf_xloc ( indf, 'FITS', 'READ', iloc, istata ) 		!Get locator to input fits
         call dat_copy ( iloc, oloc2, 'FITS', istata )  		! Copy over
         call dat_annul ( oloc2, istata )
         call dat_annul ( oloc, istata )
         call dat_annul ( iloc, istata )
      endif

      call ndf_xstat ( indf, 'STARMAN', there, istata )			!Check input image
      if ( .not.there ) then						! has STARMAN extension - if not exit
         call printo ( 'No STARMAN extension in INPUT image' )
         call ndf_annul ( indf, istata )
         call ndf_annul ( ondf, istata )
         istat = 1
         return
      endif

      call ndf_xstat ( ondf, 'STARMAN', there, istata )			!See if output extension has
      if ( .not.there ) then						! STARMAN EXT, if not create
         call ndf_xnew ( ondf, 'STARMAN', 'EXT', 0, 0, oloc, istata )
      else
         call ndf_xloc ( ondf, 'STARMAN', 'WRITE', oloc, istata )	!Write access means previous info deleted
      endif

      call ndf_cget ( indf, 'TITLE', title, istata )                    !Copy in new label and title
      call ndf_cget ( indf, 'LABEL', label, istata )
      call ndf_cput ( title, ondf, 'TITLE', istata )
      call ndf_cput ( label, ondf, 'LABEL', istata )

      call ndf_xloc ( indf, 'STARMAN', 'READ', iloc, istata )           !Find locators to extensions

      call dat_ncomp ( iloc, ncomp, istata )
      if ( ncomp.ge.2 ) then
         do i = 2, ncomp                                                !Copy extension
            call dat_index ( iloc, i, cloc, istata )
            call dat_name ( cloc, dscr, istata )
            call dat_copy ( cloc, oloc, dscr, istata )
            call dat_annul ( cloc, istata )
         enddo
      endif
      call dat_annul ( iloc, istata )
      call dat_annul ( oloc, istata )
      call ndf_annul ( indf, istata )
      call ndf_annul ( ondf, istata )

      if ( istata.ne.SAI__OK ) then
         call printo ( ' ERROR: Error in copying descriptors from'//
     +                        ' one image to another.' )
         call printo ( ' ERROR:   - Happened in subroutine ICOPDES' )
         istat = 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TCOPDES -- Copy all headers from one file to next for table
C
C    a j penny                 ral                  1990-07-09

      subroutine tcopdes ( ifile, ofile, istat )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)	ifile		!i: Parameter name for input file
      character*(*)	ofile		!i: Parameter name for output file
      integer		istat		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call icopdes ( ifile, ofile, istat )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DLDES -- Delete a descriptor from a file
C
C  a j penny                     ral            1990-06-15

      subroutine dldes ( name, dscr, istat )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	name		!i: File name
      character*(*)	dscr		!i: Descriptor
      integer		istat		!o: 0 => ok;1 = failure
C--
      integer istata, ndf
      character*(DAT__SZLOC) loc
      logical there
Cbegin


      if ( ST_FAILED ) return

      istata = SAI__OK

      istat = 1
      call ndf_exist ( name, 'UPDATE', ndf, istata )
      if (ndf.eq.NDF__NOID) return
      call ndf_xstat ( ndf, 'STARMAN', there, istata )
      if ( there ) then
        call ndf_xloc ( ndf, 'STARMAN', 'UPDATE', loc, istata )
        call dat_erase ( loc, dscr, istata )
        call dat_annul ( loc , istata )
        call ndf_annul ( ndf, istata )
      else
        call ndf_annul ( ndf, istata )
        istata =  1
      endif

      istat = 0
      if ( istata.ne.SAI__OK ) istat = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FILEBEG -- Start up file access system (ndf space common block)
C
C  Patrick Morris          leeds          1991 Dec

      subroutine filebeg ( )

      implicit none
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      NDF_COUNTER = 0
      do k = 1, NDF_LIMIT
         NDF_NAME(k) = '-'
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FILE_IS -- Does this HDS file exist?
C
C   alan penny               ral         1990 Jan

      subroutine file_is ( name, flag )

      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'STARMAN_INC'

      character*(*) name	!i: Text string file name
      logical       flag	!o: .true. if is an HDS file, .false. if not
C--
      character*(DAT__SZLOC) nloc
      integer status
Cbegin


      if ( ST_FAILED ) return


      flag = .true.

      status = SAI__OK
      call err_mark
      call hds_open ( name, 'READ', nloc, status )
      if ( status.eq.DAT__FILNF ) then
         call err_annul ( status )
         flag = .false.
      else
         call hds_close ( nloc, status )
      endif
      call err_rlse


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FILE_PAR -- Writes the name of an HDS file to a parameter
C
C     Normally, the handles to HDS data files are locators and files
C     are obtained via the parameter system.  However, some
C     applications can generate sensible names, especially when dealing
C     a long series of files that are to be created without manual
C     intervention.  There is no direct mechanism in the user-level
C     parameter-system library to put a name into the associated
C     parameter.  This routine provides that functionality.
C     This cannot be used to obtain the names of objects within
C     an HDS file.
C
C  Malcolm J. Currie (STARLINK)
C  alan penny                  ral  1994 March

      subroutine file_par ( filename, parname, istat )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*) filename		!i: The name of the file
      character*(*) parname		!i: Parameter name associated with
					!   the data object whose name is to be written
      integer       istat		!o: Error flag (0=ok; 1=bad)
C--
      integer imcode, status
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK
      call subpar_findpar ( parname, imcode, status )		!Get the ADAM internal code that refers to the piece of
								! parameter space associated with the input parameter PARNAM.

      call subpar_putname ( imcode, filename, status )		!Associate the file name with the ADAM internal pointer.

      istat = 0
      if ( status.ne.SAI__OK ) istat = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FIND_SPACE -- Find gap in work space common block
C
C  Patrick Morris          leeds          1991 Dec

      subroutine find_space ( n )

      implicit none
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      integer	n		!o : Available space, -1 no space
C--
      logical found
Cbegin


      if ( ST_FAILED ) return

      if ( WS_COUNTER.ge.WORK_LIMIT ) then
          n = -1
          return
      endif

      found = .false.
      n = 0
      do while ( .not.found )
          n = n + 1
          if ( n.gt.WORK_LIMIT ) then
             n = -1
             found = .true.
          else
             found = ( WS_NAME(n).eq.'-' )
          endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET1C -- Get a character string from the CL
C
C   a j penny                 dao           1988-04-25

      subroutine get1c ( param, out, in, def )

      implicit none
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'STARMAN_INC'

      character*(*) param		!i: Command line paramter
      character*(*) out			!o: Character string obtained
      character*(*) in			!i: Default character string to use
      logical       def			!i: If .true., null entry ok, and
					!   default taken; if not ask again
C--
      character text*200, atext*200
      integer istat, nvals, nloop, nmax
      logical loop
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call par_def1c ( param, 1, in, istat )

      nloop = 1
      loop = .true.
      nmax = 1
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         text = in
         istat = SAI__OK

         call err_mark
         call par_get1c ( param, 1, text, nvals, istat )
         call par_cancl ( param, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__INVST ) then
            atext = ' ERROR: Parameter name - '//param//' - invalid'
            call printo ( atext )
            loop = .true.
         elseif ( .not.def .and. istat.eq.PAR__NULL ) then
            text = ' ERROR: Null entry unacceptable'
            if ( nloop.le.nmax-1 ) atext = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
            call printo ( atext )
            loop = .true.
         else
            if ( istat.eq.PAR__INVST ) then
               out = in
            else
               out = text
            endif
         endif
         call err_rlse

         nloop = nloop + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETNC -- Get a number of character strings from the CL
C
C   a j penny                 dao           1988-04-25

      subroutine getnc ( param, out, nin, nout, def )

      implicit none
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'STARMAN_INC'

      character*(*) param		!i: Parameter to get info from
      integer       nin			!i: Max no of strings to get
      character*(*) out(nin)		!o: Strings got
      integer       nout		!o: No of strings got
      logical       def			!i: Is null response ok?
C--
      character text*50, atext*70
      character*500 intext
      integer istat, nvals, nloop, nmax, k
      integer ks(1000),ke(1000)
      logical loop
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call par_def0c ( param, ' ', istat )

      nloop = 1
      loop = .true.
      nmax = 1
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         istat = SAI__OK

         intext = ' '
         call err_mark
         call par_get0c ( param, intext, istat )
         call par_cancl ( param, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__INVST ) then
            atext = ' ERROR: Parameter name - '//param//' - invalid'
            call printo ( atext )
            loop = .true.
         elseif ( .not.def .and. istat.eq.PAR__NULL ) then
            atext = ' ERROR: Null entry unacceptable'
            if ( nloop.le.nmax-1 ) atext = ' ERROR: Null entry '//
     +                        'unacceptable - try again'
            call printo ( atext )
            loop = .true.
         elseif ( istat.ne.SAI__OK ) then
            call err_rep ( ' ', ' ERROR: ^status', istat )
            loop = .true.
         else
            do k = 1, nin
               call chr_fill ( ' ', out(k) )
            enddo
            call numel ( intext, nvals, ks, ke )
            nout = min( nin, nvals )
            do k = 1, nout
               if ( ke(k).ge.ks(k) ) out(k) = intext(ks(k):ke(k))
            enddo
         endif
         call err_rlse
         nloop = nloop + 1

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETGI -- Get a number of integers from the CL
C
C    a j penny                 ral                   1988-Nov


      subroutine getgi ( param, ivn, num, def, imin, imax )

      implicit none
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'STARMAN_INC'

      character*(*) param	!i: Parameter to access in CL
      integer       num		!i: Number of integers
      integer	    ivn(num)	!i/o: Integers (defaults/gotten values)
      logical       def		!i: Is a null rsponse valid?
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      logical loop, isok
      integer nvals, istat, nloop, nmax, k, iv(5), iva
      character text*50, atext*70
Cbegin


      if ( ST_FAILED ) return

      nloop = 1
      loop = .true.
      nmax = 1
      do while ( loop .and. nloop.le.nmax )
         loop = .false.
         do k = 1, num
            iv(k) = ivn(k)
         enddo
         istat = SAI__OK
         isok = .true.

         call err_mark
         call par_def1i ( param, num, iv, istat )
         call par_get1i ( param, num, iv, nvals, istat )
         call par_cancl ( param, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__INVST ) then
            atext = ' ERROR: Parameter name - '//param//' - invalid'
            call printo ( atext )
            loop = .true.
            isok = .false.
         elseif ( istat.ne.SAI__OK ) then
            atext = ' ERROR: Parameter error - '//param
            call printo ( atext )
            call err_rep ( ' ', ' ERROR: ^status', istat )
            loop = .true.
            isok = .false.
         elseif ( .not.def .and.nvals.ne.num ) then
            call pargi ( num )
            call printd ( ' ERROR: must input %d values' )
            loop = .true.
            isok = .false.
         else
            iva = iv(1)
            do k = 1, num
               iva = max(iva,iv(k))
            enddo
            if ( imax.lt.iva ) then
               write ( atext,'('' ERROR: over maximum value of '',
     +                       i11)' ) imax
               call printo ( atext )
               loop = .true.
               isok = .false.
            endif
            iva = iv(1)
            do k = 1, num
               iva = min(iva,iv(k))
            enddo
            if ( imin.gt.iva ) then
               write (atext,'('' ERROR: under minimum value of '',
     +                      i11)' ) imin
               call printo ( atext )
               loop = .true.
               isok = .false.
            endif
         endif

         call err_rlse
         nloop = nloop + 1

      enddo

      if ( nloop.eq.(nmax+1) .and. .not.isok ) then
         call printo ( ' WARNING: Default values taken' )
      else
         do k = 1, num
            ivn(k) = iv(k)
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETGB -- Get a number of booleans from the CL
C
C    a j penny                 ral                   1988-Nov


      subroutine getgb ( param, ivn, num, def )

      implicit none
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'STARMAN_INC'

      character*(*) param	!i: Parameter to access in CL
      integer       num		!i: Number of integers
      logical       ivn(num)	!i/o: Integers (defaults/gotten values)
      logical       def		!i: Is a null rsponse valid?
C--
      logical loop, isok
      integer nvals, istat, nloop, nmax, k
      character text*50, atext*70
      logical iv(5)
Cbegin


      if ( ST_FAILED ) return

      nloop = 1
      loop = .true.
      nmax = 1
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         do k = 1, num
            iv(k) = ivn(k)
         enddo
         istat = SAI__OK
         isok = .true.

         call err_mark
         call par_def1l ( param, num, iv, istat )
         call par_get1l ( param, num, iv, nvals, istat )
         call par_cancl ( param, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__INVST ) then
            atext = ' ERROR: Parameter name - '//param//' - invalid'
            call printo ( atext )
            loop = .true.
            isok = .false.
         elseif ( istat.ne.SAI__OK ) then
            atext = ' ERROR: Parameter error - '//param
            call printo ( atext )
            call err_rep ( ' ', ' ERROR: ^status', istat )
            loop = .true.
            isok = .false.
         elseif ( .not.def .and.nvals.ne.num ) then
            call pargi ( num )
            call printd ( ' ERROR: must input %d values' )
            loop = .true.
            isok = .false.
         endif
         call err_rlse
         nloop = nloop + 1

      enddo

      if ( nloop.eq.(nmax+1) .and. .not.isok ) then
         call printo ( ' WARNING: Default values taken' )
      else
         do k = 1, num
            ivn(k) = iv(k)
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETGR -- Get a number of reals from the CL
C
C    a j penny                 ral                   1988-Nov


      subroutine getgr ( param, rvn, num, def, rmin, rmax )

      implicit none
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'STARMAN_INC'

      character*(*) param	!i: Parameter to access in CL
      integer       num		!i: Number of reals
      real          rvn(num)	!i/o: Reals (defaults/gotten values)
      logical       def		!i: Is a null rsponse valid?
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      logical loop, isok
      integer nvals, istat, nloop, nmax, k
      real rv(5), rva
      character  text*50, atext*70
Cbegin


      if ( ST_FAILED ) return

      nloop = 1
      loop = .true.
      nmax = 1
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         do k = 1, num
            rv(k) = rvn(k)
         enddo
         istat = SAI__OK
         isok = .true.

         call err_mark
         call par_def1r ( param, num, rv, istat )
         call par_get1r ( param, num, rv, nvals, istat )
         call par_cancl ( param, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__INVST ) then
            atext = ' ERROR: Parameter name - '//param//' - invalid'
            call printo ( atext )
            loop = .true.
            isok = .false.
         elseif ( .not.def .and.nvals.ne.num ) then
            call pargi ( num )
            call printd ( ' ERROR: must input %d values' )
            loop = .true.
            isok = .false.
         else
            rva = rv(1)
            do k = 1, num
               rva = max(rva,rv(k))
            enddo
            if ( rmax.lt.rva ) then
               if ( abs(rmax).ge.1.0e7 ) then
                  write ( atext,'('' ERROR: over maximum value of '',
     +                                            g15.7)' ) rmax
               else
                  write ( atext,'('' ERROR: over maximum value of '',
     +                                            f15.7)' ) rmax
               endif
               call printo ( atext )
               loop = .true.
               isok = .false.
            endif
            rva = rv(1)
            do k = 1, num
               rva = min(rva,rv(k))
            enddo
            if ( rmin.gt.rva ) then
               if ( abs(rmin).ge.1.0e7 ) then
                  write ( atext,'('' ERROR: under minimum value of '',
     +                                             g15.7)' ) rmin
               else
                  write ( atext,'('' ERROR: under minimum value of '',
     +                                             f15.7)' ) rmin
               endif
               call printo ( atext )
               loop = .true.
               isok = .false.
            endif
         endif
         call err_rlse
         nloop = nloop + 1

      enddo

      if ( nloop.eq.(nmax+1) .and. .not.isok ) then
         call printo ( ' WARNING: Default values taken' )
      else
         do k = 1, num
            rvn(k) = rv(k)
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESC_GEN -- Get a character string from a file descriptor
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesc_gen ( name, ext, dscr, cv, cdef, num, istat )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	name		!i: Parameter name for file
      character*(*)	ext		!i: Extension
      character*(*)	dscr		!i: Descriptor
      character*(*)	cv		!o: String got
      character*(*)	cdef		!i: Default string on failure
      integer		num		!o: No of lines got (min=1)
      integer		istat		!o: 0 => ok;1 = failure
C--
      integer istata, istatb, ndf
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( dscr.eq.' ' ) then
         istat = 1
         cv = ' '
         return
      endif

      istata = SAI__OK
      istat = 1
      istatb = 0
      cv = cdef
      num = 1
      call ndf_exist ( name, 'READ', ndf, istata )
      if ( ndf.eq.NDF__NOID ) return
      call ndf_xstat ( ndf, ext, there, istata )
      if ( there ) then
         if ( ( dscr.eq.'TITLE' ) .or. ( dscr.eq.'LABEL') .or.
     + 			( dscr.eq.'UNITS' ) ) then
	    call ndf_cget ( ndf, dscr, cv, istata )
         else
            call ndf_xgt0c ( ndf, ext, dscr, cv, istata )
         endif
      else
         if ( ext.eq.'STARMAN' ) then
            istatb = 1
            call printo(' WARNING: Cant access descriptors in '//
     +                    'file: If needed, use IMPORT' )
            call printo('         to load STARMAN extension from'//
     +                            ' FITS or other extension.')
            call printo('         Another way is via KAPPA FITSIMP.')
         endif
      endif
      call ndf_annul ( ndf, istata )

      istat = 0
      if ( istata.ne.SAI__OK .or. istatb.ne.0 ) then
         cv = cdef
         istat = 1
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESC -- Get a character string from a file STARMAN descriptor
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesc ( name, dscr, cv, cdef, num, istat )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	name		!i: Parameter name for file
      character*(*)	dscr		!i: Descriptor
      character*(*)	cv		!o: String got
      character*(*)	cdef		!i: Default string on failure
      integer		num		!o: No of lines got (min=1)
      integer		istat		!o: 0 => ok;1 = failure
C--
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call gtdesc_gen ( name, 'STARMAN', dscr, cv, cdef, num, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESI -- Get an integer value from the descriptor
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesi ( file, dscr, iv, idef, istat )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*) file		!i: Parameter name for file
      character*(*) dscr		!i: Descriptor name
      integer iv			!o: descriptor value
      integer idef			!i: value loaded on failure
      integer istat			!o: 0=success;1=failure
C--
      integer istata, istatb, ndf
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( dscr.eq.' ' ) then
         istat = 1
         iv = 0
         return
      endif

      istata = SAI__OK
      istat = 1
      istatb = 0
      iv = idef
      call ndf_exist ( file, 'READ', ndf, istata )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istata )
      if ( there ) then
         call ndf_xgt0i ( ndf, 'STARMAN', dscr, iv, istata )
      else
         istatb = 1
         call printo(' WARNING: Cant access descriptors in '//
     +                    'file: If needed, use IMPORT' )
         call printo('         to load STARMAN extension from'//
     +                            ' FITS or other extension.')
         call printo('         Another way is via KAPPA FITSIMP.')
      endif
      call ndf_annul ( ndf, istata )

      istat = 0
      if ( istata.ne.SAI__OK .or. istatb.ne.0 ) then
         iv = idef
         istat = 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESN -- Get the name of the 'nth' descriptor of a file
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesn ( file, numd, name, istat )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Parameter name for file
      integer		numd		!i: Number of descriptor
      character*(*)	name		!o: Name of descriptor
      integer		istat		!o: 0 => ok;1 = failure
C--
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call gtdesn_gen ( file, numd, 'STARMAN', name, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESN_GEN -- Get the name of the 'nth' descriptor of a file extension
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesn_gen ( file, numd, ext, name, istat )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Parameter name for file
      integer		numd		!i: Number of descriptor
      character*(*)	ext		!i: Name of extension
      character*(*)	name		!o: Name of descriptor
      integer		istat		!o: 0 => ok;1 = failure
C--
      integer istata, istatb, ndf, ncomp
      character*(DAT__SZLOC) loc, dloc
      logical there
Cbegin


      if ( ST_FAILED ) return

      istata = SAI__OK

      istat =  1
      istatb = 0
      call ndf_exist ( file, 'READ', ndf, istata )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, ext, there, istata )
      if ( there ) then
         call ndf_xloc ( ndf, ext, 'READ', loc, istata )
         call dat_ncomp ( loc, ncomp, istata )
         istat = 0
         if ( numd.le.ncomp ) then
            call dat_index ( loc, numd, dloc, istata )
            call dat_name ( dloc, name, istata )
            call dat_annul ( dloc, istata )
         else
            name = ' '
            istat = 1
         endif

         call dat_annul ( loc, istata )
      else
         if ( ext.eq.'STARMAN' ) then
            istatb = 1
            call printo(' WARNING: Cant access descriptors in '//
     +                    'file: If needed, use IMPORT' )
            call printo('         to load STARMAN extension from'//
     +                            ' FITS or other extension.')
            call printo('         Another way is via KAPPA FITSIMP.')
         endif
      endif

      call ndf_annul ( ndf, istata )

      if ( istata.ne.SAI__OK .or. istatb.ne.0 ) then
         name = ' '
         istat = 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTDESR -- Get a real value from the descriptor
C
C  a j penny                     dao          1988-04-19

      subroutine gtdesr ( file, dscr, rv, rdef, istat )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*) file		!i: Parameter name for file
      character*(*) dscr		!i: Descriptor name
      real	rv 			!o: descriptor value
      real	rdef			!i: value loaded on failure
      integer istat			!o: 0=success;1=failure
C--
      integer istata, istatb, ndf
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( dscr.eq.' ' ) then
         istat = 1
         rv = 0.0
         return
      endif

      istata = SAI__OK
      istat = 1
      istatb = 0
      rv = rdef
      call ndf_exist ( file, 'READ', ndf, istata )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istata )
      if ( there ) then
         call ndf_xgt0r ( ndf, 'STARMAN', dscr, rv, istata )
      else
         istatb = 1
         call printo(' WARNING: Cant access descriptors in '//
     +                    'file: If needed, use IMPORT' )
         call printo('         to load STARMAN extension from'//
     +                            ' FITS or other extension.')
         call printo('         Another way is via KAPPA FITSIMP.')
      endif
      call ndf_annul ( ndf, istata )

      istat = 0
      if ( istata.ne.SAI__OK .or. istatb.ne.0 ) then
         rv = rdef
         istat = 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTIMID -- Get descriptors BSCALE, BZERO, INVAL, TITLE form an int*4 image
C
C  alan penny              ral          1990 Jan

      subroutine gtimid ( name, bs, bz, inval, title, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      character*(*) name        !i: parameter name of image
      real          bs          !o: Image scale
      real          bz          !o: Image zero
      integer       inval       !o: Image bad pixel flag
      character*(*) title       !o: Image title
      integer       ierr        !o: Error flag (0=ok)
C--
      integer ierra, ierrb, ierrc, ierrd, num, iv
Cbegin


      if ( ST_FAILED ) return

      iv = INT_INVALI
      call gtdesi ( name, 'INVAL', inval,     iv, ierra )
      call gtdesr ( name, 'BZERO',    bz,    0.0, ierrb )
      call gtdesr ( name, 'BSCALE',   bs,    1.0, ierrc )
      call gtdesc ( name, 'TITLE', title,    ' ', num, ierrd )
      ierr = max(ierra,ierrb,ierrc,ierrd)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTIMRD -- Get descriptors BSCALE, BZERO, RINVAL, TITLE form a real image
C
C  alan penny              ral          1990 Jan

      subroutine gtimrd ( name, bs, bz, rinval, title, ierr )

      implicit none
      include 'ST_LIMITS_INC'
      include 'STARMAN_INC'

      character*(*) name        !i: parameter name of image
      real          bs          !o: Image scale
      real          bz          !o: Image zero
      real          rinval      !o: Image bad pixel flag
      character*(*) title       !o: Image title
      integer       ierr        !o: Error flag (0=ok)
C--
      integer ierra, ierrb, ierrc, ierrd, num
      real rv
Cbegin


      if ( ST_FAILED ) return

      rv = INT_INVALR
      call gtdesr ( name, 'INVAL',  rinval,   rv,  ierra )
      call gtdesr ( name, 'BZERO',      bz,  0.0,  ierrb )
      call gtdesr ( name, 'BSCALE',     bs,  1.0,  ierrc )
      call gtdesc ( name, 'TITLE',   title,  ' ',  num, ierrd )
      ierr = max(ierra,ierrb,ierrc,ierrd)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTIMSD -- Get descriptors BSCALE, BZERO, INVAL, TITLE form an int*2 image
C
C  alan penny              ral          1990 Jan

      subroutine gtimsd ( name, bs, bz, inval, title, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      character*(*) name        !i: parameter name of image
      real          bs          !o: Image scale
      real          bz          !o: Image zero
      integer       inval       !o: Image bad pixel flag
      character*(*) title       !o: Image title
      integer       ierr        !o: Error flag (0=ok)
C--
      integer ierra, ierrb, ierrc, ierrd, num, iv
Cbegin


      if ( ST_FAILED ) return

      iv = INT_INVALSI
      call gtdesi ( name, 'INVAL', inval,     iv, ierra )
      call gtdesr ( name, 'BZERO',    bz,    0.0, ierrb )
      call gtdesr ( name, 'BSCALE',   bs,    1.0, ierrc )
      call gtdesc ( name, 'TITLE', title,    ' ', num, ierrd )
      ierr = max(ierra,ierrb,ierrc,ierrd)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTWRKG -- Open integer*2/integer*4/real*4 computer work space
C
C  alan penny              ral          1990 Jan

      subroutine gtwrkg ( text, n, format, ip, ierr )

      implicit none
      include 'SAE_PAR'
      include 'PSX_ERR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*) text	!i: Name to give to work space
      integer       n		!i: Size to allocate (in words)
      character*(*) format      !i: Type of space ('SHORT:INT:REAL')
      integer       ip		!o: Pointer to allocated space
      integer       ierr	!o: Error flag: 0=ok, 1=bad
C--
      integer istat, ilen, lens, next, i
      character texta*80, texte*20, iform*20
      external lens
Cbegin


      if ( ST_FAILED ) return

      ierr = 0
      ilen = min(50,lens(text))

      if ( format.eq.'SHORT' ) then
          iform = 'WORD'
          texte = ' 16-bit integers'
      elseif ( format.eq.'INT' ) then
          iform = '_INTEGER'
          texte = ' 32-bit integers'
      elseif ( format.eq.'REAL' ) then
          iform = '_REAL'
          texte = ' 32-bit reals'
      else
          call msg_setc( 'WST', text(1:ilen) )
          call printo ( ' ERROR: Programmer error in type in'//
     +                  ' work space: ^WST' )
          call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
           ierr = 1
          return
      endif

      istat = SAI__OK

      if ( ilen.gt.WS_NSIZE ) then
          call msg_setc( 'WST', text(1:ilen) )
          call printo ( ' ERROR: Programmer error in name size'//
     +                  ' in work space: ^WST' )
          call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
           ierr = 1
          return
      endif

      do i = 1, WORK_LIMIT
         if ( WS_NAME(i).eq.text ) then
          call msg_setc('WS', text(1:ilen) )
            call printo ( ' ERROR: Work space of that name'//
     +             ' still open: ^WS' )
            ierr = 1
            return
         endif
      enddo

      call find_space ( next )
      if ( next.le.0 ) then
          call msg_setc('CB', text(1:ilen) )
          call printo ( ' ERROR: No spaces left in ws common'//
     +                  ' block: ^CB')
          ierr = 1
          return
      else
         if ( iform.ne.'WORD' ) then
             call psx_calloc ( n, iform, ip, istat )
         else
             call psx_malloc ( n*2, ip, istat )
         endif
      endif

      if ( istat.ne.SAI__OK ) then
          call msg_setc( 'WS', text(1:ilen) )
          call printo ( ' ERROR: Cant open work space because ^WS')
          write ( texta, '(1x,''   Desired work size = ''
     +           ,i7, a20)') n, texte
          call printo ( texta )
          call printo ( '    May be too large or invalid' )
          ierr = 1
      else
          WS_NAME(next) = text(1:ilen)
          CPNTR(next) = ip
          WS_COUNTER = WS_COUNTER + 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISBSWOP -- Is the computer swopping bytes on read (only if osf1)?
C
C   alan penny               ral         1994 Dec

      subroutine isbswop ( flag )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      logical   flag		!o: .true. if is Byte swopping; .false. if not
C--
      character*132 sysname, nodename, release, version, machine, cv
      integer status
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK
      call psx_uname ( sysname, nodename, release, version, machine,
     +                 status )
      call lowcase ( sysname, cv )
      flag = .false.
      if ( index(cv,'osf1').ne.0 ) flag = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISVMS -- Is the computer running VMS?
C
C   alan penny               ral         1990 Jan

      subroutine isvms ( flag )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      logical   flag		!o: .true. if is VMS; .false. if not
C--
      character*132 sysname, nodename, release, version, machine
      integer status
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK
      call psx_uname ( sysname, nodename, release, version, machine,
     +                 status )
      flag = .false.
      if ( index(sysname,'VMS').ne.0 ) flag = .true.
      if ( index(sysname,'vms').ne.0 ) flag = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMGR -- Open an input 2d read-only generic image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimgr ( name, ipin, nx, ny, gtype, def, ierr )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'NDF_ERR'
      include 'PAR_ERR'
      include 'STARMAN_INC'
      include 'ST_ADAM_INC'

      character*(*) name		!i: Name of parameter to get image name
      integer       ipin		!o: Image pointer
      integer       nx			!o: Image X size
      integer       ny			!o: Image Y size
      character*(*) gtype	        !o: Image type ('REAL'/'INT'/'SHORT'/'USHORT')
      logical       def			!i: if true, null reply accepted
      integer       ierr		!o: Error 0=ok, 1=bad, 2=null and acceptable
C--
      integer nsize(2), ndim, istat, istata, nloop, nmax, i, ndf, n
      character*(NDF__SZTYP)  ggtype
      logical loop
      character text*45, textc*45, atext*70
Cbegin


      if ( ST_FAILED ) return

      nloop = 1
      loop = .true.
      nmax = 4
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         istat = SAI__OK
         call err_mark
	 call ndf_assoc ( name, 'UPDATE', ndf, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         endif
         call err_rlse
         call ndf_type ( ndf, 'DATA', ggtype, istat )
 	 call ndf_dim ( ndf, 2, nsize, ndim, istat )
	 call ndf_map ( ndf, 'DATA', ggtype, 'READ', ipin, i, istat )

         gtype = 'SHORT'
         if ( ggtype.eq.'_REAL' )    gtype = 'REAL'
         if ( ggtype.eq.'_INTEGER' ) gtype = 'INT'
         if ( ggtype.eq.'_WORD' )    gtype = 'SHORT'
         if ( ggtype.eq.'_UWORD' )   gtype = 'USHORT'

         if ( istat.eq.SAI__OK .and. ndim.eq.2 ) then
            ierr = 0
         elseif ( istat.eq.PAR__NULL ) then
            if ( .not.def ) then
               atext = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) atext = ' ERROR: Null '//
     +                           'entry unacceptable - try again'
               call printo ( atext )
               loop = .true.
               ierr = 1
            else
               ierr = 2
            endif
            call err_load ( text, n, textc, i, istat )
         else
            ierr = 1
            call printo ( ' ERROR: Cant open file because - ' )
            if ( istat.eq.NDF__ACDEN ) then
               call printo ( '   Image could not be accessed' )
            elseif ( istat.eq.PAR__INVST ) then
               call printo ( '   Programmer error -  '//
     +                       'Parameter name invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__TYPIN ) then
               call printo ( '   Programmer error -  '//
     +                       'Type code invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__DIMIN ) then
               call printo ( '   Image dimensions invalid' )
               call pargi ( ndim )
               call printd (
     +         '   Image has %d dimensions - should only have 2' )
               call printo ( ' ' )
            elseif ( istat.eq.NDF__XSDIM ) then
               call printo (
     +         '   Image dimensions too large - should only have 2' )
               call printo ( ' ' )
            else
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call printo ( ' ERROR: Error on opening image' )
               call pargi ( istat )
               call printd ( '       - ADAM error number %d ' )

               call printo ( '       Check in /star/include files' )
               call printo ( '       Perhaps program.sdf file '//
     +                       'corrupted in adam dir' )
               call printo  ( ' ' )
               call err_flush ( istat )
               call printo ( ' ' )
            endif
         endif

         if ( loop ) then
            istata = istat
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then
         nx = nsize(1)
         ny = nsize(2)
         call space_ndf ( i )
         if ( i.gt.0 ) then
            NDF_NAME(i) = name
            SNDF(i) = ndf
         else
            call printo ( 'No space left in NDF_NAME' )
         endif
      else
         if (nloop.eq.(nmax+1)) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +      call ndf_annul ( ndf, istat )
         call par_cancl ( name, istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMGW -- Open an output 2d write-only generic image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opimgw ( name, gtype, ip, nx, ny, def, ierr )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'NDF_PAR'
      include 'NDF_ERR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*) name		!i: Parameter name for image
      character*(*) gtype        	!i: Image type
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
      logical loop
      integer nloop, nmax, i, istat, istata, udims(2), ldims(2),
     +        ndf, n
      character*(DAT__SZLOC) loc
      character*(NDF__SZTYP) ggtype
      character text*40, textc*40, texta*72, atext*70
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      ggtype = '_WORD'
      if ( gtype.eq.'REAL' )   ggtype = '_REAL'
      if ( gtype.eq.'INT' )    ggtype = '_INTEGER'
      if ( gtype.eq.'SHORT' )  ggtype = '_WORD'
      if ( gtype.eq.'USHORT' ) ggtype = '_UWORD'

      ldims(1) = 1
      ldims(2) = 1
      udims(1) = nx
      udims(2) = ny

      nloop = 1
      loop = .true.
      nmax = 4
      do while ( loop .and. nloop.le.nmax )

         istat = SAI__OK
         call err_mark
	 call ndf_creat ( name, ggtype, 2, ldims, udims, ndf, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            ierr = 1
            return
         elseif ( istat.eq.SAI__OK ) then				!OK
            ierr = 0
            loop = .false.
         elseif ( istat.eq.PAR__NULL ) then				!Null response
            if ( def ) then
               loop = .false.
               ierr = 2
            else
               atext = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) atext = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
               call printo ( atext )
               ierr = 1
            endif
            call err_load ( text, n, textc, i, istat )
         elseif ( istat.ne.SAI__OK ) then				!Error
            ierr = 1
            call printo ( ' ERROR: Cant open file because - ' )
            if ( istat.eq.NDF__ACDEN ) then
               call printo ( '   Image could not be accessed' )
            elseif ( istat.eq.PAR__INVST ) then
               call printo ( '   Programmer error -  '//
     +                       'Parameter name invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__FTPIN ) then
               call printo ( '   Image type impossible' )
            elseif ( istat.eq.NDF__DIMIN ) then
                write ( texta,'(''   Invalid size:'',
     +                        i7,'' x '',i7)' ) nx, ny
               call printo ( texta )
            else
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call printo ( ' ERROR: Error on opening image' )
               call pargi ( istat )
               call printd ( '       - ADAM error number %d ' )
               call printo ( '       Check in /star/include files' )
               call printo ( '       Perhaps program.sdf file '//
     +                       'corrupted in adam dir' )
               call printo  ( ' ' )
               call err_flush ( istat )
               call printo ( ' ' )
            endif
         endif
         call err_rlse

         if ( loop ) then
            istata = istat
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then						!Made image, so put size
         call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
         call dat_annul ( loc,istat )
         call ndf_map ( ndf, 'DATA', ggtype, 'WRITE', ip, i, istat )
         call ptdesi ( name, 'NAXIS', 2 )
         call ptdesi ( name, 'NAXIS1', nx )
         call ptdesi ( name, 'NAXIS2', ny )
         call space_ndf ( i )
         if ( i.gt.0 ) then
            NDF_NAME(i) = name
            SNDF(i) = ndf
         else
            call printo ( 'No space left in NDF_NAME' )
         endif
      else
         if (nloop.eq.(nmax+1)) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +      call ndf_annul ( ndf, istat )
         call par_cancl ( name, istat )
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4GW -- Open an output 4d write-only generic image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opim4gw ( name, gtype, ip, nx, ny, nz,nt,def,ierr)

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'NDF_PAR'
      include 'NDF_ERR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*) name		!i: Parameter name for image
      character*(*) gtype        	!i: Image type
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      integer	    nz			!i: Image Z size
      integer	    nt			!i: Image T size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
      logical loop
      integer nloop, nmax, i, istat, istata, udims(4), ldims(4),
     +        ndf, n
      character*(DAT__SZLOC) loc
      character*(NDF__SZTYP) ggtype
      character text*40, textc*40, texta*72, atext*70
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      ggtype = '_WORD'
      if ( gtype.eq.'REAL' )   ggtype = '_REAL'
      if ( gtype.eq.'INT' )    ggtype = '_INTEGER'
      if ( gtype.eq.'SHORT' )  ggtype = '_WORD'
      if ( gtype.eq.'USHORT' ) ggtype = '_UWORD'

      ldims(1) = 1
      ldims(2) = 1
      ldims(3) = 1
      ldims(4) = 1
      udims(1) = nx
      udims(2) = ny
      udims(3) = nz
      udims(4) = nt

      nloop = 1
      loop = .true.
      nmax = 4
      do while ( loop .and. nloop.le.nmax )

         istat = SAI__OK
         call err_mark
	 call ndf_creat ( name, ggtype, 4, ldims, udims, ndf, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            ierr = 1
            return
         elseif ( istat.eq.SAI__OK ) then				!OK
            ierr = 0
            loop = .false.
         elseif ( istat.eq.PAR__NULL ) then				!Null response
            if ( def ) then
               loop = .false.
               ierr = 2
            else
               atext = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) atext = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
               call printo ( atext )
               ierr = 1
            endif
            call err_load ( text, n, textc, i, istat )
         elseif ( istat.ne.SAI__OK ) then				!Error
            ierr = 1
            call printo ( ' ERROR: Cant open file because - ' )
            if ( istat.eq.NDF__ACDEN ) then
               call printo ( '   Image could not be accessed' )
            elseif ( istat.eq.PAR__INVST ) then
               call printo ( '   Programmer error -  '//
     +                       'Parameter name invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__FTPIN ) then
               call printo ( '   Image type impossible' )
            elseif ( istat.eq.NDF__DIMIN ) then
                write ( texta,'(''   Invalid size:'',
     +                   i7,'' x '',i7,'' x '',i7,'' x '',i7)' )
     +                          nx, ny, nz, nt
               call printo ( texta )
            else
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call printo ( ' ERROR: Error on opening image' )
               call pargi ( istat )
               call printd ( '       - ADAM error number %d ' )
               call printo ( '       Check in /star/include files' )
               call printo ( '       Perhaps program.sdf file '//
     +                       'corrupted in adam dir' )
               call printo  ( ' ' )
               call err_flush ( istat )
               call printo ( ' ' )
            endif
         endif
         call err_rlse

         if ( loop ) then
            istata = istat
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then						!Made image, so put size
         call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
         call dat_annul ( loc,istat )
         call ndf_map ( ndf, 'DATA', ggtype, 'WRITE', ip, i, istat )
         call ptdesi ( name, 'NAXIS', 4 )
         call ptdesi ( name, 'NAXIS1', nx )
         call ptdesi ( name, 'NAXIS2', ny )
         call ptdesi ( name, 'NAXIS3', nz )
         call ptdesi ( name, 'NAXIS4', nt )
         call space_ndf ( i )
         if ( i.gt.0 ) then
            NDF_NAME(i) = name
            SNDF(i) = ndf
         else
            call printo ( 'No space left in NDF_NAME' )
         endif
      else
         if (nloop.eq.(nmax+1)) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +      call ndf_annul ( ndf, istat )
         call par_cancl ( name, istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4GR -- Open an input read-only generic image up to 4 dimensions
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opim4gr ( name, ipin, nx, ny, nz, nt, ndim, gtype,
     +                     def, ierr )

      implicit none
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'NDF_ERR'
      include 'PAR_ERR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*) name		!i: Name of parameter to get image name
      integer       ipin		!o: Image pointer
      integer       nx			!o: Image X size
      integer       ny			!o: Image Y size
      integer       nz			!o: Image Z size
      integer       nt			!o: Image T size
      integer       ndim		!o: No of dimensions
      character*(*) gtype	        !i: Image type ('REAL'/'INT'/'SHORT'/'USHORT')
      logical       def			!i: if true, null reply accepted
      integer       ierr		!o: Error 0=ok, 1=bad, 2=null and acceptable
C--
      integer nsize(10), istat, istata, nloop, nmax, i, ndf, n
      character*(NDF__SZTYP)  ggtype
      logical loop
      character text*45, textc*45, atext*70
Cbegin


      if ( ST_FAILED ) return

      nloop = 1
      loop = .true.
      nmax = 4
      do while ( loop .and. nloop.le.nmax )
         loop = .false.

         istat = SAI__OK
         call err_mark
	 call ndf_assoc ( name, 'READ', ndf, istat )
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         endif
         call err_rlse
	 call ndf_dim ( ndf, 4, nsize, ndim, istat )
	 call ndf_type ( ndf, 'DATA', ggtype, istat )
	 call ndf_map ( ndf, 'DATA', ggtype, 'READ', ipin, i, istat )

         gtype = 'SHORT'
         if ( ggtype.eq.'_REAL' )    gtype = 'REAL'
         if ( ggtype.eq.'_INTEGER' ) gtype = 'INT'
         if ( ggtype.eq.'_WORD' )    gtype = 'SHORT'

         if ( istat.eq.SAI__OK .and. ndim.le.4 .and. ndim.ge.2 ) then
            ierr = 0
         elseif ( istat.eq.PAR__NULL ) then
            if ( .not.def ) then
               atext = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) atext = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
               call printo ( atext )
               loop = .true.
               ierr = 1
            else
               ierr = 2
            endif
            call err_load ( text, n, textc, i, istat )
         else
            ierr = 1
            call printo ( ' ERROR: Cant open file because - ' )
            if ( istat.eq.NDF__ACDEN ) then
               call printo ( '   Image could not be accessed' )
            elseif ( istat.eq.PAR__INVST ) then
               call printo ( '   Programmer error -  '//
     +                       'Parameter name invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__TYPIN ) then
               call printo ( '   Programmer error -  '//
     +                       'Type code invalid' )
               call printo ( '        Code needs rewriting:'//
     +                 ' Contact person who wrote the program' )
            elseif ( istat.eq.NDF__DIMIN ) then
               call printo ( '   Image dimensions invalid' )
               call pargi ( ndim )
               call printd (
     +         '    They are %d and should be between 2 and 4 ' )
               call printo ( ' ' )
            elseif ( istat.eq.NDF__XSDIM ) then
               call printo (
     +     '   Image dimensions too large - should be between 2 and 4' )
               call printo ( ' ' )
            else
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call printo ( ' ERROR: Error on opening image' )
               call pargi ( istat )
               call printd ( '       - ADAM error number %d ' )
               call printo ( '       Check in /star/include files' )
               call printo ( '       Perhaps program.sdf file '//
     +                       'corrupted in adam dir' )
               call printo  ( ' ' )
               call err_flush ( istat )
               call printo ( ' ' )
            endif
         endif

         if ( loop ) then
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then
         nx = nsize(1)
         ny = nsize(2)
         if ( ndim.ge.3 ) nz = nsize(3)
         if ( ndim.ge.4 ) nt = nsize(4)
         call space_ndf ( i )
         if ( i.gt.0 ) then
            NDF_NAME(i) = name
            SNDF(i) = ndf
         else
            call printo ( 'No space left in NDF_NAME' )
         endif
      else
         if (nloop.eq.(nmax+1)) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +      call ndf_annul ( ndf, istat )
         call par_cancl ( name, istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPTABR -- Open an input read-only table
C  The parameter name is left attached
C
C   alan penny                   ral         1990 Jan

      subroutine optabr ( name, ip, nx, ny, def, ierr )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_ERR'
      include 'NDF_PAR'
      include 'ARY_ERR'
      include 'PAR_ERR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character name*(*)		!i: parameter name for table
      integer   ip			!o: Pointer to table
      integer   nx			!o: No of columns (inc 5 for name column)
      integer   ny			!o: No of rows
      logical	def			!i: if true, null reply acceptable
      integer   ierr			!o: Error flag 0=ok; 1=bad; 2=blank
					!   and ok; 3 = already opened
C--
      character*6 frtype
      character*(DAT__SZLOC) loc, dloc, nloc
      character*(NDF__SZFRM) form
      character*20 texta, textb, ver, nmach, nsys, sys, mach
      character*72 text
      logical  loop, type, chr_simlr, doit
      integer  nloop, nmax, k, n, istat, istata, dim(2), ndf,
     +         icptr, ierra
      external chr_simlr
Cbegin


      if ( ST_FAILED ) return

      nx = 1
      ny = 1

      call err_mark

      nloop = 1
      loop = .true.
      type = .true.
      nmax = 4
      do while ( loop .and. nloop.le.nmax )

         istat = SAI__OK

         call ndf_assoc ( name, 'UPDATE', ndf, istat )			!Open file
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         endif

         call ndf_dim ( ndf, 2, dim, k, istat )
         call ndf_map ( ndf, 'DATA', '_REAL', 'UPDATE', ip, k, istat )

         ierr = 1
         doit = .true.
         if ( istat.eq.PAR__NULL ) then					!Null reply
            doit = .false.
            if ( def ) then
               loop = .false.
               ierr = 2
            else
               text = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) text = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
               call printo ( text )
            endif
            call err_load ( texta, n, textb, k, istat )
         endif

         if ( istat.eq.ARY__CFLAC ) then				!Already accessed file
            doit = .false.
            loop = .false.
            ierr = 3
            do while ( istat.ne.SAI__OK )
               call err_load ( texta, n, textb, k, istat )
            enddo
            call err_flush ( istat )
            call err_annul ( istat )
            istata = SAI__OK
            call ndf_annul ( ndf, istata )
            call par_cancl ( name, istata )
         endif

         if ( istat.eq.NDF__ACDEN ) then				!Not accessible
            doit = .false.
            call printo ( ' ERROR: Cant access file '//
     +             ' - Table could not be accessed' )
            call err_rep ( ' ',  ' ERROR: ^status', istat )
            call err_flush ( istat )
            call err_annul ( istat )
         elseif ( istat.eq.PAR__INVST ) then				!Invalid parameter name
            doit = .false.
            call printo ( ' ERROR: Invalid parameter name '//
     +             ' - Table could not be accessed' )
            call err_rep ( ' ',  ' ERROR: ^status', istat )
            call err_flush ( istat )
            call err_annul ( istat )
         elseif ( istat.eq.NDF__DIMIN ) then				!Invalid size
            doit = .false.
            call printo ( ' ERROR: Invalid size '//
     +             ' - Table could not be accessed' )
            call err_rep ( ' ',  ' ERROR: ^status', istat )
            call err_flush ( istat )
            call err_annul ( istat )
          elseif ( istat.eq.NDF__XSDIM ) then				!Invalid size
            doit = .false.
            call printo ( ' ERROR: Invalid size '//
     +             ' - Table could not be accessed' )
            call err_rep ( ' ',  ' ERROR: ^status', istat )
            call err_flush ( istat )
            call err_annul ( istat )
          elseif ( istat.ne.SAI__OK .and. istat.ne.PAR__NULL ) then	!Unknown error
            doit = .false.
            call printo ( ' ERROR: Error on opening table ' )
            call pargi ( istat )
            call printd ( '       - ADAM error number %d ' )
            call printo ( '       Check in /star/include files' )
            call printo ( '       Perhaps program.sdf file '//
     +                    'corrupted in adam dir' )
            call printo  ( ' ' )
            call err_rep ( ' ',  ' ERROR: ^status', istat )
            call err_flush ( istat )
            call err_annul ( istat )
            call printo ( ' ' )
         endif

         if ( doit ) then						!Ok - Check dimensions and type
	    nx = dim (1)
	    ny = dim (2)
            call ndf_cget ( ndf, 'LABEL', frtype, istat )
            type = chr_simlr ( frtype, 'XYLIST' )
            if ( istat.ne.SAI__OK ) then
               call printo ( ' ERROR: Error in opening table' )
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call err_flush ( istat )
               call err_annul ( istat )
            elseif ( .not.type .or. nx.lt.6 .or. ny.lt.1 ) then
               call printo (' ERROR: Not a proper table - or size '//
     +                    'wrong - Table could not be accessed' )
            else
               loop = .false.
               ierr = 0
            endif
         endif

         if ( loop ) then
            istata = istat
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then

         call psx_uname ( nsys, texta, textb, ver, nmach, istat )
         call gtdesc ( name, 'SYSNAME', sys, 'NO', k, ierra )
         call gtdesc ( name, 'MACHINE', mach, 'NO', k, ierra )  	!Get current machine + OS and file

         if ( sys.ne.nsys .or. mach.ne.nmach ) then             	!NB Default is that the file was
                                                   			! not written on this machine or
            call ndf_loc ( ndf, 'READ', loc, istat )			! operating system
            call ndf_form ( ndf, 'DATA', form, istat )

            if ( form.eq.'SIMPLE' ) then                        	!Find data array
               call dat_find ( loc, 'DATA_ARRAY', nloc, istat )
               call dat_find ( nloc, 'DATA', dloc, istat )
            else
               call dat_find ( loc, 'DATA_ARRAY', dloc, istat )
            endif

            call dat_basic ( dloc, 'READ', icptr, k, istat )   		!Map as basic

            if ( istat.eq.SAI__OK ) then				!If okay  copy over names and write
               call copzz ( %val(icptr), nx*4, ny, 1, 20, 1,
     +                      ny, %val(ip), nx*4, ny, 1, 1 )
            endif

            call dat_annul ( dloc, istat )
            call dat_annul ( loc, istat )
            if ( form.eq.'SIMPLE' ) call dat_annul ( nloc, istat )

         endif

         call space_ndf ( k )
         if ( k.gt.0 ) then
            NDF_NAME(k) = name
            SNDF(k) = ndf
         else
            call printo ( ' ERROR: No space left in NDF_NAME' )
         endif

      elseif ( ierr.eq.1 .or. ierr.eq.2 ) then
         if ( nloop.eq.(nmax+1) ) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL ) then
            call ndf_annul ( ndf, istat )
            call printo ( ' ERROR: Error in opening table' )
            call err_rep ( ' ',  ' ERROR: ^status', istata )
            call err_flush ( istat )
            call err_annul ( istat )
         endif
         call par_cancl ( name, istat )

      endif

      call err_rlse


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPTABW -- Open an output write-only table
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine optabw ( name, ip, nx, ny, def, ierr )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'PAR_ERR'
      include 'NDF_ERR'
      include 'NDF_PAR'
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      character*(*)	name		!i: Parameter name for table
      integer		ip		!o: Pointer to table
      integer		nx		!i: No of columns (+5 for name column)
      integer		ny		!i: No of rows
      logical		def		!i: If true, null reply is accepted
      integer		ierr		!o: Error flag 0=ok;1=bad;2=blank and ok
C--
      logical loop
      integer nloop, nmax, istat, istata, iv, lbnd(2),ubnd(2), ndf,
     +        k, n
      character*(DAT__SZLOC) loc
      character*(NDF__SZTYP) gtype
      character text*55, textc*55, atext*70
      character*20 sys, node, release, ver, mach
Cbegin


      if ( ST_FAILED ) return

      if ( nx*ny.lt.6 ) then
         ierr = 1
         iv = nx - 5
         write ( text, '('' ERROR: Invalid table size - '',i7,
     +                   '' x '',i7)' ) iv, ny
         call printo ( text//' - Wont try to open file for table' )
         return
      endif

      nloop = 1
      loop = .true.
      lbnd (1) = 1
      lbnd (2) = 1
      ubnd (1) = nx
      ubnd (2) = ny
      gtype = '_REAL'

      nmax = 4
      do while ( loop .and. nloop.le.nmax )

         istat = SAI__OK
         call err_mark
 	 call ndf_creat ( name, gtype, 2, lbnd, ubnd, ndf, istat )	!Open table
         if ( istat.eq.PAR__ABORT ) then
            ST_FAILED = .true.
            call err_annul ( istat )
            call err_rlse
            return
         elseif ( istat.eq.PAR__NULL ) then				!Null response
            if ( def ) then
               loop = .false.
               ierr = 2
            else
               atext = ' ERROR: Null entry unacceptable'
               if ( nloop.le.(nmax-1) ) atext = ' ERROR: Null entry '//
     +                           'unacceptable - try again'
               call printo ( atext )
               ierr = 1
            endif
            call err_load ( text, n, textc, k, istat )
         elseif ( istat.ne.SAI__OK ) then				!Bad response
            if ( istat.eq.NDF__ACDEN ) then
               call printo ( ' ERROR: Cant access file' )
            elseif ( istat.eq.PAR__INVST ) then
               call printo ( ' ERROR: Invalid parameter name'//
     +                         ' - Cant open that file' )
            elseif ( istat.eq.NDF__DIMIN ) then
               iv = nx - 5
               write ( text, '('' ERROR: Invalid table size - '',i7,
     +                         '' x '',i7)' ) iv, ny
               call printo ( text//' - Cant open file ' )
            else
               call err_rep ( ' ',  ' ERROR: ^status', istat )
               call printo ( ' ERROR: Error on opening table ' )
               call pargi ( istat )
               call printd ( '       - ADAM error number %d ' )
               call printo ( '       Check in /star/include files' )
               call printo ( '       Perhaps program.sdf file '//
     +                       'corrupted in adam dir' )
               call printo  ( ' ' )
               call err_flush ( istat )
               call printo ( ' ' )
            endif
            ierr = 1
         else								!OK
            ierr = 0
            loop = .false.
         endif
         call err_rlse

         if ( loop ) then
            istata = istat
            istat = SAI__OK
            if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +         call ndf_annul ( ndf, istat )
            call par_cancl ( name, istat )
         endif

         nloop = nloop + 1
      enddo

      if ( ierr.eq.0 ) then						!If opened, load size
         call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
         call dat_annul ( loc, istat )
         call ndf_map ( ndf, 'DATA', gtype, 'WRITE', ip, k, istat )
         call ndf_cput ( 'XYLIST', ndf, 'LABEL', istat)
         call psx_uname ( sys, node, release, ver, mach, istat )
         call ptdesc ( name, 'SYSNAME', sys )
         call ptdesc ( name, 'MACHINE', mach )
         call ptdesi ( name, 'NITEM', nx )
         call ptdesi ( name, 'LSTLEN', ny )
         call space_ndf ( k )
         if ( k.gt.0 ) then
            NDF_NAME(k) = name
            SNDF(k) = ndf
         else
            call printo ( ' ERROR: No space left in NDF_NAME' )
         endif
      else
         if (nloop.eq.(nmax+1)) call printo
     +                          (' ERROR: Too many failures')
         istata = istat
         istat = SAI__OK
         if ( istata.ne.NDF__NOID .and. istata.ne.PAR__NULL )
     +      call ndf_annul ( ndf, istat )
         call par_cancl ( name, istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRINTO -- Write a line out to the CL
C Uses MSG_OUT and so output tokens can be set immediately prior
C to calling this routine
C   alan penny                ral        1990 Jan

      subroutine printo ( text )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'

      character*(*) text	!i: Text to output
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      if ( text(1:1).eq.'0' .or. text(1:1).eq.'1' .or.
     +     text(1:1).eq.'+' ) then
C     prepend a space
         call msg_setc('TEXT', text)
         call msg_out ( ' ', ' ^TEXT', istat )
      elseif ( text.eq.' ' ) then
         call msg_out ( ' ', '  ', istat )
      else
         call msg_out ( ' ', text, istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PTDESCN -- Write character strings into the header area of an opened file
C
C    a j penny                 ral                  1990-01

      subroutine ptdescn ( file, name, cval, n )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Parameter name for file to write to
      character*(*)	name		!i: Name to give to value
      integer           n		!i: Number of values to write
      character*(*)	cval(n)		!i: value to write
C--
      integer istat, ndf, i(1), nchar, chr_size
      character*(DAT__SZLOC) loc
      external chr_size
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( name.eq.' ' ) return

      istat = SAI__OK
      i(1) = n
      nchar = chr_size ( cval(1) )

      call ndf_exist ( file, 'UPDATE', ndf, istat )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istat )
      if ( .not.there ) then
         call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
      else
         call ndf_xloc ( ndf, 'STARMAN', 'UPDATE', loc, istat )
      endif

      call cmp_modc ( loc, name, nchar, 1, i, istat )
      call cmp_put1c ( loc, name, n, cval, istat )
      call dat_annul ( loc, istat )
      call ndf_annul ( ndf, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PTDESC -- Write a character string into the header area of an opened file
C
C    a j penny                 ral                  1990-01

      subroutine ptdesc ( file, name, cval )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Parameter name for file to write to
      character*(*)	name		!i: Name to give to value
      character*(*)	cval 		!i: value to write
C--
      integer istat, ndf
      logical there
      character*(DAT__SZLOC) loc
Cbegin


      if ( ST_FAILED ) return

      if ( name.eq.' ' ) return

      istat = SAI__OK
      call ndf_exist ( file, 'UPDATE', ndf, istat )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istat )
      if ( .not.there ) then
        call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
        call dat_annul ( loc, istat )
      endif

      if ( (name.eq.'TITLE') .or. (name.eq.'LABEL') .or.
     +					(name.eq.'UNITS') ) then
	 call ndf_cput ( cval, ndf, name, istat )
      else
         call ndf_xpt0c ( cval, ndf, 'STARMAN', name, istat )
      endif

      call ndf_annul ( ndf, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PTDESI -- Write an integer into the header area of an opened file
C
C    a j penny                 ral                  1990-01

      subroutine ptdesi ( file, name, ival )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Parameter name for file to write to
      character*(*)	name		!i: Name to give to value
      integer		ival 		!i: value to write
C--
      integer istat, ndf
      character*(DAT__SZLOC) loc
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( name.eq.' ' ) return

      istat = SAI__OK
      call ndf_exist ( file, 'UPDATE', ndf, istat )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istat )
      if ( .not.there ) then
        call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
        call dat_annul ( loc, istat )
      endif

      call ndf_xpt0i ( ival, ndf, 'STARMAN', name, istat )
      call ndf_annul ( ndf, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PTDESR -- Write a real into the header area of an opened file
C
C    a j penny                 ral                  1990-01

      subroutine ptdesr ( file, name, rval )

      implicit none
      include 'DAT_PAR'
      include 'SAE_PAR'
      include 'NDF_PAR'
      include 'STARMAN_INC'

      character*(*)	file		!i: Paramter name of file to write to
      character*(*)	name		!i: Name to give to value
      real		rval 		!i: value to write
C--
      integer istat, ndf
      character*(DAT__SZLOC) loc
      logical there
Cbegin


      if ( ST_FAILED ) return

      if ( name.eq.' ' ) return

      istat = SAI__OK
      call ndf_exist ( file, 'UPDATE', ndf, istat )
      if ( ndf.eq.NDF__NOID ) return

      call ndf_xstat ( ndf, 'STARMAN', there, istat )
      if ( .not.there ) then
        call ndf_xnew ( ndf, 'STARMAN', 'EXT', 0, 0, loc, istat )
        call dat_annul ( loc, istat )
      endif

      call ndf_xpt0r ( rval, ndf, 'STARMAN', name, istat )
      call ndf_annul ( ndf, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUTGI -- Put integer number(s) to the CL
C
C    a j penny                ral         1988-09-07

      subroutine putgi ( name, num, ival )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)	name		!i: Name of parameter
      integer           num             !i: Number of values
      integer		ival(3)		!i: Value(s) to load parameter
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call par_put1i ( name, num, ival, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUTGR -- Put real number(s) to the CL
C
C    a j penny                ral         1988-09-07

      subroutine putgr ( name, num, rval )

      implicit none
      include 'SAE_PAR'
      include 'STARMAN_INC'

      character*(*)	name		!i: Name of parameter
      integer           num             !i: Number of values
      real		rval(3)		!i: Value to load parameter
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      istat = SAI__OK
      call par_put1r ( name, num, rval, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SPACE_NDF -- Find gap in ndf space common block
C
C  Patrick Morris          leeds          1991 Dec

      subroutine space_ndf ( n )

      implicit none
      include 'ST_ADAM_INC'
      include 'STARMAN_INC'

      integer	n		!o : Available space, -1 no space
C--
      logical found
Cbegin


      if ( ST_FAILED ) return

      if ( NDF_COUNTER.ge.NDF_LIMIT ) then
         n = -1
         return
      endif

      found = .false.
      n = 0
      do while ( .not.found )
         n = n + 1
         if ( n.gt.NDF_LIMIT ) then
            n = -1
            found = .true.
         else
            found = ( NDF_NAME(n).eq.'-' )
         endif
      enddo

      if ( n.gt.0 ) NDF_COUNTER = NDF_COUNTER + 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN_END -- Close down Starman controls
C
C  alan penny               ral            1993 Jan

      subroutine starman_end ( ierradam )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'

      integer    ierradam	!o: Flag for ADAM on 'failed'
C--
Cbegin


      call wrkcan ( ' ' )
      call canpar ( ' ' )

      call gd_close

      ierradam = SAI__OK
      call ndf_end ( ierradam )

      if ( ST_FAILED ) ierradam = SAI__ERROR


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN_START -- Set up Starman defaults
C
C  alan penny               ral            1993 Jan

      subroutine starman_start ( )

      implicit none
      include 'STARMAN_INC'
C--
Cbegin


      ST_FAILED = .false.

      call ndf_begin
      call pargbeg
      call wrkbeg
      call filebeg
      call ds_setup
      call gd_setup
      call ch_setup


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C WRKBEG -- Set up computer work space information
C
C  alan penny              ral          1990 Jan

      subroutine wrkbeg ( )

      implicit none
      include 'ST_ADAM_INC'

C--
      integer k
Cbegin


      WS_COUNTER = 1
      do k = 1, WORK_LIMIT
          WS_NAME(k) = '-'
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C WRKCAN -- Cancel work space name connection
C
C  alan penny              ral               1990 Jan
C
      subroutine wrkcan ( text )

      implicit none
      include 'ST_ADAM_INC'
      include 'SAE_PAR'

      character*(*)	text	!i: Name of work space to cancel
C--
      integer istat, n, lens, kl
      logical found, chr_equal
      external chr_equal, lens
Cbegin


      istat = SAI__OK

      if ( text.eq.' ' ) then

         do n = 1, WORK_LIMIT                				!Cancel all current w/s
            if ( WS_NAME(n).ne.'-' ) then
               call psx_free ( CPNTR(n), istat )
               WS_NAME(n) = '-'
               WS_COUNTER = WS_COUNTER - 1
            endif
         enddo

      else                                     				!Cancel one named w/s

         kl = min(50,lens(text))
         n = 0
         found = .false.
         do while ( .not.found )
            n = n + 1
            found = chr_equal ( text(1:kl), WS_NAME(n) )
            if ( found ) then
               call psx_free ( CPNTR(n), istat )
               WS_NAME(n) = '-'
               WS_COUNTER = WS_COUNTER - 1
            else
               if ( n.eq.WORK_LIMIT ) then
                  found = .true.
                  call msg_setc( 'WS', text(1:kl) )
                  call printo ( 'Cant find workspace to close - ^WS')
               endif
            endif
         enddo

      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is  STARFLIB_UNIX.F
C
C  Contains UNIX specific routines:-
C
C ABORS         Bitwise Boolean OR of two short vectors
C ACHT(DR:IRS)  Load a (dble:real) array into a (real:int:short) array
C ACHTB(DR:IRS) Load a (dble:real) array into a (RIS) array with magic values
C AMOVZ         Load byte vector with another
C AMOVKZ        Load byte vector with constant
C AZEROZ        Load byte vector with zero
C COPZZ         Copy part of a byte array into an area of a byte array
C MOREHELP      See if more HELP is desired
C XXTIME        Type out a message and the time


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ABORS -- Bitwise Boolean OR of two short vectors
C alan penny            ral                1988-12-29
      subroutine abors ( a, b, c, n )
      implicit none
      include 'STARMAN_INC'
      integer    n	!i: No of values
      integer*2  a(n)	!i: 1st input vector
      integer*2  b(n)	!i: 2nd input vector
      integer*2  c(n)	!o: Output vector
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, n
         c(j) = ior(a(j),b(j))
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTBRI -- Load a real array into an integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine achtbri ( a, ka, b, kb, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer	n	!i: No of points
      real	a(n)	!i: input array
      real      ka	!i: input magic value
      integer	b(n)	!o: output array
      integer   kb	!i: Output magic value
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.INT_MINIR .or.
     +        a(j).gt.INT_MAXIR ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTBRS -- Load a real array into a short integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine achtbrs ( a, ka, b, kb, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer	n	!i: No of points
      real	a(n)	!i: input array
      real      ka	!i: Input magic value
      integer*2	b(n)	!o: output array
      integer   kb	!i: Output magica value
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.INT_MINSR .or.
     +        a(j).gt.INT_MAXSR ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTBDR -- Load a double precision array into a real array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine achtbdr ( a, ka, b, kb, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision	a(n)	!i: Input array
      double precision  ka	!i: input magic value
      real		b(n)	!o: input array
      real              kb	!i: Output magic value
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dku = INT_MAXRR
      dkl = INT_MINRR
      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.dkl .or. a(j).gt.dku ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTBDI -- Load a double precision array into a integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine achtbdi ( a, ka, b, kb, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision	a(n)	!i: Input array
      double precision  ka	!i: input magic value
      integer		b(n)	!o: input array
      integer           kb	!i: Output magic value
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dkl = INT_MINIR
      dku = INT_MAXIR
      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.dkl .or. a(j).gt.dku ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTBDS -- Load a double precision array into a integer*2 array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine achtbds ( a, ka, b, kb, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision	a(n)	!i: Input array
      double precision  ka	!i: input magic value
      integer*2		b(n)	!o: input array
      integer*2          kb	!i: Output magic value
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dkl = INT_MINSR
      dku = INT_MAXSR
      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.dkl .or. a(j).gt.dku ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTRI -- Load a real array into an integer array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtri ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer	n	!i: No of points
      real	a(n)	!i: input array
      integer	b(n)	!o: output array
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         if ( a(j).gt.INT_MAXIR ) then
            b(j) = INT_MAXII
         elseif ( a(j).lt.INT_MINIR ) then
            b(j) = INT_MINII
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTRS -- Load a real array into a short integer array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtrs ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer	n	!i: No of points
      real	a(n)	!i: input array
      integer*2	b(n)	!o: output array
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         if ( a(j).gt.INT_MAXSR ) then
            b(j) = INT_MAXSS
         elseif ( a(j).lt.INT_MINSR ) then
            b(j) = INT_MINSS
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTDR -- Load a double precision array into a real array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtdr ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision	a(n)	!i: Input array
      real		b(n)	!o: input array
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dku = INT_MAXRR
      dkl = INT_MINRR
      do j = 1, n
         if ( a(j).gt.dku ) then
            b(j) = INT_MAXRR
         elseif ( a(j).lt.dkl ) then
            b(j) = INT_MINRR
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTDI -- Load a double precision array into a integer array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtdi ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision	a(n)	!i: Input array
      integer 		b(n)	!o: input array
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dkl = INT_MINIR
      dku = INT_MAXIR
      do j = 1, n
         if ( a(j).gt.dku ) then
            b(j) = INT_MAXII
         elseif ( a(j).lt.dkl ) then
            b(j) = INT_MINII
         else
            b(j) = a(j)
         endif
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTDS -- Load a double precision array into a integer*2 array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtds ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      integer		n	!i: No of points
      double precision  a(n)	!i: Input array
      integer*2 	b(n)	!o: input array
C--
      integer j
      double precision dkl, dku
Cbegin
      if ( ST_FAILED ) return

      dku = INT_MAXSR
      dkl = INT_MINSR
      do j = 1, n
         if ( a(j).gt.dku ) then
            b(j) = INT_MAXSS
         elseif ( a(j).lt.dkl ) then
            b(j) = INT_MINSS
         else
            b(j) = a(j)
         endif
      enddo

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMOVZ -- Load a byte array into another  B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovz ( a, b, n )
      implicit none
      include 'STARMAN_INC'
      integer	n	!i: No of points
      byte 	a(n)	!i: input array
      byte 	b(n)	!o: output array
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         b(j) = a(j)
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMOVKZ -- Load a byte constant into a byte array  A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovkz ( k, a, n )
      implicit none
      include 'STARMAN_INC'
      byte 	k 	!i: constant to load
      integer	n	!i: No of points
      byte 	a(n)	!o: Array
C--
      integer j
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         a(j) = k
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZEROZ -- Load zero into a byte array  A = 0
C   a j penny                    dao	         1988-05-16
      subroutine azeroz ( a, n )
      implicit none
      include 'STARMAN_INC'
      integer	n	!i: No of points
      byte 	a(n)	!o: Array
C--
      integer j
      byte bzero
      data bzero / 0 /
Cbegin
      if ( ST_FAILED ) return

      do j = 1, n
         a(j) = bzero
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPZZ -- Copy part of a byte array into an area of a byte array
C
C    a j penny                ral                 88-08-12

      subroutine copzz ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      byte 	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      byte 	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = in(i,j)
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MOREHELP -- See if more HELP is desired
C
C alan penny           RAL            1991 March

      subroutine morehelp ( more )

      implicit none
      include 'STARMAN_INC'

      logical     more		!o: True if more help wanted, false if not
C--
      character*256 text
Cbegin


      if ( ST_FAILED ) return

      write ( 6, '('' '')' )
      write ( 6, '('' Press RETURN to continue ... ''$)' )
      read ( 5, '(a)' ) text
      more = .false.
      if ( text.eq.' ' ) then
         more = .true.
         write ( 6, '('' '')' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C XXTIME -- Type out a message and the time
C
C  alan penny                      ral         1990-07-04

      subroutine xxtime ( text )

      implicit none
      include 'STARMAN_INC'

      character*(*)	text	!i: Message
C--
      integer k, lens
      character xt*8, atext*72
      external lens
Cbegin


      if ( ST_FAILED ) return

      call time ( xt )
      k = min(60,lens(text))
      write ( atext, '(1x,a,'' at '',a8)' ) text(1:k), xt
      call printo ( atext )


      end
