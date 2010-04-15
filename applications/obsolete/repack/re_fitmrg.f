*+RE_FITMRG Merge a set of RE event (FIT) files
      PROGRAM RE_FITMRG
      IMPLICIT 		NONE

*    Global constants :
      INCLUDE 		'CONSTANTS.INC'

* P. McGale - Apr 95
*-

*    Local constants:
      integer maxfls			! max # files to merge
      integer mx_wid			! max width of a table
	parameter (maxfls = 20, mx_wid=100)
      character*25 version
	parameter (version='RE_FITMRG Version 010595')

*    Local variables :
      INTEGER		ILSI		! input channel
      INTEGER		ifit, ofit	! in/out fits channels
      integer		nevf		! # event files to merge
      integer 		rw		! read/write
      integer		blksz		! blocksize
      integer           mrkeys		! Space for more keywords
      integer		hdutyp		! Header type
      integer		nrows		! # rows
      integer		tmpnrows	! temp # rows
      integer		tmin, tmax	! Min max event times
      integer		tmpmin, tmpmax	! Temp Min max event times
      integer		naxes(2)	! axes dimensions
      integer 		nkeys		! # keywords found
      integer		trow(mx_wid)	! temp row
      integer 		status, lstat
      integer		i, row
      integer 		fc, lc
      integer		det, tdet	! detector #
      integer    	rev, trev	! revision #
*
      CHARACTER*80	LIST		! List of event files to merge.
      CHARACTER*80	ONAME	        ! Name of output merged event file.
      CHARACTER*80	INAME(maxfls)   ! Name of input event file.
      character*80      hisrec		! History record.
      character*80      c_dum		! Dummy variable.
*
      logical    	fpres		! Merged file already exists
      logical		delfil		! Delete file then
*
      data status/0/

      write(*,*)
      WRITE(*,*) version
      write(*,*)

      STATUS = 0
      CALL PAR_cmdl (' ', STATUS)
      CALL AST_INIT()
      CALL SSO_INIT()


* Open and read the ev_list file containing source positions etc.
      call par_put0c('LIST', 're_evmrg.list', status)
      CALL PAR_GETLC ('LIST of files to merge', LIST, STATUS)
      CALL CHR_FANDL(LIST, FC, LC)


* Setup reading from terminal or file.
      IF (INDEX(LIST,'term') .GT. 0 ) THEN
        ILSI = 5
      ELSE
        CALL SYS_GETLUN(ILSI,lstat)
        OPEN (ILSI, FILE=LIST, STATUS='OLD', READONLY)
      ENDIF


      nevf = 1
 700  READ(ILSI, '(a)', END=799) iname(NEVF)
	NEVF = NEVF+1
	if (nevf .gt. maxfls) then
	  write(*,*) '   Error in RE_FITMRG'
	  write(*,*) '   Too many event files > ',maxfls
	  goto 999
	endif
	GOTO 700
 799  CONTINUE
      NEVF = NEVF-1
      close (ilsi)

* Name of merged event file.
      call par_put0c('OUT', 'mrgd_ev.fit', status)
      CALL PAR_GETLC ('OUT merged event file name', ONAME, STATUS)
      IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in RE_FITMRG after PAR_GETLC'
	 goto 999
      END IF

* Check this merged file does not already exist.
      CALL CHR_FANDL(oname, FC, LC)
      CALL MX_DEFEXT (oname(fc:lc), '.fit', oname)
      CALL CHR_FANDL(oname, FC, LC)
      fpres = .FALSE.
      INQUIRE (file=oname, EXIST=fpres)
      if (fpres) then
        write(*,*)
	write(*,*)'   Merged file ',oname(fc:lc),' already exists!'
        write(*,*)
        call par_put0l('DELFIL', .TRUE., status)
        call par_get0l('DELFIL delete this file then', delfil, status)
        if (delfil) then
          call deletefile(oname, status)
	  if (status .ne. 0) then
	    write(*,*)
	    write(*,*)'   Could not delete it.  Aborting'
	    goto 999
	  endif
	else
	  write(*,*)
	  write(*,*)'   Aborting RE_FITMRG.'
	  goto 999
	endif
      endif

*
      write(*,*)
      WRITE(*,*) '   Merging ',NEVF,' event files.'
      write(*,*)

* Copy first fit file to merged file.
      call ftgiou(ifit, status)
      call ftgiou(ofit, status)

* Input file
      rw = 0
      CALL CHR_FANDL(iname(1), FC, LC)
      CALL MX_DEFEXT (iname(1)(fc:lc), '.fit', iname(1))
      call ftopen(ifit, iname(1), rw, blksz, status)
      CALL CHR_FANDL(iname(1), FC, LC)
      blksz = 1
      call ftinit(ofit, oname, blksz, status)

* Primary header
      mrkeys = 2
      call ftcopy(ifit, ofit, mrkeys, status)
      call ftphis(ofit, 'Merged RE event file. '//version, status)
      call ftpkyj(ofit, 'NMAPS', nevf,
     &               'number of event maps merged', status)

* Bintable extension
      call ftcrhd(ofit, status)
      call ftmahd(ifit, 2, hdutyp, status)
      mrkeys = nevf
      call ftcopy(ifit, ofit, mrkeys, status)
      call ftgkyj(ifit, 'NAXIS2', nrows, c_dum, status)
      call ftgkyj(ifit, 'TDMIN1', tmin, c_dum, status)
      call ftgkyj(ifit, 'TDMAX1', tmax, c_dum, status)

* Get revision etc to check consistency.
      call ftgkyj(ifit, 'DETNAM', det, c_dum, status)
      call ftgkyj(ifit, 'REVISION', rev, c_dum, status)

      write(hisrec,'(a8,i7,a16,a)') 'Copied ',nrows,' rows from file ',
     &iname(1)(fc:lc)
      call ftphis(ofit, hisrec, status)
      call ftrdef(ofit, status)          ! Re-read keyword values.

* Close the input file.
      call ftclos(ifit, status)
      call ftfiou(ifit, status)
      if (status .ne. 0) call printerror(status)

* Merge in the rest of the files.
      do i=2,nevf
        rw = 0
        CALL CHR_FANDL(iname(i), FC, LC)
        CALL MX_DEFEXT (iname(i)(fc:lc), '.fit', iname(i))
        call ftopen(ifit, iname(i), rw, blksz, status)
        CALL CHR_FANDL(iname(i), FC, LC)

* Move to input bintable
        call ftmahd(ifit, 2, hdutyp, status)
	call ftgknj(ifit, 'NAXIS', 1, 2, naxes, nkeys, status)
        call ftgkyj(ifit, 'NAXIS2', tmpnrows, c_dum, status)
        call ftgkyj(ifit, 'TDMIN1', tmpmin, c_dum, status)
        call ftgkyj(ifit, 'TDMAX1', tmpmax, c_dum, status)
        call ftgkyj(ifit, 'DETNAM', tdet, c_dum, status)
        call ftgkyj(ifit, 'REVISION', trev, c_dum, status)

* Check merging consistent files.
	if (tdet .ne. det) then
	  write(*,*)
	  write(*,*)'   WARNING: Merging mixed detector types.'
	  write(*,*)'   File: ',iname(i)(fc:lc)
	  write(*,*)
	endif
	if (trev .ne. rev) then
	  write(*,*)
	  write(*,*)'   WARNING: Merging mixed file revisions.'
	  write(*,*)'   File: ',iname(i)(fc:lc)
	  write(*,*)
	endif

* Append rows.
        do row=1,tmpnrows
          call ftgtbb(ifit, row, 1, naxes(1), trow, status)
	  call ftptbb(ofit, nrows+row, 1, naxes(1), trow, status)
	enddo

* Update keywords
	nrows = tmpnrows + nrows
	tmin = min(tmin,tmpmin)
	tmax = max(tmax,tmpmax)
        call ftmkyj(ofit, 'NAXIS2', nrows, '&', status)
        call ftmkyj(ofit, 'TDMIN1', tmin, '&', status)
        call ftmkyj(ofit, 'TDMAX1', tmax, '&', status)
        write(hisrec,'(a9,i7,a16,a)') 'Appended ',tmpnrows,
     &' rows from file ',iname(i)(fc:lc)
        call ftphis(ofit, hisrec, status)
        call ftrdef(ofit, status)
* Close input file.
        call ftclos(ifit, status)
        call ftfiou(ifit, status)
        if (status .ne. 0) call printerror(status)
      enddo

* Remove SORT keyword from output file.
      call ftdkey(ofit, 'TSORTKEY', status)

* Close output file.
      call ftclos(ofit, status)
      call ftfiou(-1, status)
      if (status .ne. 0) call printerror(status)


999   IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in RE_FITMRG'
      END IF

      CALL SSO_CLOSE(STATUS)
      CALL AST_CLOSE

      END

