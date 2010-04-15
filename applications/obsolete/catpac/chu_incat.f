      SUBROUTINE
     :  CHU_INCAT (INPUT, DATAFILE, NUMCOL, CNAMES, INCFORMATS,
     :  CUNITS, CCOMMENTS, STARTPOS, STATUS )
*+
*  Name:
*     CHU_INCAT

*  Purpose:
*      Create a new catalogue from data in an ASCII file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_UNCAT (INPUT, DATAFILE, NUMCOL, CNAMES, INCFORMATS,
*    :  CUNITS, CCOMMENTS, STATUS )

*  Description :
*     Create a catalogue and load data from an ASCII file into it.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be created.
*     DATAFILE = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the file containing the ASCII data.
*     NUMFLD = INTEGER (Returned)
*        Number of fields.
*     FNAMES( CHI__NUMFLDS ) = CHARACTER * ( CHI__SZFNAME ) (Given)
*        Names of the fields.
*     INFFORMATS( CHI__NUMFLDS ) = CHARACTER * ( CHI__SZFFMT ) (Given)
*        Formats of the fields being read.
*     FUNITS( CHI__NUMFLDS ) = CHARACTER * ( CHI__SZFUNIT ) (Given)
*        Units of the fields.
*     FCOMMENTS( CHI__NUMFLDS ) = CHARACTER * ( CHI__SZFCMT ) (Given)
*        Comments associated with the fields.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__IVLDFFMT

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHI errors
      INCLUDE 'FIO_ERR'   ! Standard FIO errors

*  Global Variables:

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) DATAFILE
      CHARACTER * ( * ) CNAMES( * )
      CHARACTER * ( * ) INCFORMATS( * )
      CHARACTER * ( * ) CUNITS( * )
      CHARACTER * ( * ) CCOMMENTS( * )
      INTEGER NUMCOL
      INTEGER STARTPOS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      logical chr_simlr

*  Local Variables:
      integer ptmins
      integer secs
      integer intvals(chp__numcols)
      real    realvals(chp__numcols)
      logical logvals(chp__numcols)
      double precision doubvals(chp__numcols)
      character*(chp__szcval) charvals(chp__numcols)
      character*(1) fieldtypes(chp__numcols)
      character*(14) newfmat(chp__numcols)
      character*(chp__szcfmt) standfmat(chp__numcols)
      character*(chp__szcfmt) format
      character*(chp__szcval) tempchar
      integer fpos(chp__numcols)
      character*(256) buff1
      character*(256) buff2
      character*(256) buff3
      character*(1) dig1
      character*(1) dig2
      character*(2) charlen
      integer numc1
      integer numc2
      integer numc3
      integer fd
      integer endpos(128)
      integer spos
      integer epos
      integer fcount
      integer lstatus
      integer idig1
      integer idig2
      integer length(chp__numcols)
      logical prefdis(chp__numcols)
      integer arrshp(chp__numcols)
      integer arrdim(chp__numcols,7)
      logical assert(chp__numcols)
      character * ( chp__szexp ) assexp
      logical domchk(chp__numcols)
      logical nullflags(chp__numcols)
      integer ptrvals(chp__numcols)
      integer coldes(chp__numcols)
      integer colcount

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Call the CHP_NOENT to create the table.
*
      do colcount = 1, numcol
        prefdis(colcount) = .TRUE.
        coldes(colcount) = 1
        arrshp(colcount) = 0
        assert(colcount) = .FALSE.
        domchk(colcount) = .FALSE.
      enddo

*
*   Sort out the formats for reading the data.
*
          do fcount = 1, numcol
*
           format = incformats(fcount)
           standfmat(fcount) = format
           call chr_ucase(format)
*
           if (format.eq.'HH MM SS.S' .or.
     :         format.eq.'HH:MM:SS.S') then
              newfmat(fcount) = '(A10)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 10
           elseif (format.eq.'HH MM SS' .or.
     :             format.eq.'HH:MM:SS') then
              newfmat(fcount) = '(A8)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 8
           elseif (format.eq.'HH MM' .or.
     :             format.eq.'HH:MM') then
              newfmat(fcount) = '(A10)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 10
           elseif (format.eq.'HHMMSS.S') then
              newfmat(fcount) = '(A8)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 8
              standfmat(fcount) = 'HH MM SS.S'
           elseif (format.eq.'HHMMSS') then
              newfmat(fcount) = '(A6)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 6
              standfmat(fcount) = 'HH MM SS'
           elseif (format.eq.'HHMM') then
              newfmat(fcount) = '(A4)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 4
              standfmat(fcount) = 'HH MM'
           elseif (format.eq.'HH MM.M' .or.
     :             format.eq.'HH:MM.M') then
              newfmat(fcount) = '(A7)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 7
              standfmat(fcount) = 'HH MM SS'
           elseif (format.eq.'HHMM.M') then
              newfmat(fcount) = '(A6)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 6
              standfmat(fcount) = 'HH MM SS'
           elseif (format.eq.'S DD MM SS.S' .or.
     :             format.eq.'S:DD:MM:SS.S') then
              newfmat(fcount) = '(A12)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 12
              standfmat(fcount) = 'SDD MM SS.S'
           elseif (format.eq.'SDD MM SS.S' .or.
     :             format.eq.'SDD:MM:SS.S') then
              newfmat(fcount) = '(A11)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 11
           elseif (format.eq.'SDD MM SS' .or.
     :             format.eq.'SDD:MM:SS') then
              newfmat(fcount) = '(A9)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 9
           elseif (format.eq.'SDD MM' .or.
     :             format.eq.'SDD:MM') then
              newfmat(fcount) = '(A6)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 6
           elseif (format.eq.'SDDMMSS.S') then
              newfmat(fcount) = '(A9)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 9
              standfmat(fcount) = 'SDD MM SS.S'
           elseif (format.eq.'SDDMMSS') then
              newfmat(fcount) = '(A7)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 7
              standfmat(fcount) = 'SDD MM SS'
           elseif (format.eq.'SDDMM.M') then
              newfmat(fcount) = '(A7)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 7
              standfmat(fcount) = 'SDD MM SS'
           elseif (format.eq.'SDDMM') then
              newfmat(fcount) = '(A5)'
              fieldtypes(fcount) = 'C'
              length(fcount) = 5
              standfmat(fcount) = 'SDD MM'
*
*
           elseif (format(1:6).eq.'DEGREE') then
              newfmat(fcount) = format(7:9)
              fieldtypes(fcount) = 'C'
              dig1 = format(7:7)
              read(dig1,'(I1)') length(fcount)
*
           elseif (format(1:1).eq.'A') then
*
*          CHAR
*
           dig1 = format(2:2)
           dig2 = format(3:3)
           if (dig1 .eq. '.' .or. dig2 .eq. ' ') then
             read(dig1,'(I1)') length(fcount)
           else
             read(dig1,'(I1)') idig1
             read(dig1,'(I1)') idig2
             length(fcount) = idig1*10+idig2
           endif
              newfmat(fcount) = '('//incformats(fcount)//')'
              fieldtypes(fcount) = 'C'
*
           elseif (format(1:1).eq.'D') then
*
*          DOUBLE
*
           dig1 = format(2:2)
           dig2 = format(3:3)
           if (dig1 .eq. '.' .or. dig2 .eq. ' ') then
             read(dig1,'(I1)') length(fcount)
           else
             read(dig1,'(I1)') idig1
             read(dig1,'(I1)') idig2
             length(fcount) = idig1*10+idig2
           endif
              newfmat(fcount) = '('//incformats(fcount)//')'
              fieldtypes(fcount) = 'D'
*
           elseif (format(1:1).eq.'F'.or.format(1:1).eq.'E') then
*
*          REAL
*
           dig1 = format(2:2)
           dig2 = format(3:3)
           if (dig1 .eq. '.' .or. dig2 .eq. ' ') then
             read(dig1,'(I1)') length(fcount)
           else
             read(dig1,'(I1)') idig1
             read(dig1,'(I1)') idig2
             length(fcount) = idig1*10+idig2
           endif
              newfmat(fcount) = '('//incformats(fcount)//')'
              fieldtypes(fcount) = 'R'
*
           elseif (format(1:1).eq.'I') then
*
*          INTEGER
*
           dig1 = format(2:2)
           dig2 = format(3:3)
           if (dig1 .eq. '.' .or. dig2 .eq. ' ') then
             read(dig1,'(I1)') length(fcount)
           else
             read(dig1,'(I1)') idig1
             read(dig1,'(I1)') idig2
             length(fcount) = idig1*10+idig2
           endif
              newfmat(fcount) = '('//incformats(fcount)//')'
              fieldtypes(fcount) = 'I'
*
           elseif (format(1:1).eq.'L') then
*
*          LOGICAL
*
           dig1 = format(2:2)
           dig2 = format(3:3)
           if (dig1 .eq. '.' .or. dig2 .eq. ' ') then
             read(dig1,'(I1)') length(fcount)
           else
             read(dig1,'(I1)') idig1
             read(dig1,'(I1)') idig2
             length(fcount) = idig1*10+idig2
           endif
              newfmat(fcount) = '('//incformats(fcount)//')'
              fieldtypes(fcount) = 'L'
*
           endif
          enddo
*
      call chp_crecat(input,100, numcol, cnames, standfmat,
     : cunits, ccomments, prefdis, coldes, arrshp, arrdim, assert,
     : assexp, domchk, status)
*

*   Loop reading in and interpreting the ASCII rows of data.
*
      call fio_open(datafile, 'READ', 'FORTRAN', 256, fd, status)
      do while (status .eq. SAI__OK)
        call fio_read(fd, buff1, numc1, status)
        if (status .eq. SAI__OK) then
*
          do fcount = 1, numcol
*
           if (newfmat(fcount)(2:2).eq.'A') then
*
*          CHAR
*
              spos = startpos(fcount)
              epos = spos + length(fcount) - 1
              charvals(fcount) = buff1(spos:epos)
              tempchar = charvals(fcount)
*
*  Check for sexagesimal formats that cannot be dealt with by CHP
*
*
           format = incformats(fcount)
           if (format.eq.'HHMMSS.S') then
             charvals(fcount)(1:2) = tempchar(1:2)
             charvals(fcount)(3:3) = ' '
             charvals(fcount)(4:5) = tempchar(3:4)
             charvals(fcount)(6:6) = ' '
             charvals(fcount)(7:8) = tempchar(5:6)
             charvals(fcount)(9:9) = '.'
             charvals(fcount)(10:10) = tempchar(8:8)
           elseif (format.eq.'HHMMSS') then
             charvals(fcount)(1:2) = tempchar(1:2)
             charvals(fcount)(3:3) = ' '
             charvals(fcount)(4:5) = tempchar(3:4)
             charvals(fcount)(6:6) = ' '
             charvals(fcount)(7:8) = tempchar(5:6)
           elseif (format.eq.'HHMM') then
             charvals(fcount)(1:2) = tempchar(1:2)
             charvals(fcount)(3:3) = ' '
             charvals(fcount)(4:5) = tempchar(3:4)
           elseif (format.eq.'HH MM.M' .or.
     :             format.eq.'HH:MM.M') then
             charvals(fcount)(1:2) = tempchar(1:2)
             charvals(fcount)(3:3) = ' '
             charvals(fcount)(4:5) = tempchar(4:5)
             charvals(fcount)(6:6) = ' '
             dig1 = tempchar(7:7)
             read(dig1,'(I1)') ptmins
             secs = ptmins * 6
             write(charvals(fcount)(7:8),'(I2)') secs
           elseif (format.eq.'HHMM.M') then
             charvals(fcount)(1:2) = tempchar(1:2)
             charvals(fcount)(3:3) = ' '
             charvals(fcount)(4:5) = tempchar(3:4)
             charvals(fcount)(6:6) = ' '
             dig1 = tempchar(6:6)
             read(dig1,'(I1)') ptmins
             secs = ptmins * 6
             write(charvals(fcount)(7:8),'(I2)') secs
           elseif (format.eq.'S DD MM SS.S' .or.
     :             format.eq.'S:DD:MM:SS.S') then
             charvals(fcount)(1:1) = tempchar(1:1)
             charvals(fcount)(2:3) = tempchar(3:4)
             charvals(fcount)(4:4) = ' '
             charvals(fcount)(5:6) = tempchar(6:7)
             charvals(fcount)(7:7) = ' '
           elseif (format.eq.'SDDMMSS.S') then
             charvals(fcount)(1:3) = tempchar(1:3)
             charvals(fcount)(4:4) = ' '
             charvals(fcount)(5:6) = tempchar(4:5)
             charvals(fcount)(7:7) = ' '
             charvals(fcount)(8:11) = tempchar(6:9)
           elseif (format.eq.'SDDMMSS') then
             charvals(fcount)(1:3) = tempchar(1:3)
             charvals(fcount)(4:4) = ' '
             charvals(fcount)(5:6) = tempchar(4:5)
             charvals(fcount)(7:7) = ' '
             charvals(fcount)(8:9) = tempchar(6:7)
           elseif (format.eq.'SDDMM.M') then
             charvals(fcount)(1:3) = tempchar(1:3)
             charvals(fcount)(4:4) = ' '
             charvals(fcount)(5:6) = tempchar(4:5)
             charvals(fcount)(7:7) = ' '
             dig1 = tempchar(7:7)
             read(dig1,'(I1)') ptmins
             secs = ptmins * 6
             write(charvals(fcount)(8:9),'(I2)') secs
           endif
*
*
           elseif (newfmat(fcount)(2:2).eq.'D') then
*
*          DOUBLE
*
              spos = startpos(fcount)
              epos = spos + length(fcount) - 1
              read(buff1(spos:epos),newfmat(fcount))doubvals(fcount)
*
           elseif (newfmat(fcount)(2:2).eq.'F'
     :             .or.newfmat(fcount)(2:2).eq.'E') then
*
*          REAL
*
              spos = startpos(fcount)
              epos = spos + length(fcount) - 1
              read(buff1(spos:epos),newfmat(fcount))realvals(fcount)
*
           elseif (newfmat(fcount)(2:2).eq.'I') then
*
*          INTEGER
*
              spos = startpos(fcount)
              epos = spos + length(fcount) - 1
              read(buff1(spos:epos),newfmat(fcount))intvals(fcount)
*
           elseif (newfmat(fcount)(2:2).eq.'L') then
*
*          LOGICAL
*
              spos = startpos(fcount)
              epos = spos + length(fcount) - 1
              read(buff1(spos:epos), newfmat(fcount))logvals(fcount)
*
           elseif (newfmat(fcount)(2:2).eq.'*') then
*
*          SEXAGESIMAL
*
              spos = startpos(fcount)
              epos = index(buff1(spos:),' ')
              length(fcount) = epos - spos
              write(charlen,'(i2)') length(fcount)
              newfmat(fcount) = '(A'//charlen//')'
              read(buff1(spos:epos),newfmat(fcount))charvals(fcount)
*
           endif
          enddo
*
*    Put the complete row into the table
*
          call chp_putent(input,1, numcol,cnames,fieldtypes, charvals,
     :  doubvals, intvals, logvals, realvals, ptrvals, nullflags,
     :  status)
        endif
      enddo
*
      if (status .eq. fio__eof) then
        call err_annul(status)
      endif

      call fio_close(fd, status)
      end
