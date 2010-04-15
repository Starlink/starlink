      SUBROUTINE
     :  CHU_CATREP(INPUT, HEADER, SCREEN, CNAMES, NUMCOL, ALL, STATUS)
*+
*  Name:
*     CHU_CATREP

*  Purpose:
*     Produce a catalogue report.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  CHU_CATREP(INPUT, HEADER, SCREEN, CNAMES, NUMCOL, ALL, STATUS)

*  Description :
*     Produce a catalogue report with or without a header and report it to
*     the screen or to a file.

*  Arguments:
*     INPUT = CHARACTER * ( chp__SZNAME ) (Given)
*        Name of the catalogue whose columns are to be reported.
*     HEADER = LOGICAL (Given)
*        Add a header to the output.
*     SCREEN = LOGICAL (Given)
*        Output to the screen or, false, toa file.
*     FNAMES( chp__NUMFLDS ) = CHARACTER * ( chp__SZFNAME ) (Given)
*        Name of the columns to be reported.
*     NUMFLD = INTEGER (Given)
*        Number of fields.
*     ALL = LOGICAL (Given)
*        All fields to be output.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     chp__CATNOTFND

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
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      LOGICAL HEADER
      LOGICAL SCREEN
      CHARACTER * ( * ) CNAMES( * )
      INTEGER NUMCOL
      LOGICAL ALL

*  Status:
      INTEGER STATUS             ! Global status

*  External references :
      INTEGER
     :  CHR_LEN,       ! Length of character string
     :  CHR_INDEX      ! Position of character in string

*  Local variables :
      LOGICAL MORE
      LOGICAL PAUSE
      CHARACTER
     :  FILENAME*(CHP__SZNAME),      ! Name of input catalogue
     :  NEWNAME*(20),      ! Name of input catalogue
     :  CATIN*(CHP__SZNAME),      ! Name of input catalogue
     :  TITLE*(132),             ! String containing title (was 1000)
     :  BUFFER*(132),            ! String containing list of values (was 1000)
     :  VALUE(132)*(132),        ! Character value of field (was ADC__MXELM * 1000
     :  NFRMT*(10),      ! Null format of field
     :  CURFMAT*(28)
       CHARACTER*(CHP__SZCNAME)  REQCNAMES(CHP__NUMCOLS)
       CHARACTER*(CHP__SZCNAME)  CATCNAMES(CHP__NUMCOLS)
      INTEGER
     :  POS,           ! Position in character string
     :  PAD,           ! Spacing for centring title
     :  NAMLEN,        ! Length of catalogue name
     :  CURCOL,        ! Current field number
     :  LENNAM,        ! Length of field name character string
     :  LENUNT,        ! Length of units character string
     :  LENVAL,        ! Length of value character string
     :  LNFRMT,        ! Length of null format string
     :  NUMREC,        ! Record number
     :  MARK,          ! Position of '!' in value character string
     :  WRSTAT,        ! Status for write
     :  FSTART,        ! Start value for field
     :  FLEN,          ! Length of field
     :  EXP,           ! Exponent of field
     :  FRMLEN         ! Format buffer length
       INTEGER
     :  NEWLEN,
     :  NAMELEN,
     :  CWIDTH(CHP__NUMCOLS),
     :  BUFLEN,
     :  SPACE,
     :  FD,
     :  CCOUNT,
     :  CATCCOUNT,
     :  CLOOP,
     :  LENCOL,
     :  LENNFT,
     :  ENDNFT,
     :  WIDTH
      INTEGER
     :  TABNUMCOL,
     :  TEMPNUMCOL,
     :  ROWCOUNT,
     :  TEMPROWCOUNT,
     :  TROWCOUNT,
     :  NFIELDS,
     :  CATNUMCOL,
     :  NUMROW,
     :  I
      REAL
     :  RVALUE         ! Real value of field
      LOGICAL
     :  MAXWD
      character*(chp__szcfmt) remfmats(chp__numcols)
      character*(chp__szcname) tabnames(chp__numcols)
      character*(chp__szcfmt) cformats(chp__numcols)
      character*(chp__szcfmt) printfmat
      logical nsfmatflag
      character*(chp__szcunit) cunits(chp__numcols)
      logical cnulls(chp__numcols)
      character*(chp__szccmt) ccomments(chp__numcols)
      character*(1) ctypes(chp__numcols)
      integer coldes(chp__numcols)
      integer intvals(chp__numcols)
      integer ptrvals(chp__numcols)
      real    realvals(chp__numcols)
      logical logvals(chp__numcols)
      double precision doubvals(chp__numcols)
      character*(chp__szcval) charvals(chp__numcols)
      character*(1) coltypes(chp__numcols)
      logical specflg
      logical nullvals(chp__numcols)
      LOGICAL CMDATAACC( chp__numcols )
      LOGICAL CDATAACC( chp__numcols )
      LOGICAL PREFDIS( chp__numcols )
      INTEGER ARRSHP( chp__numcols )
      INTEGER ARRDIM( chp__numcols,7 )
      LOGICAL ASSERT( chp__numcols )
      LOGICAL DOMCHK( chp__numcols )
      character*(chp__szcassexp) ASSEXP(chp__numcols)
      character*(chp__szcassexp) VCEXP(chp__numcols)
      INTEGER DATELM( chp__numcols )
      LOGICAL VCFLAG( chp__numcols )
      LOGICAL NSFLAG( chp__numcols )
      LOGICAL DELIND( chp__numcols )

*  Status:

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Initialse
*

      SPACE = 1
      IF (SCREEN) THEN
        BUFLEN = 80
      ELSE
        BUFLEN = 132
      ENDIF
*
*   Initialize formats
*
      DO I = 1, chp__NUMCOLS
        CFORMATS(I) = '          '
      ENDDO
*
*
      CALL CHP_GALLCD(INPUT, TABNUMCOL, TABNAMES, CFORMATS,
     : CUNITS, CCOMMENTS, PREFDIS, COLTYPES, COLDES, ARRSHP, ARRDIM,
     : ASSERT, ASSEXP, DOMCHK, CMDATAACC, CDATAACC,DATELM, VCFLAG,
     : VCEXP, DELIND, NSFLAG, STATUS)
*
*
      IF (STATUS .EQ. SAI__OK) THEN
*
*  If all the fields are to be reported.
*
        WIDTH = 0
        IF (ALL) THEN
          NUMCOL = 1
          DO CLOOP = 1 ,TABNUMCOL
              CALL CHU_NSPFMAT(CFORMATS(CLOOP), NSFMATFLAG, PRINTFMAT,
     :                         STATUS)
              IF (NSFMATFLAG) THEN
                CFORMATS(CLOOP) = PRINTFMAT
              ENDIF
              NFRMT = CFORMATS(CLOOP)
              REMFMATS(NUMCOL) = NFRMT
              LENNFT = CHR_LEN (NFRMT)
              NFRMT(1:LENNFT-1) = NFRMT (2:LENNFT)
              NFRMT(LENNFT:LENNFT) = ' '
              ENDNFT = CHR_INDEX (NFRMT, '.')
              IF (ENDNFT .EQ. 0) THEN
*           lennft = 2
                CALL CHR_CTOI (NFRMT(1:LENNFT-1), LENCOL, STATUS)
              ELSE
                CALL CHR_CTOI (NFRMT(1:ENDNFT-1), LENCOL, STATUS)
              END IF

             IF (HEADER) THEN

                LENNAM = CHR_LEN (TABNAMES(CLOOP))
                LENUNT = CHR_LEN (CUNITS(CLOOP))
                CWIDTH(CLOOP) = MAX(LENCOL,LENNAM,LENUNT)
                WIDTH = WIDTH + SPACE + CWIDTH(CLOOP)
             ELSE
                CWIDTH(CLOOP) = LENCOL
                WIDTH = WIDTH + SPACE + CWIDTH(CLOOP)
             END IF
*
*       Check if this field will fit on to the page
*
             IF (WIDTH .GT. BUFLEN) THEN
               CALL MSG_OUT ('MESSAGE 1','Excess fields', STATUS)
               MAXWD = .TRUE.
             ELSE
               REQCNAMES(NUMCOL) = TABNAMES(NUMCOL)
               NUMCOL = NUMCOL + 1
             END IF
          END DO
          NUMCOL = NUMCOL - 1
        ELSE

*  Loop through the columns to be reported an find the formats from the info
*  obout the columns in the table.
*
*              If field is not a character string change field
*              length to that given by format
*
        TEMPNUMCOL = 1
        DO CCOUNT = 1, NUMCOL
          DO CLOOP = 1 ,TABNUMCOL
            IF (CNAMES(CCOUNT) .EQ. TABNAMES(CLOOP)) THEN
              CALL CHU_NSPFMAT(CFORMATS(CLOOP), NSFMATFLAG, PRINTFMAT,
     :                         STATUS)
              IF (NSFMATFLAG) THEN
                CFORMATS(CLOOP) = PRINTFMAT
              ENDIF
              NFRMT = CFORMATS(CLOOP)
              REMFMATS(TEMPNUMCOL) = NFRMT
              LENNFT = CHR_LEN (NFRMT)
              NFRMT(1:LENNFT-1) = NFRMT (2:LENNFT)
              ENDNFT = CHR_INDEX (NFRMT, '.')
              IF (ENDNFT .EQ. 0) THEN
                CALL CHR_CTOI (NFRMT(1:LENNFT-1), LENCOL, STATUS)
              ELSE
                CALL CHR_CTOI (NFRMT(1:ENDNFT-1), LENCOL, STATUS)
              END IF

             IF (HEADER) THEN

              LENNAM = CHR_LEN (CNAMES(CCOUNT))
              CWIDTH(CCOUNT) = MAX(LENCOL,LENNAM,LENUNT)
              WIDTH = WIDTH + SPACE + CWIDTH(CCOUNT)
             ELSE
              CWIDTH(CCOUNT) = LENCOL
              WIDTH = WIDTH + SPACE + CWIDTH(CCOUNT)
             END IF
*
*       Check if this field will fit on to the page
*
             IF (WIDTH .GT. BUFLEN) THEN
               CALL MSG_OUT ('MESSAGE 1','Excess fields', STATUS)
               MAXWD = .TRUE.
             ELSE
               REQCNAMES(TEMPNUMCOL) = CNAMES(CCOUNT)
               TEMPNUMCOL = TEMPNUMCOL + 1
             END IF
            END IF
          END DO
        END DO
        NUMCOL = TEMPNUMCOL - 1
        END IF
*
*   Open the out file if required.
*
        IF (.NOT. SCREEN) THEN
         FILENAME = INPUT
         NAMELEN = CHR_LEN(FILENAME)
           CALL CHR_APPND ('.rep',FILENAME, NAMELEN)
           CALL FIO_ERASE(FILENAME, STATUS)
           IF (STATUS .NE. SAI__OK) THEN
             CALL ERR_ANNUL( STATUS )
           ENDIF
           CALL FIO_OPEN (FILENAME, 'WRITE', 'LIST', 0,
     :                  FD, STATUS)
       ENDIF
*
*       Output a header if required.
*
         IF (HEADER) THEN
           CATIN = INPUT
             IF (CATIN .NE. ' ') THEN
*              assemble the title
               TITLE = ' '
               POS   = 0
               CALL CHR_PUTC('List of named columns '/
     :              /'for catalogue ', TITLE, POS)
               NAMLEN = CHR_LEN (CATIN)
               CALL CHR_PUTC (CATIN(1:NAMLEN), TITLE,
     :            POS)
               CALL CHR_PUTC ('.', TITLE, POS)
*               centre title
               PAD                 = (BUFLEN - POS)/2
               TITLE(PAD+1:BUFLEN) = TITLE(1:POS)
               TITLE(1:PAD)        = ' '

*               output the title to file and/or environment
               CALL CHU_PTLST (TITLE(1:BUFLEN), SCREEN,
     :              FD, STATUS)
             END IF

*           output blank line between title and field names
             BUFFER = ' '
             CALL CHU_PTLST (BUFFER, SCREEN, FD, STATUS)

*           assemble list of field names, right hand justify
             POS = -1 * SPACE

             DO CURCOL = 1, NUMCOL
                LENNAM = CHR_LEN(REQCNAMES(CURCOL))
                POS = POS + CWIDTH(CURCOL) + SPACE - LENNAM
                CALL CHR_PUTC (REQCNAMES(CURCOL)(1:LENNAM), BUFFER, POS)
             END DO

*           output the list to file and/or environment
             CALL CHU_PTLST (BUFFER(1:POS), SCREEN, FD,
     :          STATUS)


         END IF

*
*   Get the number of rows in the table.
*
       CALL CHP_GNENTS(INPUT, NUMROW, STATUS)
*
*   Loop through the table getting the values and writing them to the report.
*
       MORE = .TRUE.
       PAUSE = SCREEN
       ROWCOUNT = 0
       TEMPROWCOUNT = 0
       DO WHILE (ROWCOUNT .LT. NUMROW)
         ROWCOUNT = ROWCOUNT + 1
         IF (MORE) THEN
         CALL CHP_GDNAC(INPUT, CATNUMCOL, CATCNAMES, COLTYPES, COLDES,
     :  CHARVALS,
     :  DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLVALS,
     :  STATUS)
*
*   Loop through the fields getting the values an putting them into the
*   output buffer using the formats we found earlier.
*
        BUFFER = ' '
        POS = -1 * SPACE
        DO CCOUNT = 1, NUMCOL

          DO CATCCOUNT = 1, CATNUMCOL
           IF (REQCNAMES(CCOUNT) .EQ. CATCNAMES(CATCCOUNT)) THEN
              CALL CHR_FILL(' ',VALUE(CCOUNT))
              CURFMAT = '('//REMFMATS(CCOUNT)//')'
              IF (COLTYPES(CATCCOUNT) .EQ. 'C') THEN
                WRITE (VALUE(CCOUNT), CURFMAT, IOSTAT=STATUS)
     : CHARVALS(CATCCOUNT)

             ELSEIF (COLTYPES(CATCCOUNT) .EQ. 'D') THEN
                WRITE (VALUE(CCOUNT), CURFMAT, IOSTAT=STATUS)
     : DOUBVALS(CATCCOUNT)
              ELSEIF (COLTYPES(CATCCOUNT) .EQ. 'I') THEN
                WRITE (VALUE(CCOUNT), CURFMAT, IOSTAT=STATUS)
     : INTVALS(CATCCOUNT)
              ELSEIF (COLTYPES(CATCCOUNT) .EQ. 'L') THEN
                WRITE (VALUE(CCOUNT), CURFMAT, IOSTAT=STATUS)
     :  LOGVALS(CATCCOUNT)
              ELSEIF (COLTYPES(CATCCOUNT) .EQ. 'R') THEN
                WRITE (VALUE(CCOUNT), CURFMAT, IOSTAT=STATUS)
     :  REALVALS(CATCCOUNT)
              ENDIF
*
              LENVAL = CHR_LEN (VALUE(CCOUNT))

*                   add value to output buffer
*
              POS = POS + CWIDTH(CCOUNT) + SPACE - LENVAL
              CALL CHR_PUTC (VALUE(CCOUNT)(1:LENVAL), BUFFER,
     :                  POS)
            ENDIF
          ENDDO
        ENDDO
*  output the buffer to file and/or environment

        CALL CHU_PTLST (BUFFER(1:POS), SCREEN, FD,
     :              STATUS)
           ELSE
             ROWCOUNT = NUMROW
           ENDIF
         TEMPROWCOUNT = TEMPROWCOUNT + 1
         IF (PAUSE .AND. (TEMPROWCOUNT .EQ. 20)) THEN
           TEMPROWCOUNT = 0
           CALL PAR_GET0L('MORE', MORE, STATUS)
           CALL PAR_CANCL('MORE', STATUS)
         ENDIF
       END DO
       IF (.NOT. SCREEN) THEN
         CALL FIO_CLOSE(FD, STATUS)
       ENDIF
      END IF
      END
