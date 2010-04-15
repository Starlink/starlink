      SUBROUTINE CAP_GSTAT (FILE, STATUS)
*+
*  Name:
*     CAP_GSTAT
*  Purpose:
*     Compute statistics for specified individual columns.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSTAT (FILE; STATUS)
*  Description:
*     Compute statistics for specified individual columns.  Each set
*     of statistics refers only to a single column; they are not
*     correlations between columns.  The statistics are displayed on the
*     screen and optionally written to a text file.
*  Arguments:
*     FILE  =  CHARACTER*(*) (Given)
*        Name of the text file to which statistics are to be written;
*        'NONE' if a text file is not required.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is an open catalogue then
*       If there is a list of columns for which statistics are to be
*       computed then
*         If an output file is required then
*           Attempt to get a free Fortran unit number.
*           Attempt to open the specified file.
*           If the file failed to open
*             Report an error.
*             Flush the error.
*             Annul the status.
*             Set the flag saying no output file is required.
*           end if
*         else
*           Set the flag saying no output file is required.
*         end if
*         Determine the width of each column.
*         Determine the number of columns which will fit on a line.
*         If there are more columns than will fit then
*           Report a warning.
*         end if
*         For every column
*           Determine the data type of the column.
*           If the column is numeric then
*             Attempt to map a work array to hold the column.
*           end if
*         end for
*         Map the work array required by the statistics routine.
*         Read in the required columns.
*         Compute the statistics.
*         Display the statistics.
*         Release the work space.
*         If there is an open output file then
*           Attempt to close the output file.
*         end if
*         If an output file has been written and all is ok then
*           Display a message.
*         end if
*       else
*         Report warning: no list of columns for which statistics are
*         required has been specified.
*       end if
*     else
*       Report message: catalogue not open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*  History:
*     28/11/96 (ACD): Original version.
*     4/12/96  (ACD): First stable version.
*     30/11/98 (ACD): Fixed the display format for displaying individual
*        statistics to work on Linux.
*     15/8/05 (TIMJ): OPEN should use FILE= not NAME=
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'      ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'      ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'      ! catview parametric constants.
      INCLUDE 'CNF_PAR'      ! CNF functions
*  Global Variables:
      INCLUDE 'SGZ_CMN'      ! catview common block.
*  Arguments Given:
      CHARACTER
     :  FILE*(*)
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER DSCRSZ         ! Size of description of statistics.
      PARAMETER (DSCRSZ = 20)

      INTEGER NSTAT          ! No. of statistics to compute.
      PARAMETER (NSTAT = 12)

      INTEGER IOOK           ! Fortran I/O success status.
      PARAMETER (IOOK = 0)
*  Local Variables:
      INTEGER
     :  CWIDTH,  ! Width of each field in the output table.
     :  MXCMP,   ! Maximum number of columns to fit on a line.
     :  SCMPS,   ! Number of columns to be computed.
     :  STYPI(SGZ__MXCMP),   ! Data types of the columns.
     :  SNNROW(SGZ__MXCMP),  ! Number of non-null rows in each column.
     :  SCLPTR(SGZ__MXCMP),  ! Pointers to arrays of values for each column.
     :  CURCMP,  ! Number of current column.
     :  SI,      ! Selection identifier.
     :  DTYPE,   ! Data type of the current column.
     :  LCTYPE,  ! Length of CTYPE (excl. trail. blanks).
     :  CSIZE    ! Size of CHARACTER data types.
      INTEGER
     :  CLPTR,   ! Pointer to array of values for the current column.
     :  ROWS,    ! Number of rows in the current selection.
     :  LENGTH,  ! Length of CHARACTER string (excl. trail. blanks).
     :  START,   ! Start position in BUFFER.
     :  STOP,    ! Stop     "     "    "   .
     :  LSTAT,   ! Local Fortran I/O status.
     :  CSTAT,   ! Current statistic.
     :  BUFPOS,  ! Current position in BUFFER.
     :  LCNAME,  ! Length of CNAME__SGZ (excl. trail. blanks).
     :  LCRIT    !   "    "  CRIT       ( "  .   "  .   "   ).
      INTEGER
     :  WRKPTR,  ! Pointer to statistics routine work array.
     :  FUNIT,   ! Fortran unit no. for output file; -1 if not required.
     :  LFORMT   ! Length of SFORMT (excl. trail. blanks).
      CHARACTER
     :  STYPC(SGZ__MXCMP)*(CAT__SZTYP), ! CHARACTER data types of columns.
     :  CTYPE*(CAT__SZTYP),    ! CHARACTER data types of current column.
     :  BUFFER*(SGZ__SZOLN),   ! Output buffer for the current row.
     :  TITLE1*(SGZ__SZOLN),   ! First  title for the listing.
     :  TITLE2*(SGZ__SZOLN),   ! Second   "    "   "     "   .
     :  CRIT*(CAT__SZEXP),     ! Selection expression.
     :  DSTAT(NSTAT)*(DSCRSZ), ! Description of statistics.
     :  CFIELD*30,             ! Current field.
     :  SFORMT*35              ! Display format for individual statistics.

*
*    Statistics for the current column.

      DOUBLE PRECISION
     :  MINVL,   ! Minimum value.
     :  MAXVL,   ! Maximum value.
     :  RANGE,   ! Total range.
     :  QUAR1,   ! First quartile.
     :  QUAR3,   ! Third quartile.
     :  IQRNG,   ! Interquartile range.
     :  MEDN,    ! Median (second quartile).
     :  MEAN,    ! Mean.
     :  MODE,    ! Mode.
     :  STDEV,   ! Standard deviation.
     :  SKEW,    ! Skewness.
     :  KURT     ! Kurtosis.

*
*    Null value flags for statistics on the current columns.

      LOGICAL
     :  MINVLN,  ! Minimum value.
     :  MAXVLN,  ! Maximum value.
     :  RANGEN,  ! Total range.
     :  QUAR1N,  ! First quartile.
     :  QUAR3N,  ! Third quartile.
     :  IQRNGN,  ! Interquartile range.
     :  MEDNN,   ! Median (second quartile).
     :  MEANN,   ! Mean.
     :  MODEN,   ! Mode.
     :  STDEVN,  ! Standard deviation.
     :  SKEWN,   ! Skewness.
     :  KURTN    ! Kurtosis.

*
*    Cells of values and null values for all the statistics for all the
*    columns.

      DOUBLE PRECISION
     :  SCELL(SGZ__MXCMP, NSTAT)    ! values.
      LOGICAL
     :  SCELNL(SGZ__MXCMP, NSTAT)   ! Null value flags.
*  Local Data:
      DATA DSTAT/'Minimum',     'Maximum',     'Total range',
     :    'First quartile',  'Third quartile', 'Interquartile range',
     :    'Median',             'Mean',        'Mode (approx.)',
     :    'Standard deviation', 'Skewness',    'Kurtosis'/
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check whether there is a list of columns for which statistics
*          are to be computed.

            IF (SCPS__SGZ .GT. 0) THEN

*
*             If an output file is required then attempt to get a
*             free Fortran unit number and to open the file.  If the
*             attempt fails then report an error, flush the error,
*             annul the status and set the flag saying no output
*             file is being written.  Alternatively, if no output
*             file is required then simply set the flag saying so.

               IF (FILE .NE. 'NONE'  .AND.  FILE .NE. ' ') THEN
                  CALL FIO_GUNIT (FUNIT, STATUS)

                  OPEN(UNIT=FUNIT, FILE=FILE, STATUS='NEW',
     :              FORM='FORMATTED', IOSTAT=LSTAT)
                  CALL FIO_SERR (LSTAT, STATUS)

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETC ('FILE', FILE)
                     CALL ERR_REP ('CAP_GSTAT_OPEN', 'Unable to '/
     :                 /'open file ^FILE.', STATUS)
                     CALL ERR_FLUSH (STATUS)
                     CALL ERR_ANNUL (STATUS)

                     FUNIT = -1
                  END IF

               ELSE
                  FUNIT = -1

               END IF

*
*             Determine the width of each column.  Note that eight
*             spaces are required for the sign, decimal point and
*             exponent in a double precision number, with one free
*             space left over for 'white space' between columns.

               CWIDTH = SDCPL__SGZ + 8

*
*             Determine the number of columns which will fit on a line
*             and report a message if there are more columns than will
*             fit.

               MXCMP = (SWID__SGZ - DSCRSZ) / CWIDTH

C              print3000, cwidth, dscrsz, swid__sgz, mxcmp, scps__sgz
C3000          format( '! cwidth, dscrsz, swid__sgz, mxcmp, ',
C    :           'scps__sgz' /
C    :           '! ',  i5, i5, i5, i5, i5)

               IF (MXCMP .LT. SCPS__SGZ) THEN
                  CALL MSG_SETI ('MXCMP', MXCMP)
                  CALL CAP_WARN (GUI__SGZ, ' ', 'There is sufficient '/
     :              /'space to list only the first ^MXCMP columns.',
     :              STATUS)

                  SCMPS = MXCMP

               ELSE
                  SCMPS = SCPS__SGZ

               END IF

*
*             Determine the number of rows in the current selection.

               SI = SELID__SGZ(CSEL__SGZ)

               CALL CAT_TROWS (SI, ROWS, STATUS)

*
*             Examine every column, determine its data type and if
*             it is numeric (that is, not CHARACTER or LOGICAL) then
*             then map a work array to hold the values for the column.
*             The character representation of the data type is
*             assembled for subsequent display.

               DO CURCMP = 1, SCMPS
                  CALL CAT_TIQAI (SCPID__SGZ(CURCMP), 'DTYPE', DTYPE,
     :              STATUS)

                  CALL CAT_TIQAI (SCPID__SGZ(CURCMP), 'CSIZE', CSIZE,
     :              STATUS)

                  CTYPE = ' '
                  LCTYPE = 0

                  CALL CAT_TYFMT (DTYPE, CSIZE, CTYPE, LCTYPE, STATUS)

                  STYPI(CURCMP) = DTYPE
                  STYPC(CURCMP) = CTYPE

                  IF (DTYPE .NE. CAT__TYPEC  .AND.
     :                DTYPE .NE. CAT__TYPEL) THEN
                     CALL CAP_CRTAR (ROWS, '_DOUBLE', CLPTR, STATUS)
                     SCLPTR(CURCMP) = CLPTR

                  ELSE
                     SCLPTR(CURCMP) = 0

                  END IF
               END DO

*
*             Map the work array required by the statistics routine.

               CALL CAP_CRTAR (ROWS, '_INTEGER', WRKPTR, STATUS)

*
*             Read in the required columns.

               CALL CAP_GSTRD (SI, ROWS, SCMPS, SCPID__SGZ, STYPI,
     :           SCLPTR, SNNROW, STATUS)

*
*             Compute the statistics.

               DO CURCMP = 1, SCMPS
                  IF (STYPI(CURCMP) .NE. CAT__TYPEC  .AND.
     :                STYPI(CURCMP) .NE. CAT__TYPEL) THEN
                     CALL CAP_GSTAR (SNNROW(CURCMP),
     :                 %VAL(CNF_PVAL(SCLPTR(CURCMP))),
     :                 %VAL(CNF_PVAL(WRKPTR)),
     :                 MINVL, MINVLN,   MAXVL, MAXVLN,   RANGE, RANGEN,
     :                 QUAR1, QUAR1N,   QUAR3, QUAR3N,   IQRNG, IQRNGN,
     :                 MEDN,  MEDNN,    MEAN, MEANN,     MODE, MODEN,
     :                 STDEV, STDEVN,   SKEW, SKEWN,     KURT,  KURTN,
     :                 STATUS)

                     SCELL(CURCMP, 1)  = MINVL
                     SCELL(CURCMP, 2)  = MAXVL
                     SCELL(CURCMP, 3)  = RANGE
                     SCELL(CURCMP, 4)  = QUAR1
                     SCELL(CURCMP, 5)  = QUAR3
                     SCELL(CURCMP, 6)  = IQRNG
                     SCELL(CURCMP, 7)  = MEDN
                     SCELL(CURCMP, 8)  = MEAN
                     SCELL(CURCMP, 9)  = MODE
                     SCELL(CURCMP, 10) = STDEV
                     SCELL(CURCMP, 11) = SKEW
                     SCELL(CURCMP, 12) = KURT

                     SCELNL(CURCMP, 1)  = MINVLN
                     SCELNL(CURCMP, 2)  = MAXVLN
                     SCELNL(CURCMP, 3)  = RANGEN
                     SCELNL(CURCMP, 4)  = QUAR1N
                     SCELNL(CURCMP, 5)  = QUAR3N
                     SCELNL(CURCMP, 6)  = IQRNGN
                     SCELNL(CURCMP, 7)  = MEDNN
                     SCELNL(CURCMP, 8)  = MEANN
                     SCELNL(CURCMP, 9)  = MODEN
                     SCELNL(CURCMP, 10)  = STDEVN
                     SCELNL(CURCMP, 11)  = SKEWN
                     SCELNL(CURCMP, 12) = KURTN

                  ELSE
                     SCELNL(CURCMP, 1)  = .TRUE.
                     SCELNL(CURCMP, 2)  = .TRUE.
                     SCELNL(CURCMP, 3)  = .TRUE.
                     SCELNL(CURCMP, 4)  = .TRUE.
                     SCELNL(CURCMP, 5)  = .TRUE.
                     SCELNL(CURCMP, 6)  = .TRUE.
                     SCELNL(CURCMP, 7)  = .TRUE.
                     SCELNL(CURCMP, 8)  = .TRUE.
                     SCELNL(CURCMP, 9)  = .TRUE.
                     SCELNL(CURCMP, 10) = .TRUE.
                     SCELNL(CURCMP, 11) = .TRUE.
                     SCELNL(CURCMP, 12) = .TRUE.

                  END IF
               END DO

*
*             Display the statistics.  First the header.

               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Catalogue ', BUFFER, BUFPOS)

               IF (CNAME__SGZ .NE. ' ') THEN
                  LCNAME = CHR_LEN(CNAME__SGZ)
                  CALL CHR_PUTC (CNAME__SGZ(1 : LCNAME), BUFFER, BUFPOS)
               ELSE
                  CALL CHR_PUTC ('<blank>', BUFFER, BUFPOS)
               END IF

               CALL CHR_PUTC (', selection ', BUFFER, BUFPOS)

               CALL CHR_PUTI (CSEL__SGZ, BUFFER, BUFPOS)
               CALL CHR_PUTC (': ', BUFFER, BUFPOS)

               CRIT = CRIT__SGZ(CSEL__SGZ)

               IF (CRIT .NE. ' ') THEN
                  LCRIT = CHR_LEN(CRIT)
                  CALL CHR_PUTC (CRIT(1 : LCRIT), BUFFER, BUFPOS)
               ELSE
                  CALL CHR_PUTC ('<none>', BUFFER, BUFPOS)
               END IF

               TITLE1 = BUFFER

               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('The number of rows in the selection = ',
     :           BUFFER, BUFPOS)
               CALL CHR_PUTI (ROWS, BUFFER, BUFPOS)

               TITLE2 = BUFFER

               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', TITLE1, STATUS)
               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', TITLE2, STATUS)
               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', ' ', STATUS)

*
*             The column name, data type and number of non-null rows.

               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Column', BUFFER, BUFPOS)

               STOP = DSCRSZ

               DO CURCMP = 1, SCMPS
                  STOP = STOP + CWIDTH

                  LENGTH = CHR_LEN(SCPNM__SGZ(CURCMP) )
                  START = STOP + 1 - LENGTH

                  BUFFER(START : STOP) = SCPNM__SGZ(CURCMP)(1 : LENGTH)
               END DO

               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', BUFFER(1 : STOP),
     :           STATUS)

               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Data type', BUFFER, BUFPOS)

               STOP = DSCRSZ

               DO CURCMP = 1, SCMPS
                  STOP = STOP + CWIDTH

                  LENGTH = CHR_LEN(STYPC(CURCMP) )
                  START = STOP + 1 - LENGTH

                  BUFFER(START : STOP) = STYPC(CURCMP)(1 : LENGTH)
               END DO

               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', BUFFER(1 : STOP),
     :           STATUS)

               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Non-null rows', BUFFER, BUFPOS)

               STOP = DSCRSZ

               DO CURCMP = 1, SCMPS
                  STOP = STOP + CWIDTH

                  CFIELD = ' '
                  LENGTH = 0

                  CALL CHR_PUTI (SNNROW(CURCMP), CFIELD, LENGTH)

                  START = STOP + 1 - LENGTH

                  BUFFER(START : STOP) = CFIELD(1 : LENGTH)
               END DO

               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', BUFFER(1 : STOP),
     :           STATUS)
               CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', ' ', STATUS)

*
*             Cells containing the values for the statistics.
*
*             Assemble the display format.

               SFORMT = ' '
               LFORMT = 0

               CALL CHR_PUTC ('(1PD', SFORMT, LFORMT)
               CALL CHR_PUTI (CWIDTH, SFORMT, LFORMT)
               CALL CHR_PUTC ('.', SFORMT, LFORMT)
               CALL CHR_PUTI (SDCPL__SGZ, SFORMT, LFORMT)
               CALL CHR_PUTC (')', SFORMT, LFORMT)

*
*             Output the cells.

               DO CSTAT = 1, NSTAT
                  BUFFER = ' '
                  BUFPOS = 0

                  LENGTH = CHR_LEN(DSTAT(CSTAT) )
                  CALL CHR_PUTC (DSTAT(CSTAT)(1 : LENGTH), BUFFER,
     :              BUFPOS)

                  STOP = DSCRSZ

                  DO CURCMP = 1, SCMPS
                     IF (.NOT. SCELNL(CURCMP, CSTAT) ) THEN
                        WRITE(CFIELD, SFORMT(1 : LFORMT),
     :                    IOSTAT=LSTAT) SCELL(CURCMP, CSTAT)
                        IF (LSTAT .NE. IOOK) THEN
                           CFIELD = ' '
                           LENGTH = 0
                           CALL CHR_PUTC ('<bad>', CFIELD, LENGTH)
                        END IF

                     ELSE
                        CFIELD = ' '
                        LENGTH = 0
                        CALL CHR_PUTC ('<null>', CFIELD, LENGTH)

                     END IF

                     LENGTH = CHR_LEN(CFIELD)
                     STOP = STOP + CWIDTH
                     START = STOP + 1 - LENGTH

                     BUFFER(START : STOP) = CFIELD(1 : LENGTH)
                  END DO

                  CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none',
     :              BUFFER(1 : STOP), STATUS)

*
*                Output a blank line to separate different types of
*                statistics.

                  IF (CSTAT .EQ. 3  .OR.  CSTAT .EQ. 6  .OR.
     :                CSTAT .EQ. 9) THEN
                     CALL CAP_OUTF (FUNIT, GUI__SGZ, 'none', ' ',
     :                 STATUS)
                  END IF
               END DO

*
*             Release the work space.

               DO CURCMP = 1, SCMPS
                  IF (SCLPTR(CURCMP) .NE. 0) THEN
                     CALL CAP_FREAR (SCLPTR(CURCMP), STATUS)
                  END IF
               END DO

*
*             If an output file has been written then attempt to close
*             it.

               IF (FUNIT .GT. -1) THEN
                  CLOSE(UNIT=FUNIT, IOSTAT=LSTAT)
                  CALL FIO_SERR (LSTAT, STATUS)

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETC ('FILE', FILE)
                     CALL ERR_REP ('CAP_GSTAT_CLSE', 'Error closing '/
     :                 /'file ^FILE.', STATUS)
                  END IF
               END IF

*
*             If an output file has been written and all is ok then
*             display a message.

               IF (FUNIT .GT. -1  .AND.  STATUS .EQ. SAI__OK) THEN
                  CALL MSG_SETC ('FILE', FILE)
                  CALL CAP_INFO (GUI__SGZ, ' ', 'Statistics written '/
     :              /'to file ^FILE.', STATUS)
               END IF


            ELSE

*
*             Report warning: no list of columns for which statistics are
*             required has been specified.

               CALL CAP_WARN (GUI__SGZ, ' ', 'No list of columns for '/
     :           /'which statistics are required is set.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
