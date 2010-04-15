
      SUBROUTINE PERIOD_INPUT(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                        INFILEARRAY, YERRORARRAY, DETRENDARRAY)

C=============================================================================
C Routine to input data into the PERIOD program. The data must be read from
C an ASCII file. Input of Y axis errors is optional.
C
C Written by Vikram Singh Dhillon @Sussex 31-May-1991.
C
C Unused parameter MXVEC removed - GJP June 1995
C
C Removed upper case conversion on file names - GJP October 1995
C
C GJP March 1997
C
C Removed variable NVEC
C Added some variable initialisation
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C
C Brad Cavanagh, 13-APR-2006: Replaced non-standard TYPE parameter to OPEN
C  with STATUS.
C=============================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

      INCLUDE 'SAE_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_INPUT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXROW
      INTEGER HJDPTR
      INTEGER JUNK1PTR, JUNK2PTR
      INTEGER VELPTR, SIGVELPTR
      DOUBLE PRECISION JUNK(1), DUMMY
      INTEGER NUMCOLS, NUMROWS, IFAIL, XCOL, YCOL, YCOLERR
      INTEGER SLOT, FIRSTSLOT, LASTSLOT, IUNIT
      INTEGER KEYPTR, YSLOT1, STATUS
      CHARACTER*72 INFILEARRAY(MXSLOT), INFILE
      CHARACTER*1 BELL, REPLY, FLAG*6, STRING
      LOGICAL YERRORARRAY(MXSLOT), YERROR, DETRENDARRAY(MXSLOT)


C-----------------------------------------------------------------------------
C Initialise variables.
C-----------------------------------------------------------------------------

      NUMROWS=0
      NUMCOLS=0
      YERROR=.FALSE.
      STATUS=SAI__OK
      CALL FIO_GUNIT( IUNIT, STATUS )

C-----------------------------------------------------------------------------
C Select slots to load.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for input ' //
     :                     '(0,0 to quit) : '
      READ (*, *, ERR=100) FIRSTSLOT, LASTSLOT
      IF ( FIRSTSLOT.NE.0 .AND. LASTSLOT.NE.0 ) THEN
         IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Maximum slot number = ', MXSLOT
            GO TO 400
         END IF

         DO 300 SLOT = FIRSTSLOT, LASTSLOT

C-----------------------------------------------------------------------------
C Read in the data from a file using PERIOD_READFREE.
C-----------------------------------------------------------------------------

 110        CONTINUE
            WRITE (*, '(X,A,$)')
     :                       'Enter name of data file (<CR> to quit) : '
            READ (*, '(A)', ERR=110) INFILEARRAY(SLOT)
            IF ( INFILEARRAY(SLOT)(1:1).EQ.' ' ) GO TO 400
            INFILE = INFILEARRAY(SLOT)
            INFILEARRAY(SLOT) = 'Data File = ' // INFILEARRAY(SLOT)
            OPEN (UNIT=IUNIT, FILE=INFILE, STATUS='OLD',
     :            FORM='FORMATTED', ERR=110)

            MXROW = 1
            NUMROWS = -1

            CALL PERIOD_READFREE(JUNK, MXROW, NUMCOLS, NUMROWS,
     :                           IUNIT, IFAIL)

            IF ( IFAIL.EQ.1 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Something wrong with input file.'
               CLOSE (UNIT=IUNIT)
               GO TO 400
            END IF

            REWIND (UNIT=IUNIT)

            WRITE (*, *) ' '
            WRITE (*, *) '** OK: Number of columns found = ', NUMCOLS
            WRITE (*, *) '** OK: Number of rows found    = ', NUMROWS

            IF ( (NUMCOLS.GT.0) .AND. (NUMROWS.EQ.0) ) THEN
               WRITE (*, *) ' '
 115           CONTINUE
               WRITE (*, '(X,A,$)') '** OK: Is this an' //
     :                           ' ANTARES file output by RUBY ? [Y] : '
               READ (*, '(A)', ERR=115) REPLY
               CALL PERIOD_CASE(REPLY, .TRUE.)
               IF ( REPLY.EQ.'Y' .OR. REPLY.EQ.' ' ) THEN

                  REWIND (UNIT=IUNIT)
                  IFAIL = 0

                  DO WHILE ( IFAIL.EQ.0 )
                     READ (IUNIT, '(A)', IOSTAT=IFAIL) FLAG
                     IF ( FLAG.EQ.'0FRAME' ) THEN
                        NUMROWS = NUMROWS + 1
                        READ (IUNIT, *) DUMMY, DUMMY, DUMMY,
     :                                  DUMMY, DUMMY, DUMMY
                        READ (IUNIT, '(A)') STRING
                        READ (IUNIT, '(A)') STRING
                        READ (IUNIT, '(A)') STRING
                        READ (IUNIT, *) DUMMY, DUMMY, DUMMY,
     :                                  DUMMY, DUMMY, DUMMY,
     :                                  DUMMY, DUMMY, DUMMY, DUMMY
                        READ (IUNIT, *) DUMMY, DUMMY, DUMMY,
     :                                  DUMMY, DUMMY, DUMMY, DUMMY,
     :                                  DUMMY, DUMMY
                     END IF
                  END DO

                  IF ( NPTSARRAY(SLOT).NE.0 )
     :               CALL PERIOD_DEALL(YPTR(SLOT))
                  CALL PERIOD_ALLOC('_DOUBLE', NUMROWS*MXCOL,
     :                              YPTR(SLOT))
                  YSLOT1 = YPTR(SLOT)

                  CALL PERIOD_ALLOC('_DOUBLE', NUMROWS, HJDPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NUMROWS, VELPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NUMROWS, SIGVELPTR)

                  REWIND (UNIT=IUNIT)

                  CALL PERIOD_READANTARES(%VAL(CNF_PVAL(YSLOT1)),
     :                                    NUMROWS,
     :                                    MXCOL, %VAL(CNF_PVAL(HJDPTR)),
     :                                    %VAL(CNF_PVAL(VELPTR)),
     :                                    %VAL(CNF_PVAL(SIGVELPTR)),
     :                                    IUNIT)

                  CALL PERIOD_DEALL(SIGVELPTR)
                  CALL PERIOD_DEALL(VELPTR)
                  CALL PERIOD_DEALL(HJDPTR)

                  YERROR = .TRUE.
                  WRITE (*, *) ' '
                  WRITE (*, *) '** OK: Number of velocities found = ',
     :                         NUMROWS
                  GO TO 200
               ELSE
                  CLOSE (UNIT=IUNIT)
                  GO TO 400
               END IF
            ELSE IF ( NUMCOLS.EQ.1 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Only one column found.'
               CLOSE (UNIT=IUNIT)
               GO TO 400
            ELSE IF ( NUMCOLS.EQ.2 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** WARNING: Only two columns found.'
               WRITE (*, *)
     :                    '** WARNING: Will assume there are no errors.'
               WRITE (*, *) ' '
 120           CONTINUE
               WRITE (*, '(X,A,$)')
     :                     'Enter number of column containing X data : '
               READ (*, *, ERR=120) XCOL
               IF ( XCOL.GT.NUMCOLS ) GO TO 120
               IF ( XCOL.LE.0 ) GO TO 120
 125           CONTINUE
               WRITE (*, '(X,A,$)')
     :                     'Enter number of column containing Y data : '
               READ (*, *, ERR=125) YCOL
               IF ( YCOL.GT.NUMCOLS ) GO TO 125
               IF ( YCOL.LE.0 ) GO TO 125
               YERROR = .FALSE.
            ELSE IF ( NUMCOLS.EQ.3 ) THEN
               WRITE (*, *) ' '
               WRITE (*, *) '** OK: Three data columns have been found.'
               WRITE (*, *) '** OK: The default column assignment is: '
               WRITE (*, *) ' '
               WRITE (*, *) '** OK: COLUMN 1: X axis data.'
               WRITE (*, *) '** OK: COLUMN 2: Y axis data.'
               WRITE (*, *) '** OK: COLUMN 3: Y axis errors.'
               WRITE (*, *) ' '
 130           CONTINUE
               WRITE (*, '(X,A,$)')
     :                     'Does this agree with the data file ? [Y] : '
               READ (*, '(A)', ERR=130) REPLY
               CALL PERIOD_CASE(REPLY, .TRUE.)
               IF ( REPLY.EQ.'Y' .OR. REPLY.EQ.' ' ) THEN
                  XCOL = 1
                  YCOL = 2
                  YCOLERR = 3
                  YERROR = .TRUE.
               ELSE
                  WRITE (*, *) ' '
 140              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                    'Enter number of column containing X data  : '
                  READ (*, *, ERR=140) XCOL
                  IF ( XCOL.GT.NUMCOLS ) GO TO 140
                  IF ( XCOL.LE.0 ) GO TO 140
 145              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                    'Enter number of column containing Y data  : '
                  READ (*, *, ERR=145) YCOL
                  IF ( YCOL.GT.NUMCOLS ) GO TO 145
                  IF ( YCOL.LE.0 ) GO TO 145
 150              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                    'Are there errors on the Y axis data ? [Y] : '
                  READ (*, '(A)', ERR=150) REPLY
                  CALL PERIOD_CASE(REPLY, .TRUE.)
                  IF ( REPLY.EQ.'Y' .OR. REPLY.EQ.' ' ) THEN
 155                 CONTINUE
                     WRITE (*, '(X,A,$)')
     :                       'Enter number of column containing Y ' //
     :                      'data errors : '
                     READ (*, *, ERR=155) YCOLERR
                     IF ( YCOLERR.GT.NUMCOLS ) GO TO 155
                     IF ( YCOLERR.LE.0 ) GO TO 155
                     YERROR = .TRUE.
                  ELSE
                     YERROR = .FALSE.
                  END IF
               END IF
            ELSE IF ( NUMCOLS.GE.4 ) THEN
               WRITE (*, *) ' '
 160           CONTINUE
               WRITE (*, '(X,A,$)')
     :                    'Enter number of column containing X data  : '
               READ (*, *, ERR=160) XCOL
               IF ( XCOL.GT.NUMCOLS ) GO TO 160
               IF ( XCOL.LE.0 ) GO TO 160
 165           CONTINUE
               WRITE (*, '(X,A,$)')
     :                    'Enter number of column containing Y data  : '
               READ (*, *, ERR=165) YCOL
               IF ( YCOL.GT.NUMCOLS ) GO TO 165
               IF ( YCOL.LE.0 ) GO TO 165
 170           CONTINUE
               WRITE (*, '(X,A,$)')
     :                    'Are there errors on the Y axis data ? [Y] : '
               READ (*, '(A)', ERR=170) REPLY
               CALL PERIOD_CASE(REPLY, .TRUE.)
               IF ( REPLY.EQ.'Y' .OR. REPLY.EQ.' ' ) THEN
 175              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                            'Enter number of column containing Y '
     :                            // 'data errors : '
                  READ (*, *, ERR=175) YCOLERR
                  IF ( YCOLERR.GT.NUMCOLS ) GO TO 175
                  IF ( YCOLERR.LE.0 ) GO TO 175
                  YERROR = .TRUE.
               ELSE
                  YERROR = .FALSE.
               END IF
            END IF

C-----------------------------------------------------------------------------
C Now create a data array ready for input to PLT.
C-----------------------------------------------------------------------------

            IF ( NPTSARRAY(SLOT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOT))
            CALL PERIOD_ALLOC('_DOUBLE', NUMROWS*MXCOL, YPTR(SLOT))
            YSLOT1 = YPTR(SLOT)

            CALL PERIOD_ALLOC('_DOUBLE', NUMROWS*NUMCOLS, JUNK2PTR)
            CALL PERIOD_ALLOC('_DOUBLE', NUMROWS, JUNK1PTR)
            CALL PERIOD_ALLOC('_INTEGER', NUMROWS, KEYPTR)

            IFAIL = 0

            CALL PERIOD_INPUTSORTCYCLE(%VAL(CNF_PVAL(YSLOT1)),
     :                                 NUMROWS, MXCOL,
     :                                 %VAL(CNF_PVAL(JUNK2PTR)),
     :                                 NUMCOLS, XCOL,
     :                                 YCOL, YERROR, YCOLERR,
     :                                 %VAL(CNF_PVAL(JUNK1PTR)),
     :                                 %VAL(CNF_PVAL(KEYPTR)),
     :                                 IUNIT, IFAIL)

            CALL PERIOD_DEALL(KEYPTR)
            CALL PERIOD_DEALL(JUNK1PTR)
            CALL PERIOD_DEALL(JUNK2PTR)

            IF ( IFAIL.EQ.1 ) THEN
               CLOSE (UNIT=IUNIT)
               NPTSARRAY(SLOT) = 0
               GO TO 400
            END IF

            WRITE (*, *) ' '
 200        CONTINUE
            WRITE (*, *) '** OK: Filled Slot = ', SLOT
            WRITE (*, *) ' '

            YERRORARRAY(SLOT) = YERROR
            DETRENDARRAY(SLOT) = .FALSE.
            NPTSARRAY(SLOT) = NUMROWS

            NUMCOLS = 0
            NUMROWS = 0
            CLOSE (UNIT=IUNIT)

 300     CONTINUE

      END IF

 400  CONTINUE
      CALL FIO_PUNIT( IUNIT, STATUS )
      RETURN
      END
