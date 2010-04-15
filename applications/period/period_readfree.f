
      SUBROUTINE PERIOD_READFREE(DATA, MX, NX, NY, IUNIT, IFAIL)

C=========================================================================
C General purpose routine to read free format real data stored in the form
C of columns. It will ignore all blank lines and lines starting with an
C exclamation mark
C
C DATA(MX)    -- The data array. On initial invocation of READFREE,
C                MX should be set to 1; on the second invocation,
C                MX will equate to NX*NY - NX entries/line, NY lines.
C NX          -- the number of entries to be read/line.
C                Enter a value of zero and the routine will try to
C                determine the number of entries automatically. NX will
C                then be returned.
C                Can be changed on exit.
C NY          -- The number of lines to be read. Enter 0 to read all
C                the lines. Enter -1, on initial invocation of READFREE,
C                to perform a count of real data lines, with no assignment
C                of values to array DATA. On exit contains the number of
C                real data line read.
C IUNIT       -- Reads from IUNIT.
C IFAIL       -- 0 for normal exit, anything else an error has occurred.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C GJP June 1995
C
C Unused variable NEXT removed. Removed REAL and INTEGER*4 refs.
C
C Converted to Double Precision (KPD), August 2001
C Modified for dynamic array size calculations on first pass through
C  the data file  (KPD), August 2001
C=========================================================================

      INTEGER          NX,NY,IFAIL,MX
      DOUBLE PRECISION DATA(MX)
      INTEGER          LENGTH,NLINES,NBLOCK,IUNIT,I
      CHARACTER*100    STRING
      LOGICAL BLANK

      LENGTH = LEN(STRING)
      IFAIL = 0
      NLINES = 0

C-------------------------------------------------------------------------
C Find number of columns.
C-------------------------------------------------------------------------

      IF ( NX.LE.0 ) THEN
         IFAIL = 0
         NBLOCK = 0
         DO WHILE ( IFAIL.EQ.0 .AND. NBLOCK.EQ.0 )
            STRING(1:1) = '!'
            DO WHILE ( STRING(1:1).EQ.'!' )
               READ (IUNIT, '(A)', IOSTAT=IFAIL) STRING
            END DO

C-------------------------------------------------------------------------
C Search for number of contiguous blocks of data.
C-------------------------------------------------------------------------

            IF ( IFAIL.EQ.0 ) THEN
               NBLOCK = 0
               BLANK = .TRUE.
               DO 10 I = 1, LENGTH
                  IF ( BLANK .AND.
     :                 (STRING(I:I).NE.' '.AND.STRING(I:I).NE.CHAR(9)) )
     :                 THEN
                     NBLOCK = NBLOCK + 1
                     BLANK = .FALSE.
                  ELSE IF ( .NOT.BLANK .AND.
     :                      (STRING(I:I).EQ.' '.OR.STRING(I:I)
     :                      .EQ.CHAR(9)) ) THEN
                     BLANK = .TRUE.
                  END IF
 10            CONTINUE
            END IF
         END DO
         IF ( IFAIL.NE.0 ) RETURN

         NX = NBLOCK
         IF ( NY.GE.0 ) THEN
C-------------------------------------------------------------------------
C Translate first line.
C-------------------------------------------------------------------------
            READ (STRING, *, IOSTAT=IFAIL) (DATA(I), I=1, NX)
            IF ( IFAIL.NE.0 ) RETURN
            NLINES = 1
         END IF
      END IF
C-------------------------------------------------------------------------
C Read data.
C-------------------------------------------------------------------------
      IF ( NY.LT.0 ) THEN
         NLINES = 1
         DO WHILE ( IFAIL.EQ.0 )
            READ (IUNIT, '(A)', IOSTAT=IFAIL) STRING
            IF ( IFAIL.EQ.0 .AND. STRING(1:1).NE.'!'
     :                                  .AND. STRING.NE.' ' )
     :            NLINES = NLINES + 1
         END DO
      ELSE IF ( NY.EQ.0 ) THEN
         DO WHILE ( IFAIL.EQ.0 )
            READ (IUNIT, '(A)', IOSTAT=IFAIL) STRING
            IF ( IFAIL.EQ.0 .AND. STRING(1:1).NE.'!'
     :                             .AND. STRING.NE.' ' ) THEN
            NLINES = NLINES + 1
            READ (STRING, *, IOSTAT=IFAIL)
     :            (DATA(NX*(NLINES-1)+I), I=1, NX)
            END IF
         END DO
      ELSE
         DO WHILE ( IFAIL.EQ.0 .AND. NLINES.LT.NY )
            READ (IUNIT, '(A)', IOSTAT=IFAIL) STRING
            IF ( IFAIL.EQ.0 .AND. STRING(1:1).NE.'!'
     :                             .AND. STRING.NE.' ' ) THEN
            NLINES = NLINES + 1
            READ (STRING, *, IOSTAT=IFAIL)
     :            (DATA(NX*(NLINES-1)+I), I=1, NX)
            END IF
         END DO
      END IF


C-------------------------------------------------------------------------
C Error check.
C-------------------------------------------------------------------------

      IF ( IFAIL.GT.0 ) THEN
         NY = 0
         RETURN
      END IF
      IFAIL = 0
      NY = NLINES

      RETURN
      END
