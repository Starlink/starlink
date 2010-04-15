*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*        Unused I, J, ICH1, ICH2, LUN, READONLY, SYM_INDEX, LENGTH, ADDRESS
*        Change error message to "-- gen_encode --" (was gen_sprint)
*        Remove "unevaluable" error message - now in GEN_EVAL_AE
*     30 October 2002 (ajc):
*        Initialise ISTAT
*-----------------------------------------------------------------------


      SUBROUTINE GEN_ENCODE (STRING, OUT_LINE, LOUT, IERR)

*  Routine to print the value of a symbol onto an output character array

      IMPLICIT  NONE

*  Formal parameter(s):

      CHARACTER STRING*(*)          ! List of items to print
      CHARACTER OUT_LINE*(*)        ! Output character string
      INTEGER   LOUT                ! Given: max length; returned: actual length
      INTEGER*4 IERR                ! Error return

*     GENLIB unit numbers

      INCLUDE 'LOGICAL_UNITS.INC'

*     Local parameters

      LOGICAL*4 LVALUE
      INTEGER*4 IVALUE
      REAL*4    R4VALUE
      REAL*8    R8VALUE
      CHARACTER CVALUE*128
      EQUIVALENCE (LVALUE,IVALUE,R4VALUE,R8VALUE,CVALUE)

      INTEGER*4 ISTAT
      INTEGER*4 ILS
      INTEGER*4 L
      INTEGER*4 IST, IEND, INEXT
      INTEGER*4 LS
      INTEGER*4 LFORM
      INTEGER*4 START
      INTEGER*4 ICOLON
      CHARACTER FORMAT*15

      CHARACTER TYPE*4
      INTEGER*4 NBYTES

*     Functions

      INTEGER*4 GEN_ILEN

*  Do it

      IERR = 0
      ISTAT = 0

*     Initialize the output line

      L = 1
      OUT_LINE(1:LOUT) = ' '

*     Is it bracketed? Skip over blanks and then look for (((...)))

      CALL STRIP_STRING (STRING, START, ILS)

*     Now iterate over elements of the list

      DO WHILE (.TRUE.)

        IF (START.GT.ILS) GO TO 100

        CALL GEN_GETIT3 (STRING(START:ILS), 1, IST, IEND, INEXT, IERR)
        IF (IERR.NE.0) THEN
          IERR = 0
          GO TO 100
        END IF

        IST   = START + IST   - 1
        IEND  = START + IEND  - 1
        START = START + INEXT - 1

CD      PRINT *,'Next item: ', STRING(IST:IEND)

*       Search for hollerith delimiters and throw out if they exist
*       -- implies this must be a string item though.

        IF (STRING(IST:IST).EQ.'''' .AND. STRING(IEND:IEND).EQ.'''')THEN
          CALL GEN_HDNORM (STRING(IST:IEND), STRING(IST:IEND), LS ,IERR)
          IEND = LS + IST - 1
CD        Print *, 'Bit of string we want', ist, ' to', iend
CD        Print *, 'Quoted string: ', string(ist:iend)

          OUT_LINE (L:L+IEND-IST) = STRING(IST:IEND)
          L = L + LS - 1

*       Otherwise it must be a symbol to translate. First search for
*       optional format argument:

        ELSE
          ICOLON = INDEX (STRING(IST:IEND), ':')
          IF (ICOLON.NE.0) THEN
            FORMAT = '(' // STRING(IST+ICOLON:IEND) // ')'
            LFORM  = IEND - (IST+ICOLON)
            IEND   = IST + ICOLON - 2
          ELSE
            FORMAT = ' '
            LFORM  = 0
          END IF

*         then evaluate the expression and print it with its own type

          TYPE = ' '
CD        PRINT *,' calling gen_eval_ae on string --> ', string(ist:iend)
          CALL GEN_EVAL_AE (STRING(IST:IEND), TYPE, LVALUE, IERR)
          IF (IERR .EQ. 0) THEN
            READ (TYPE(2:GEN_ILEN(TYPE)), '(I3)', IOSTAT=IERR) NBYTES
            IF (IERR.NE.0) THEN
              PRINT *, '-- gen_encode --'
              PRINT *, '   internal problems - report to RP'
              IERR = -1
              RETURN
            END IF
          ELSE
!            PRINT *, '-- gen_encode --'
!            PRINT *, '   expression "', STRING(IST:IEND),
!     &              '" not evaluable'
            RETURN
          END IF

          IF (TYPE(1:1).EQ.'L') THEN
            WRITE (OUT_LINE(L:LOUT), *, IOSTAT=ISTAT, ERR=999) LVALUE
          ELSE IF (TYPE(1:1).EQ.'I') THEN
            IF (LFORM.EQ.0) THEN
              WRITE (OUT_LINE(L:LOUT), *, IOSTAT=ISTAT, ERR=999) IVALUE
            ELSE
              WRITE (OUT_LINE(L:LOUT),FORMAT,IOSTAT=ISTAT,ERR=999)IVALUE
            END IF
          ELSE IF (TYPE(1:1).EQ.'R') THEN
            IF (NBYTES.EQ.4) THEN
              IF (LFORM.EQ.0) THEN
                WRITE (OUT_LINE(L:LOUT),*,IOSTAT=ISTAT,ERR=999) R4VALUE
              ELSE
                WRITE (OUT_LINE(L:LOUT), FORMAT, IOSTAT=ISTAT, ERR=999)
     &                 R4VALUE
              END IF
            ELSE IF (NBYTES.EQ.8) THEN
              IF (LFORM.EQ.0) THEN
                WRITE (OUT_LINE(L:LOUT),*,IOSTAT=ISTAT,ERR=999) R8VALUE
              ELSE
                WRITE (OUT_LINE(L:LOUT), FORMAT, IOSTAT=ISTAT, ERR=999)
     &                 R8VALUE
              END IF
            END IF
          ELSE IF (TYPE(1:1).EQ.'C') THEN

            IF (FORMAT.NE.' ') THEN
              WRITE (OUT_LINE(L:LOUT), FORMAT, IOSTAT=ISTAT, ERR=999)
     &               CVALUE(:NBYTES)
            ELSE
              OUT_LINE (L:MIN(L+NBYTES-1,LOUT)) = CVALUE(:NBYTES)
            END IF
          ELSE
            Print *,'Unknown symbol type: ', TYPE
          END IF

          IF (ISTAT.NE.0) THEN
            PRINT *, '-- gen_encode --'
            CALL GEN_ERMSG (ISTAT)
          END IF

          L = GEN_ILEN (OUT_LINE(1:LOUT))

        END IF
        L = L + 1
      END DO

  100 CONTINUE

      LOUT = GEN_ILEN (OUT_LINE(1:LOUT))
      RETURN

*  Error return

  999 CONTINUE
      WRITE (LUN_ERROR, *) 'Error writing output line'
      CALL GEN_ERMSG (ISTAT)

      RETURN

      END

*-----------------------------------------------------------------------
