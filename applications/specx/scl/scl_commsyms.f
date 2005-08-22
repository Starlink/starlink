*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*        Set common variable PRINT_OUTPUT false in a statement rather
*        than in the declaration.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     15 Jan 1994 (rp):
*        Change CHR_UCASE to UUCASE
*      6 Jun 2000 (ajc):
*        Replace 'type *' with 'PRINT *'
*        Unused AMBIGUOUS, NAME2
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_CRSYMBOL (NAME, VALUE, IERR)

*  Routine to define a new SPECX command symbol

      IMPLICIT NONE

*  Include files

      INCLUDE  'SYMBOLS'
      INCLUDE  'COMMAND_TABLE'

*  Formal parameters

      INTEGER*4 IERR
      CHARACTER NAME*(*)
      CHARACTER VALUE*(*)

*  Functions

      INTEGER*4 GEN_ILEN
      INTEGER*4 SCL_HASHINSERT
      INTEGER*4 SCL_HASHSEARCH
      LOGICAL*4 SCL_HASHDELETE

*  Local variables

      LOGICAL*4 POSN_FREE
      INTEGER*4 FREE
      INTEGER*4 HASH
      INTEGER*4 I
      INTEGER*4 INEW
      INTEGER*4 ISS
      INTEGER*4 LMATCH
      INTEGER*4 LNAME
      INTEGER*4 LVALUE
      INTEGER*4 MATCH
      INTEGER*4 SZENTRY
      CHARACTER NAME1*(LNAMES)

*  Make sure table initialized

      EXTERNAL  INIT_SYMBOLS

      IERR   = 0

      LNAME  = GEN_ILEN (NAME)
      LVALUE = GEN_ILEN (VALUE)

      SZENTRY   = MIN (LNAME, LNAMES)
      FREE      = 0
      POSN_FREE = .FALSE.

CD    PRINT *, NAME, ' := ', VALUE

*  Look for match with existing symbols:

      CALL UUCASE (NAME)
      NAME1 = NAME(1:MIN(LNAMES,LNAME))

      HASH = SCL_HASHSEARCH (NAME1, M_CTAB, TABLE, COMMS, MATCH)

*  No match with quoted name

      IF (HASH.LT.0) THEN

        I = 1
        DO WHILE (.NOT.POSN_FREE .AND. I.LE.MIN (SZTABLE,STMAX+1))
          IF (COMMS(NFUNC+I).EQ.' ') THEN
            POSN_FREE = .TRUE.
            FREE      = I
          END IF
          I = I + 1
        END DO

        IF (.NOT.POSN_FREE) THEN
          IERR = 2
          PRINT *,'Command table full!'

        ELSE    ! i.e. free space, and no match
          IF (LVALUE.GT.0) THEN

*          (Strip leading blanks off symbol translation)

            ISS = 1
            DO WHILE (VALUE(ISS:ISS).EQ.' ')
              ISS = ISS + 1
            END DO

            INEW          = NFUNC + FREE
            COMMS(INEW)   = NAME(1:SZENTRY)
            SYMBOLS(FREE) = VALUE(ISS:MIN(LSYMBOL,LVALUE))
*           PRINT *,'New symbol ',COMMS(INEW)(:SZENTRY), ' defined'
            STMAX = MAX (FREE, STMAX)
            HASH = SCL_HASHINSERT (COMMS(INEW), INEW, M_CTAB, TABLE)
            IERR = 0

          ELSE
            PRINT *,'Attempt to delete non-existent symbol!'
            IERR = 3
          END IF
        END IF

*  Else at least one match

      ELSE IF (MATCH.GT.0) THEN

*       Check that it is not a built-in command...
        IF (MATCH.GT.NFUNC) THEN

          LMATCH = GEN_ILEN (COMMS(MATCH))
          IF (LVALUE.EQ.0) THEN
            IF (SCL_HASHDELETE (COMMS(MATCH), M_CTAB,
     &                          TABLE, COMMS, HASH)) THEN
              PRINT *,'Symbol ',COMMS(MATCH)(:LMATCH),' deleted'
              COMMS(MATCH)         = ' '
              SYMBOLS(MATCH-NFUNC) = ' '
              IF (MATCH.EQ.NFUNC+STMAX) STMAX = STMAX-1
            ELSE
              PRINT *,'Failed to delete symbol from hash table'
            END IF
          ELSE
            SYMBOLS(MATCH-NFUNC) = VALUE(1:MIN(LSYMBOL,LVALUE))
            PRINT *,'Symbol ',COMMS(MATCH)(:LMATCH),' redefined'
          END IF

        ELSE
          PRINT *,'Attempt to delete/redefine built in command'
        END IF

      END IF

      IF (IERR.NE.0) IERR = 18

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SPECX_SHOWSYMB (NAME)

*  Routine to translate a SPECX symbol

      IMPLICIT NONE

*  Include files

      INCLUDE  'SYMBOLS'
      INCLUDE  'COMMAND_TABLE'

*  Formal parameters

      CHARACTER NAME*(*)

*   I/O control

       INCLUDE 'IODATA'

*  Functions

      INTEGER*4 GEN_ILEN
      INTEGER*4 GEN_KNTWRD

*  Local variables

      INTEGER*4 I, J
      INTEGER*4 IERR
      INTEGER*4 LNAME
      INTEGER*4 NMATCH
      INTEGER*4 NWORD1, NWORD2
      INTEGER*4 LSTR
      CHARACTER NAME1*(LNAMES)

*

      PRINT_OUTPUT = .FALSE.

*  Strip off leading and trailing blanks from given name.

      IF (GEN_ILEN(NAME).GT.0) THEN
        I = 1
        DO WHILE (NAME(I:I).EQ.' ')
          I = I + 1
        END DO
        NAME  = NAME(I:GEN_ILEN(NAME))//' '
        LNAME = GEN_ILEN (NAME)
        CALL UUCASE (NAME)
      ELSE
        LNAME = 0
      END IF

      LSTR  = MAX (1, MIN (LNAME, LNAMES))

*     PRINT *,'Searching for match for symbol ',NAME
*     PRINT *,'(total symbol length =',LNAME,') characters'

      NMATCH = 0

*     IF (LNAME.EQ.0) THEN
*       PRINT *,'Highest entry in symbol table =',STMAX
*     END IF

      IF (LNAME.EQ.0) THEN
        DO I = 1, STMAX
          IF (COMMS(NFUNC+I).NE.' ') THEN
            WRITE(ILOUT2, *) COMMS(NFUNC+I)(1:LNAMES)//' := '//
     &              SYMBOLS(I)(:GEN_ILEN(SYMBOLS(I)))
            NMATCH = NMATCH + 1
          END IF
        END DO

      ELSE
        NAME1 = NAME(1:MAX(1,MIN(LNAME,LNAMES)))
        CALL UUCASE (NAME1)
        DO J = 1,STMAX
          NWORD1 = GEN_KNTWRD (NAME1)
          NWORD2 = GEN_KNTWRD (COMMS(NFUNC+J))
          IF (NWORD1.LE.NWORD2) THEN
            CALL GEN_MATCH (NAME1, COMMS(NFUNC+J), NWORD1, IERR)
            IF (IERR.EQ.1) THEN
              WRITE (ILOUT2, *) COMMS(NFUNC+J)(1:LNAMES)//' := '//
     &                SYMBOLS(J)(:GEN_ILEN(SYMBOLS(J)))
              NMATCH = NMATCH + 1
            END IF
          END IF
        END DO
      END IF

      IF (NMATCH.EQ.0) THEN
        PRINT *,'Symbol not found'
      END IF

      RETURN
      END
