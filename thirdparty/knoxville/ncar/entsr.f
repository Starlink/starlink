C PACKAGE ERPRT77        DESCRIPTION OF INDIVIDUAL USER ENTRIES
C                        FOLLOWS THIS PACKAGE DESCRIPTION.
C
C LATEST REVISION        FEBRUARY 1985
C
C PURPOSE                TO PROVIDE A PORTABLE, FORTRAN 77 ERROR
C                        HANDLING PACKAGE.
C
C USAGE                  THESE ROUTINES ARE INTENDED TO BE USED IN
C                        THE SAME MANNER AS THEIR SIMILARLY NAMED
C                        COUNTERPARTS ON THE PORT LIBRARY.  EXCEPT
C                        FOR ROUTINE SETER, THE CALLING SEQUENCES
C                        OF THESE ROUTINES ARE THE SAME AS FOR
C                        THEIR PORT COUNTERPARTS.
C                        ERPRT77 ENTRY          PORT ENTRY
C                        -------------          ----------
C                        ENTSR                  ENTSRC
C                        RETSR                  RETSRC
C                        NERRO                  NERROR
C                        ERROF                  ERROFF
C                        SETER                  SETERR
C                        EPRIN                  EPRINT
C                        FDUM                   FDUMP
C
C I/O                    SOME OF THE ROUTINES PRINT ERROR MESSAGES.
C
C PRECISION              NOT APPLICABLE
C
C REQUIRED LIBRARY       MACHCR, WHICH IS LOADED BY DEFAULT ON
C FILES                  NCAR'S CRAY MACHINES.
C
C LANGUAGE               FORTRAN 77
C
C HISTORY                DEVELOPED OCTOBER, 1984 AT NCAR IN BOULDER,
C                        COLORADO BY FRED CLARE OF THE SCIENTIFIC
C                        COMPUTING DIVISION BY ADAPTING THE NON-
C                        PROPRIETARY, ERROR HANDLING ROUTINES
C                        FROM THE PORT LIBRARY OF BELL LABS.
C
C PORTABILITY            FULLY PORTABLE
C
C REFERENCES             SEE THE MANUAL
C                          PORT MATHEMATICAL SUBROUTINE LIBRARY
C                        ESPECIALLY "ERROR HANDLING" IN SECTION 2
C                        OF THE INTRODUCTION, AND THE VARIOUS
C                        SUBROUTINE DESCRIPTIONS.
C ******************************************************************
C
C SUBBROUTINE ENTSR(IROLD,IRNEW)
C
C PURPOSE               SAVES THE CURRENT RECOVERY MODE STATUS AND
C                       SETS A NEW ONE.  IT ALSO CHECKS THE ERROR
C                       STATE, AND IF THERE IS AN ACTIVE ERROR
C                       STATE A MESSAGE IS PRINTED.
C
C USAGE                 CALL ENTSR(IROLD,IRNEW)
C
C ARGUMENTS
C
C ON INPUT               IRNEW
C                          VALUE SPECIFIED BY USER FOR ERROR
C                          RECOVERY
C                          = 0 LEAVES RECOVERY UNCHANGED
C                          = 1 GIVES  RECOVERY
C                          = 2 TURNS  RECOVERY OFF
C
C ON OUTPUT              IROLD
C                          RECEIVES THE CURRENT VALUE OF THE ERROR
C                          RECOVERY MODE
C
C SPECIAL CONDITIONS     IF THERE IS AN ACTIVE ERROR STATE, THE
C                        MESSAGE IS PRINTED AND EXECUTION STOPS.
C
C                        ERROR STATES -
C                        1 - ILLEGAL VALUE OF IRNEW.
C                        2 - CALLED WHILE IN AN ERROR STATE.
C ******************************************************************
C
C SUBROUTINE RETSR(IROLD)
C
C PURPOSE                SETS THE RECOVERY MODE TO THE STATUS GIVEN
C                        BY THE INPUT ARGUMENT.  A TEST IS THEN MADE
C                        TO SEE IF A CURRENT ERROR STATE EXISTS WHICH
C                        IS UNRECOVERABLE; IF SO, RETSR PRINTS AN
C                        ERROR MESSAGE AND TERMINATES THE RUN.
C
C                        BY CONVENTION, RETSR IS USED UPON EXIT
C                        FROM A SUBROUTINE TO RESTORE THE PREVIOUS
C                        RECOVERY MODE STATUS STORED BY ROUTINE
C                        ENTSR IN IROLD.
C
C USAGE                  CALL RETSR(IROLD)
C
C ARGUMENTS
C
C ON INPUT               IROLD
C                          = 1 SETS FOR RECOVERY
C                          = 2 SETS FOR NONRECOVERY
C
C ON OUTPUT              NONE
C
C SPECIAL CONDITIONS     IF THE CURRENT ERROR BECOMES UNRECOVERABLE,
C                        THE MESSAGE IS PRINTED AND EXECUTION STOPS.
C
C                        ERROR STATES -
C                          1 - ILLEGAL VALUE OF IROLD.
C ******************************************************************
C
C INTEGER FUNCTION NERRO(NERR)
C
C PURPOSE                PROVIDES THE CURRENT ERROR NUMBER (IF ANY)
C                        OR ZERO IF THE PROGRAM IS NOT IN THE
C                        ERROR STATE.
C
C USAGE                  N = NERRO(NERR)
C
C ARGUMENTS
C
C ON INPUT               NONE
C
C ON OUTPUT              NERR
C                          CURRENT VALUE OF THE ERROR NUMBER
C ******************************************************************
C SUBROUTINE ERROF
C
C PURPOSE                TURNS OFF THE ERROR STATE BY SETTING THE
C                        ERROR NUMBER TO ZERO
C
C USAGE                  CALL ERROF
C
C ARGUMENTS
C
C ON INPUT               NONE
C
C ON OUTPUT              NONE
C ******************************************************************
C
C SUBROUTINE SETER(MESSG,NERR,IOPT)
C
C PURPOSE                SETS THE ERROR INDICATOR AND, DEPENDING
C                        ON THE OPTIONS STATED BELOW, PRINTS A
C                        MESSAGE AND PROVIDES A DUMP.
C
C
C USAGE                  CALL SETER(MESSG,NERR,IOPT)
C
C ARGUMENTS
C
C ON INPUT               MESSG
C                          HOLLERITH STRING CONTAINING THE MESSAGE
C                          ASSOCIATED WITH THE ERROR
C
C                        NERR
C                          THE NUMBER TO ASSIGN TO THE ERROR
C
C                        IOPT
C                          = 1 FOR A RECOVERABLE ERROR
C                          = 2 FOR A FATAL ERROR
C
C                         IF IOPT = 1 AND THE USER IS IN ERROR
C                         RECOVERY MODE, SETERR SIMPLY REMEMBERS
C                         THE ERROR MESSAGE, SETS THE ERROR NUMBER
C                         TO NERR, AND RETURNS.
C
C                         IF IOPT = 1 AND THE USER IS NOT IN ERROR
C                         RECOVERY MODE, SETERR PRINTS THE ERROR
C                         MESSAGE AND TERMINATES THE RUN.
C
C                         IF IOPT = 2 SETERR ALWAYS PRINTS THE ERROR
C                         MESSAGE, CALLS FDUM, AND TERMINATES THE RUN.
C
C ON OUTPUT              NONE
C
C SPECIAL CONDITIONS     CANNOT ASSIGN NERR = 0, AND CANNOT SET IOPT
C                        TO ANY VALUE OTHER THAN 1 OR 2.
C ******************************************************************
C
C  SUBROUTINE EPRIN
C
C PURPOSE                PRINTS THE CURRENT ERROR MESSAGE IF THE
C                        PROGRAM IS IN THE ERROR STATE; OTHERWISE
C                        NOTHING IS PRINTED.
C
C USAGE                  CALL EPRIN
C
C ARGUMENTS
C
C ON INPUT               NONE
C
C ON OUTPUT              NONE
C ******************************************************************
C
C SUBROUTINE FDUM
C
C PURPOSE                TO PROVIDE A DUMMY ROUTINE WHICH SERVES
C                        AS A PLACEHOLDER FOR A SYMBOLIC DUMP
C                        ROUTINE, SHOULD IMPLEMENTORS DECIDE TO
C                        PROVIDE SUCH A ROUTINE.
C
C USAGE                  CALL EPRIN
C
C ARGUMENTS
C
C ON INPUT               NONE
C
C ON OUTPUT              NONE
C ******************************************************************
      SUBROUTINE ENTSR(IROLD,IRNEW)
C
      LOGICAL TEMP
      IF (IRNEW.LT.0 .OR. IRNEW.GT.2)
     1   CALL SETER(' ENTSR - ILLEGAL VALUE OF IRNEW',1,2)
C
      TEMP = IRNEW.NE.0
      IROLD = I8SAV(2,IRNEW,TEMP)
C
C  IF HAVE AN ERROR STATE, STOP EXECUTION.
C
      IF (I8SAV(1,0,.FALSE.) .NE. 0) CALL SETER
     1   (' ENTSR - CALLED WHILE IN AN ERROR STATE',2,2)
C
      RETURN
C
      END
