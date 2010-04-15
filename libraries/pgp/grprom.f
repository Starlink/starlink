      SUBROUTINE GRPROM
*+
*     - - - - - - - -
*       G R P R O M     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Prompt user before clearing screen
*
*   If the program is running under control of a terminal display "Type
*   <RETURN> for next page: " and wait for the user to type <CR> before
*   proceeding.
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'


      INTEGER ISTAT
      LOGICAL TERM

*   See if the standard input is a terminal
      ISTAT = SAI__OK
      CALL PSX_ISATTY( 0, TERM, ISTAT)
      IF (ISTAT.EQ.SAI__OK .AND. TERM) THEN

*     It is, so prompt (any errors are simply ignored)
          PRINT *, 'Type <RETURN> for next page:'
          READ (*,'(A)',ERR=10, END=10)
   10     CONTINUE
      END IF
      END
