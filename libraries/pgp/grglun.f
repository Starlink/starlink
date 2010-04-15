      SUBROUTINE GRGLUN(LUN)
*+
*   - - - - - - - -
*     G R G L U N     (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Gets an unused Fortran logical unit number
*
*   Outputs
*      LUN     i    Logical unit number
*
*   The logical unit number returned is guaranteed not to be to connected
*   to a file but there is no coordination with any other system logical
*   number allocation scheme. GRPCKG only uses logical unit numbers
*   for temporary file access and always closes the files before returning
*   control to the user.
*
*   If a free unit cannot be found, -1 is returned.
*
*   D L Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      INTEGER LUN

*  Maximum and minimum values for lun
      INTEGER MAXLUN, MINLUN
      PARAMETER (MAXLUN=48, MINLUN = 10)
      LOGICAL OPN
      INTEGER I

      INTEGER LUNS
*  Starting point for search
      DATA LUNS/10/
*  Save the current value so that we always start with the number that worked
*  last time
      SAVE LUNS

      DO 10 I = 1, MAXLUN-MINLUN+1
         INQUIRE( UNIT=LUNS, OPENED=OPN)
         IF (.NOT.OPN) THEN
            LUN = LUNS
            GO TO 100
         END IF

         LUNS = LUNS + 1
         IF (LUNS.GT.MAXLUN) LUNS = MINLUN
   10 CONTINUE

*  Can't find a free unit
      LUN = -1

  100 CONTINUE
      END
