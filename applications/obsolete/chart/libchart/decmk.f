      SUBROUTINE DECMK(X,Y,DEC, STATUS )
*+
*   Marks on the Value of the Declination
*   Against the End of a Parallel of Dec
*
*   Gets
*   ----
*      X      - X Co-ord (at edge)
*      Y      - Y Co-ord (at edge)
*      DEC    - DEC of the Line Being Marked

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*-

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*1 ISIGN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      XP = X + 5.0
      YP = Y
      CALL TCONV (1,DEC,0,ISIGN,ID,IM,IS,FS, STATUS )
      CALL SGS_STXJ ('CR')
      CALL SGS_BTEXT (XP,YP)
      CALL SGS_ATEXT (ISIGN)
      IF (ID.GE.10) THEN
         CALL SGS_ATXI (ID,-2)
      ELSE
         CALL SGS_ATEXT ('0')
         CALL SGS_ATXI (ID,-1)
      ENDIF
      CALL SGS_ATEXT (':')
      IF (IM.GE.10) THEN
         CALL SGS_ATXI (IM,-2)
      ELSE
         CALL SGS_ATEXT ('0')
         CALL SGS_ATXI (IM,-1)
      ENDIF
      CALL SGS_OTEXT

      END
