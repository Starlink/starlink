      SUBROUTINE RAMK(X,Y,RA, STATUS )
*+
*   Marks RA against the ends of the Meridians
*   of equal RA.
*
*   Gets
*   ----
*      X,Y   - Co-ordinates of end of meridian where Ra is to be marked.
*      RA    - Value of RA for that meridian (in radians)

*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV calls
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     20-MAR-1993 (AJJB):
*       Changed the second argument, RA, to be double precision.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION TWOPI, HALFPI,RDSA,RDST,RDDG,DRA, RA
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

      CHARACTER*1 ISIGN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      XP = X + 5.0
      YP = Y
      IF (RA.LT.0.0) THEN
        DRA = RA + TWOPI
      ELSE
        DRA = RA
      ENDIF
      CALL CONV(2,DRA,0,ISIGN,IH,IM,IS,FS, STATUS )
      IF (Y.LT.0) THEN
         CALL SGS_STXJ ('TC')
      ELSE
         CALL SGS_STXJ ('BC')
      ENDIF

      CALL SGS_BTEXT (XP,YP)
      CALL SGS_ATXI  (IH,-2)
      CALL SGS_ATEXT (':')
      IF (IM.GE.10) THEN
         CALL SGS_ATXI (IM,-2)
      ELSE
         CALL SGS_ATEXT ('0')
         CALL SGS_ATXI (IM,-1)
      ENDIF
      CALL SGS_OTEXT

      END
