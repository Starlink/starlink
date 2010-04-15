
      SUBROUTINE TSP_PHSINITAG(ZONE,STATUS)
*+
*
*  T S P _ P H S I N I T A G
*
*  PHASEPLOT command - Intialize plotting
*
*  Initialize plotting using Autograph/SGS for the PHASEPLOT program.
*  Get the plot device, and open SGS and NCAR. Return the SGS zone
*  identifier.
*
*  Parameters
*
*  (<)  ZONE   (Integer)  SGS zone identifier for zone being used
*  (!)  STATUS (Integer)  Status value
*
*  Jeremy Bailey   28/2/1988
*
*  Modified:
*      11/12/1991
*+

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

*  Parameters
      INTEGER STATUS
      INTEGER ZONE

*  Local variables
      INTEGER I
      INTEGER IERR
      LOGICAL FIRST
      REAL WIND(4),VIEWP(4)
      REAL HEAP(3000)
      CHARACTER*40 LABEL

      DATA FIRST /.TRUE./

      IF (STATUS .EQ. SAI__OK) THEN

*  Get the Graphics device

         CALL SGS_ASSOC('DEVICE','WRITE',ZONE,STATUS)
         CALL GQNT(1,IERR,WIND,VIEWP)

*  Initialize Autograph

         IF (FIRST) THEN
             CALL SNX_AGSAV(HEAP)
             FIRST = .FALSE.
         ELSE
             CALL SNX_AGRES(HEAP)
         ENDIF

*  Set up for NCAR plot

         CALL AGSETP('GRAPH.',VIEWP,4)

      ENDIF
      END

