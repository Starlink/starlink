************************************************************************

      SUBROUTINE AGI_ISAMD ( PICID, LSAME, STATUS )

*+
*  Name :
*     AGI_ISAMD
*
*  Purpose :
*     Inquire if pictures are on same device
*
*  Invocation :
*     CALL AGI_ISAMD( PICID, LSAME, STATUS )
*
*  Description :
*     Inquire if the given picture is on the same device as the current
*     picture.
*
*  Arguments :
*     PICID = INTEGER (Given)
*        Picture identifier
*     LSAME = LOGICAL (Returned)
*        True if pictures on same device, otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Check status on entry.
*     Get the workstation name of the current picture.
*     Get the workstation name of the given picture.
*     Compare the names.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     Aug 1988 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Arguments Returned :
      LOGICAL LSAME

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER * ( DAT__SZNAM ) TWKNAM, WKNAME
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Initialise LSAME
         LSAME = .FALSE.

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )

*   Get details of the given picture
            IF ( PICID .GT. 0 ) THEN
               TWKNAM = CAGIWK( PICID )

*   Test if the workstation names are the same
               IF ( WKNAME .EQ. TWKNAM ) THEN
                  LSAME = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END

