************************************************************************

      SUBROUTINE AGI_ISAMP ( PICID, LSAME, STATUS )

*+
*  Name :
*     AGI_ISAMP
*
*  Purpose :
*     Inquire if two pictures are the same
*
*  Invocation :
*     CALL AGI_ISAMP( PICID, LSAME, STATUS )
*
*  Description :
*     Inquire if a picture identifier references the same picture as
*     the current picture. The picture referenced by the given picture
*     identifier is compared with the current picture to see if they
*     point to the same picture in the database.
*
*  Arguments :
*     PICID = INTEGER (Given)
*        Picture identifier.
*     LSAME = LOGICAL (Returned)
*        True if the pictures are the same, otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     Check status on entry.
*     Get the workstation name and picture number of the current picture.
*     Get the workstation name and picture number of the given picture.
*     Compare the names and numbers.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     30-JAN-1992 (NE):
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
      INTEGER PICNUM, TPICNM
*.

*   Initialise LSAME
      LSAME = .FALSE.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )

*   Get details of the given picture
            IF ( PICID .GT. 0 ) THEN
               TWKNAM = CAGIWK( PICID )
               TPICNM = CPICNM( PICID )

*   Test if the pictures are the same
               IF ( ( WKNAME .EQ. TWKNAM ) .AND.
     :              ( PICNUM .EQ. TPICNM ) ) THEN
                  LSAME = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END

