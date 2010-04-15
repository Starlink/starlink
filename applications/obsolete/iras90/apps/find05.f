      SUBROUTINE FIND05( PBANDS,MENU,STATUS )
*+
*  Name:
*     FIND05

*  Purpose:
*     Routine adds wavebands for all sources

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND05( PBANDS,MENU,STATUS)

*  Description:
*     Routine adds wavebands for all sources. It calls FIND22 to request
*     one set of wavebands requirements from the user. FIND22 also
*     translates these into a set of logical values indicating whether
*     each waveband is required. This routine copies those logicals
*     into the waveband required logicals for all sources.

*  Arguments:
*     PBANDS = CHARACTER * ( * ) (Given)
*        Description of PBANDS
*     MENU   = CHARACTER * ( * ) (Given)
*        Description of MENU
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  External Routines Used:
*     FINDCRDD:
*        FIND22
*     ERR:
*        ERR_ANNUL
*
*  Authors:
*     GHS: George Spalding (Rutherford Appleton Laboratory)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1991 (GHS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD
*  Arguments Given:
      CHARACTER * ( * ) PBANDS
      CHARACTER * ( 1 ) MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL WAFOUN             ! Flag indicating that some wavebands
                                 ! are required for this source
      INTEGER IK                 ! Source number
      LOGICAL SOWAB( 4 )         ! Temporary storage of waveband
                                 ! required logicals

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Call subroutine FIND22. This subroutine asks the user for the
*  wavebands he requires and translates the waveband identifiers to
*  give logical values to the four elements of SOWAB, so that SOWAB( 1)
*  tells whether the first waveband is required etc.
         CALL FIND22( PBANDS, .FALSE., SOWAB, WAFOUN, STATUS)

*  Check whether the return status from FIND22 was abort
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the return status from FIND22 was par__null
         IF ( STATUS .EQ. PAR__NULL ) THEN

*  If there was an error message, annul the error, which sets the
*  status to SAI__OK.
             CALL ERR_ANNUL( STATUS )

         ELSE
*  If the return status from FIND22 was O.K. then set the wavebands
*  required logicals for each source to the values of the SOWAB array
*  obtained in FIND22. NOFSO is the number of sources.
            DO 200 IK = 1, NOFSO
               SOWAB1( IK ) = SOWAB(1)
               SOWAB2( IK ) = SOWAB(2)
               SOWAB3( IK ) = SOWAB(3)
               SOWAB4( IK ) = SOWAB(4)
 200        CONTINUE
         END IF

*  The menu choice is changed to M so that the user can select from the
*  add size and wavebands menu in the FIND26 routine which called this
*  subroutine
300      MENU = 'M'

      END
