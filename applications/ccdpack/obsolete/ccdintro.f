      SUBROUTINE CCDINTRO( STATUS )
*+
*  Name:
*     CCDINTRO

*  Purpose:
*     Introduces NDFs to the CCDPACK system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDINTRO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  [usage]
*  [ADAM_parameters]
*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_FAC'          ! IRG/IRH constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'PAR_ERR'          ! Parameter system error codes.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER NNDF( 5 )          ! Number of NDFs per input group
      INTEGER GID( 5 )           ! Input IRH group identifiers.
      LOGICAL SIMPLE             ! Flag showing whether simple
                                 ! NDF input has been performed
      LOGICAL MODIFY             ! Does the user want to modify any
                                 ! existing values
      LOGICAL ATIMES             ! Controls whether times are added.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the routine opening logging system.
      CALL CCD1_START( 'CCDINTRO', STATUS )

*  Set number of NDFs counters.
      DO 1 I = 1, 5
         NNDF( I ) = 0
         GID( I ) = IRH__NOID
 1    CONTINUE

*  Access a list of NDF names, or access a series of names for the
*  different data types.
      CALL PAR_GET0L( 'SIMPLE', SIMPLE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( SIMPLE ) THEN

*  All names supplied in one prompt.
         CALL CCD1_NDFGU( GID( 1 ), NNDF( 1 ), 'IN', 1, CCD1__MXINS,
     :                    STATUS )
      ELSE

*  Access the name types individually. Creating a separate group for
*  each set.

*  First get the bias frames.
         CALL CCD1_NDFGU( GID( 1 ), NNDF( 1 ), 'BIAS', 1, CCD1__MXINS,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ',
     :      '  No NDFs accessed using parameter BIAS', STATUS )
            NNDF( 1 ) = 0
         END IF

*  The target frames (objects/astronomically interesting data).
         CALL CCD1_NDFGU( GID( 2 ), NNDF( 2 ), 'TARGET', 1, CCD1__MXINS,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ',
     :      '  No NDFs accessed using parameter TARGET', STATUS )
            NNDF( 2 ) = 0
         END IF

*  Any dark counts frames (essential for IR data).
         CALL CCD1_NDFGU( GID( 3 ), NNDF( 3 ), 'DARK', 1, CCD1__MXINS,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ',
     :      '  No NDFs accessed using parameter TARGET', STATUS )
            NNDF( 3 ) = 0
         END IF

*  Any preflash frames (mostly not used any more).
         CALL CCD1_NDFGU( GID( 4 ), NNDF( 4 ), 'FLASH', 1, CCD1__MXINS,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ',
     :      '  No NDFs accessed using parameter FLASH', STATUS )
            NNDF( 4 ) = 0
         END IF

*  And flatfields (only allowing one type as no support for others at
*  present).
         CALL CCD1_NDFGU( GID( 5 ), NNDF( 5 ), 'FLAT', 1, CCD1__MXINS,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ',
     :      '  No NDFs accessed using parameter FLAT', STATUS )
            NNDF( 5 ) = 0
         END IF
      END IF

*  Does the user want global values to supercede those in place already?
      CALL PAR_GET0L( 'MODIFY', MODIFY, STATUS )

*  Do we want to add times to the NDFs? If these values are not added
*  then .TIMES.DARK and .TIMES.FLASH will be assumed 1.0.
      IF ( NNDF( 3 ) .GT. 0 .OR. NNDF( 4 ) .GT. 0 ) THEN

*  We have explicit DARKs or FLASHes - set default ADDTIMES to TRUE.
         CALL PAR_DEF0L( 'ADDTIMES', .TRUE., STATUS )
      ELSE

*  No explicit values set default to true  is SIMPLE is true (should
*  have these values according to the user).
         IF ( SIMPLE ) THEN
            CALL PAR_DEF0L( 'ADDTIMES', .TRUE., STATUS )
         ELSE

*  No explicit DARKs or FLASHes these times may not be needed.
            CALL PAR_DEF0L( 'ADDTIMES', .FALSE., STATUS )
         END IF
      END IF

*  Get the ADDTIMES value.
      CALL PAR_GET0L( 'ADDTIMES', ATIMES, STATUS )

*  Right have the input NDF groups, check each one in turn for all the
*  information to perform a reduction using these files.
      CALL CCD1_NDFCK( SIMPLE, MODIFY, NNDF, GID, ATIMES, STATUS )

*  Exit with error statement label. Perform orderly closedown.
 99   CONTINUE

*  Close IRG down.
      CALL IRH_CLOSE( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'CCDINTRO_ERR',
     :   'CCDINTRO: Error introducing CCD data frames.',
     :   STATUS )
      END IF

*  Close CCDPACK, write terminator.
      CALL CCD1_END( STATUS )

      END
* $Id$
