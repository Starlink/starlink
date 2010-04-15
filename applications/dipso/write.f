      SUBROUTINE WRITE( COMM, PARAMS, WORV, TITLE, STATUS )
*+
*  Name:
*     WRITE

*  Purpose:
*     Implements the DIPSO command WRITE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRITE( COMM, PARAMS, WORV, TITLE, STATUS )

*  Description:
*     The WRITE command writes the contents of the current arrays
*     to an NDF. The first command parameter is the name of the output
*     NDF, and is mandatory. The second command parameter is the name of
*     a model NDF, and is optional.
*
*     If no model NDF is supplied, a 1-dimensional output NDF is created
*     with lower bound set to 1 and upper bound set to the number of
*     values in the current arrays. The FLUX values are copied to the
*     DATA array, and the WAVE values are copied to the AXIS CENTRE
*     array for axis 1. A DIPSO_EXTRA extension is created holding the
*     break array and the WORV value. The supplied title is stored in
*     the NDF, the current X label is stored as the label for axis 1,
*     and the current Y label is stored as the NDF label.
*
*     If a model NDF is given then the output NDF is created by copying
*     the model NDF. The FLUX values stored in common are then written
*     into the DATA array. The position of each FLUX value in the DATA
*     array is determined by matching the wavelength value stored in
*     common with the NDFs AXIS CENTRE values (an error is reported if
*     there is no overlap or if the model NDF is not 1 dimensional).
*     None of the other attributes or components of the output NDF are
*     changed and so they retain the values copied from the model
*     NDF. No DIPSO_EXTRA extension is put into the output NDF (it may
*     already have one of course, copied from the model NDF).

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "WRITE", but could conceivably be something else
*        ("EXIT" for instance).
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     WORV = REAL (Given)
*        The "wavelength or velocity" value.
*     TITLE = CHARACTER * ( * ) (Given)
*        The title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        NPOINT = INTEGER (Read)
*           The number of data elements in the current arrays.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        NDFNM*255,         ! Full name of NDF structure.
     :        ROOT*255           ! Root name of NDF structure.

      INTEGER
     :        INDFM              ! Identifier for model NDF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the current arrays are empty, issue a warning and return.
      IF( NPOINT .EQ. 0 ) THEN
         CALL MSGOUT( COMM, 'Current arrays are empty - not written.',
     :                .TRUE., STATUS )
         GO TO 999
      END IF

*  Give a warning if three or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 2, STATUS )

*  Get the name of the output NDF.
      CALL GET0C( PARAMS, 1, .FALSE., COMM, 'Name of the NDF ' //
     :            'structure', ' ', ROOT, STATUS )

*  Tidy the name of the output NDF.
      CALL NDFNAM( ROOT, ' ', NDFNM, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get an identifier to the model NDF.
      CALL GETNDF( PARAMS, 2, .TRUE., COMM, 'Model NDF', '!', 'READ',
     :             ' ', INDFM, STATUS )

*  If a null value was obtained there is no model NDF.
      IF( STATUS .EQ. PAR__NULL ) THEN

*  Annull the error.
         CALL ERR_ANNUL( STATUS )

*  Tell the user what is about to happen.
         CALL MSG_SETC( 'NDFNAM', NDFNM )
         CALL MSGOUT( COMM, 'Writing current arrays to NDF '//
     :                '''^NDFNAM''.', .FALSE., STATUS )

*  Write the current array contents.
         CALL WRNDF( COMM, NDFNM, WORV, TITLE, STATUS )

*  If a model NDF was obtained...
      ELSE

*  Tell the user what is about to happen.
         CALL MSG_SETC( 'NDFNAM', NDFNM )
         CALL NDF_MSG( 'MODEL', INDFM )
         CALL MSGOUT( COMM, 'Writing current arrays to NDF '//
     :                '''^NDFNAM'' (modelled on ''^MODEL'').', .FALSE.,
     :                STATUS )

*  Write the current array contents into a copy of the model NDF.
         CALL WRPROP( NDFNM, INDFM, STATUS )

*  Annul the NDF identifier for the model NDF.
         CALL NDF_ANNUL( INDFM, STATUS )

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, re-report it with less information unlerss
*  the current message filtering level is verbose.
      CALL REREP( COMM, 'An error occurred while writing the '//
     :            'current arrays to an NDF.', STATUS )

      END
