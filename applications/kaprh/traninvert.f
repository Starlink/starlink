      SUBROUTINE TRANINVERT( STATUS )
*+
*  Name:
*     TRANINVERT

*  Purpose:
*     Inverts a transformation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANINVERT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This inverts a transformation stored in the TRANSFORM (SUN/61)
*     format within an existing HDS file.

*  Usage:
*     traninvert transform

*  ADAM Parameters:
*     TRANSFORM = TRN (Read and Write)
*        The transformation structure to be inverted.  This may be an
*        HDS container file, in which case the transformation structure
*        is assumed to be called TRANSFORM at the top level of the
*        file; or a path to the HDS object.  The suggested default is
*        the current transformation structure.

*  Examples:
*     traninvert shear.transform
*        This inverts the transformation structure stored in object
*        TRANSFORM within the HDS file called shear.
*     traninvert shear
*        This does the same as above.
*     traninvert \
*        This inverts the current transformation structure.
*     traninvert m51.more.polar
*        This inverts the transformation structure called POLAR in
*        the extension of the NDF called m51.

*  Notes:
*     -  On completion, the destination structure for the
*     transformation information equates to the current transformation
*     global parameter.

*  Related Applications:
*     KAPRH: TRANSFORMER, TRANJOIN, TRANMAKE, TRANTRACE;
*     CCDPACK: CCDEDIT, TRANLIST, TRANNDF.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 March 9 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 256 ) FILNAM ! HDS file name
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the input object
      CHARACTER * ( DAT__SZLOC ) LOCTRN ! Locator to the transformation
      INTEGER NLEV               ! Number of levels in the path
      CHARACTER * ( 132 ) PATH   ! Path of the transformation structure
      LOGICAL THERE              ! TRANSFORM object exists at top level

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access and invert the transformation.
*  =====================================

*  Obtain a locator to the transformation structure.
      CALL DAT_ASSOC( 'TRANSFORM', 'UPDATE', LOC, STATUS )

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILNAM, STATUS )

*  There is only one level.
      IF ( NLEV .EQ. 1 ) THEN

*  Check that there is a TRANSFORM structure.
         CALL DAT_THERE( LOC, 'TRANSFORM', THERE, STATUS )

*  Report an error if the TRANSFORM structure is absent.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NOTRANSFORM',
     :        'The container file does not contain a TRANSFORM '/
     :        /'structure at the top level.', STATUS )
         ELSE

*  Obtain a locator to the TRANSFORM structure.
            CALL DAT_FIND( LOC, 'TRANSFORM', LOCTRN, STATUS )

*  Invert the transformation.
            CALL TRN_INV( LOCTRN, STATUS )

*  Annul the locator.
            CALL DAT_ANNUL( LOCTRN, STATUS )
         END IF

*  An object name was given.
      ELSE

*  Invert the transformation.
         CALL TRN_INV( LOC, STATUS )
      END IF

*  Tidy up.
*  ========

*  Close the transformation system.
      CALL TRN_CLOSE( STATUS )

*  Annul the locator to the file or structure.
      CALL DAT_ANNUL( LOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANINVERT_ERR',
     :     'TRANINVERT: Unable to invert a transformation in '/
     :     /'$TRANSFORM.', STATUS )
      END IF

      END
