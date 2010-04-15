      SUBROUTINE TRANJOIN( STATUS )
*+
*  Name:
*     TRANJOIN

*  Purpose:
*     Joins two transformations.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANJOIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This joins two transformations stored in the TRANSFORM (SUN/61)
*     format.  The concatenated transformation can be stored with either
*     original transformation or be placed in a new file.

*  Usage:
*     tranjoin in1 in2 out dest=?

*  ADAM Parameters:
*     DEST = LITERAL (Read)
*        The destination for the concatenated transformations.  This can
*        be one of the following:
*           "First"  -  Appends the second transformation in the first.
*                       The second transformation is unchanged.
*           "Second" -  Prefixes the first transformation in the second.
*                       The first transformation is unchanged.
*           "New"    -  Creates a new transformation structure using
*                       parameter OUT.  The input transformations are
*                       unchanged.
*        ["New"]
*     IN1 = TRN (Read and Write)
*        The first transformation structure to be concatenated.  It
*        prefixes the second supplied transformation.  This may be an
*        HDS container file, in which case the transformation structure
*        is assumed to be called TRANSFORM at the top level of the
*        file; or a path to the HDS object.  The suggested default is
*        the current value.
*     IN2 = TRN (Read and Write)
*        The second transformation structure to be concatenated.  It
*        appends to the first supplied transformation.  This may be an
*        HDS container file, in which case the transformation structure
*        is assumed to be called TRANSFORM at the top level of the
*        file; or a path to the HDS object.  The suggested default is
*        the current value.
*     OUT = TRN (Write)
*        The path to the new transformation structure created when
*        DEST="NEW" to hold the concatenated transformations.  If only
*        an HDS container filename is supplied, the transformation is
*        placed within a structure called TRANSFORM at the top-level of
*        the file.  So for instance, if OUT=warp9, the transformation
*        will be placed in the top-level structure TRANSFORM within
*        the file warp9.sdf.  In this case the container file may
*        already exist.  If, on the other hand, an explicit structure
*        is named, the transformation information will be placed there.
*        For example, to place the transformation in the extension
*        GALPHOT of the NDF called NGC253, OUT would be
*        NGC253.MORE.GALPHOT.  The structure name is limited to 15
*        printing characters.  Note that the structure must not already
*        exist.  If it does, an error condition results.

*  Examples:
*     tranjoin tr1 tr2 tr3
*        This prefixes the transformation in the HDS file called
*        tr1.sdf to that in file tr2.sdf, and stores the result in HDS
*        file tr3.sdf.  All three transformations are located within
*        objects called TRANSFORM at the top-level.
*     tranjoin offset shear.tr1 shape.rotate
*        This prefixes the transformation in the structure TRANSFORM at
*        the top level of the HDS container file called offset.sdf
*        (i.e.  OFFSET.TRANSFORM) to the transformation in the
*        structure TR1 in the HDS file shear.sdf.  The resulting
*        transformation is in the file called shape.sdf and is named
*        ROTATE.
*     tranjoin norm.scale1 polar dest=S
*        This prefixes the transformation structure NORM.SCALE1 to
*        POLAR.TRANSFORM, the concatenation being stored in
*        POLAR.TRANSFORM.
*     tranjoin norm.scale1 polar dest=f
*        This appends the transformation structure POLAR.TRANSFORM to
*        NORM.SCALE1, the concatenation being stored in NORM.SCALE1.

*  Notes:
*     -  The number of output variables of the first transformation must
*     equal the number of input variables of the second.  Also it is not
*     permitted to concatenate a transformation in which only the
*     forward mapping is defined with another in which only the inverse
*     mapping is specified.
*     -  On completion, the destination structure for the
*     transformation information equates to the current transformation
*     global parameter.

*  Related Applications:
*     KAPRH: TRANSFORMER, TRANINVERT, TRANMAKE, TRANTRACE;
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
      INCLUDE 'DAT_ERR'          ! Data-system error constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string ignoring
                                 ! trailing spaces

*  Local Variables:
      CHARACTER * ( 6 ) ACCTRN( 2 ) ! Access codes for input objects
      CHARACTER * ( 6 ) DEST     ! Destination for the concatenated
                                 ! transformation
      CHARACTER * ( 256 ) FILNAM ! HDS file name, output transformation
      CHARACTER * ( 256 ) FILNM1 ! HDS file name, first transformation
      CHARACTER * ( 256 ) FILNM2 ! HDS file name, second transformation
      CHARACTER * ( DAT__SZLOC ) ILOC1 ! Locator to the 1st input object
      CHARACTER * ( DAT__SZLOC ) ILOC2 ! Locator to the 2nd input object
      CHARACTER * ( DAT__SZLOC ) LOCTR1 ! Locator to the first
                                 ! transformation structure
      CHARACTER * ( DAT__SZLOC ) LOCTR2 ! Locator to the second
                                 ! transformation structure
      CHARACTER * ( DAT__SZLOC ) LOCTRN ! Locator to the output
                                 ! transformation
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of the component holding
                                 ! the transformation structure
      INTEGER NCHAR              ! Number of characters to last node
      INTEGER NCP                ! Number of characters in the path
      INTEGER NLEV               ! Number of levels in the output path
      INTEGER NLEV1              ! Number of levels in the first path
      INTEGER NLEV2              ! Number of levels in the second path
      CHARACTER * ( DAT__SZLOC ) PARLOC ! Locator to the parent
                                 ! structure of the transformation
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to the output object
      CHARACTER * ( 132 ) PATH   ! Path of output transformation
                                 ! structure
      CHARACTER * ( 132 ) PATH1  ! Path of 1st transformation structure
      CHARACTER * ( 132 ) PATH2  ! Path of 2nd transformation structure
      LOGICAL THERE              ! TRANSFORM object exists at top level

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the destination for the concatenated transformation.
*  ==============================================================
      CALL PAR_CHOIC( 'DEST', 'New', 'First,Second,New', .FALSE., DEST,
     :                STATUS )

*  Set the structure accesses for the input transformations.
      IF ( DEST .EQ. 'NEW' ) THEN
         ACCTRN( 1 ) = 'READ'
         ACCTRN( 2 ) = 'READ'

      ELSE IF ( DEST .EQ. 'FIRST' ) THEN
         ACCTRN( 1 ) = 'UPDATE'
         ACCTRN( 2 ) = 'READ'

      ELSE IF ( DEST .EQ. 'SECOND' ) THEN
         ACCTRN( 1 ) = 'READ'
         ACCTRN( 2 ) = 'UPDATE'

      END IF

*  Access the first input transformation.
*  ======================================

*  Obtain a locator to the transformation structure.
      CALL DAT_ASSOC( 'IN1', ACCTRN( 1 ), ILOC1, STATUS )

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( ILOC1, NLEV1, PATH1, FILNM1, STATUS )

*  There is only one level.
      IF ( NLEV1 .EQ. 1 ) THEN

*  Check that there is a TRANSFORM structure.
         CALL DAT_THERE( ILOC1, 'TRANSFORM', THERE, STATUS )

*  Report an error if the TRANSFORM structure is absent.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NOTRANSFORM',
     :        'The container file $IN1 does not contain a TRANSFORM '/
     :        /'structure at the top level.', STATUS )
         ELSE

*  Obtain a locator to the TRANSFORM structure.
            CALL DAT_FIND( ILOC1, 'TRANSFORM', LOCTR1, STATUS )

         END IF

      ELSE

*  Just clone the locator as the full path as specified.
         CALL DAT_CLONE( ILOC1, LOCTR1, STATUS )
      END IF

*  Access the second input transformation.
*  ======================================

*  Obtain a locator to the transformation structure.
      CALL DAT_ASSOC( 'IN2', ACCTRN( 2 ), ILOC2, STATUS )

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( ILOC2, NLEV2, PATH2, FILNM2, STATUS )

*  There is only one level.
      IF ( NLEV2 .EQ. 1 ) THEN

*  Check that there is a TRANSFORM structure.
         CALL DAT_THERE( ILOC2, 'TRANSFORM', THERE, STATUS )

*  Report an error if the TRANSFORM structure is absent.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NOTRANSFORM',
     :        'The container file $IN2 does not contain a TRANSFORM '/
     :        /'structure at the top level.', STATUS )
         ELSE

*  Obtain a locator to the TRANSFORM structure.
            CALL DAT_FIND( ILOC2, 'TRANSFORM', LOCTR2, STATUS )

         END IF

      ELSE

*  Just clone the locator as the full path as specified.
         CALL DAT_CLONE( ILOC2, LOCTR2, STATUS )
      END IF

*  Get a locator to the output structure.
*  ======================================

*  Obtain the output file.
      IF ( DEST .EQ. 'NEW' ) THEN

*  Obtain a locator to the the output object.  DAT_ASSOC reprompts if
*  the file does not exist, but DAT_EXIST will exit.
         CALL DAT_EXIST( 'OUT', 'WRITE', OLOC, STATUS )

*  Create a new container file, if the nominated file does not exist.
*  Annul the error as this is an expected condition.
         IF ( STATUS .EQ. DAT__FILNF .OR. STATUS .EQ. PAR__ERROR ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL DAT_CREAT( 'OUT', 'TRN', 0, 0, STATUS )
            CALL DAT_ASSOC( 'OUT', 'WRITE', OLOC, STATUS )
         END IF

*  Obtain the transformation's name and a locator to its parent.
*  =============================================================

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
         CALL HDS_TRACE( OLOC, NLEV, PATH, FILNAM, STATUS )

*  Place the transformation at the top-level.
*  ------------------------------------------
         IF ( NLEV .EQ. 1 ) THEN

*  The locator to the structure to contain the transformation is the
*  top-level locator.  Clone the original locator to the parent locator
*  as it is the parent structure.  The name of the transformation
*  structure is TRANSFORM at the top-level of the file.
             CALL DAT_CLONE( OLOC, PARLOC, STATUS )
             NAME = 'TRANSFORM'

         ELSE

*  Place the transformation in the current object.
*  -----------------------------------------------
*
*  Get the locator to the parent object.
            CALL DAT_PAREN( OLOC, PARLOC, STATUS )

*  Look for the last delimiter, searching backwards through the path.
            NCP = CHR_LEN( PATH )
            NCHAR = NCP
            CALL CHR_FIND( PATH, '.', .FALSE., NCHAR )

*  Extract the name of the transformation structure.
            NAME = PATH( NCHAR+1 : NCP )

*  TRN_NEW will create a new structure, so the one just made by the
*  DAT_ASSOC/EXIST must be deleted first.
            CALL DAT_ERASE( PARLOC, NAME, STATUS )

         END IF

*  Perform the concatenation.
*  ==========================

*  Concatenate in the new structure.
         CALL TRN_JOIN( LOCTR1, LOCTR2, PARLOC, NAME, LOCTRN, STATUS )

*  As we do not need to use the concatenated transformation, the locator
*  can be annulled immediately.  Also annul the parent structure, and
*the output file or structure.
         IF ( STATUS .EQ. SAI__OK ) CALL DAT_ANNUL( LOCTRN, STATUS )
         CALL DAT_ANNUL( PARLOC, STATUS )
         CALL DAT_ANNUL( OLOC, STATUS )

*  The global value should only be changed for the output transformation
*  so cancel the input objects.
         CALL DAT_CANCL( 'IN1', STATUS )
         CALL DAT_CANCL( 'IN2', STATUS )

*  Append the second transformation into the first transformation.
      ELSE IF ( DEST .EQ. 'FIRST' ) THEN
         CALL TRN_APND( LOCTR1, LOCTR2, STATUS )

*  The global value should only be changed for the revised
*  transformation so annul it and cancel the other input object.
         CALL DAT_ANNUL( ILOC1, STATUS )
         CALL DAT_CANCL( 'IN2', STATUS )

*  Annul the input transformation.
         CALL DAT_ANNUL( LOCTR1, STATUS )

*  Prefix the first transformation into the second transformation.
      ELSE IF ( DEST .EQ. 'SECOND' ) THEN
         CALL TRN_PRFX( LOCTR1, LOCTR2, STATUS )

*  The global value should only be changed for the revised
*  transformation so annul it and cancel the other input object.
         CALL DAT_ANNUL( ILOC2, STATUS )
         CALL DAT_CANCL( 'IN1', STATUS )

*  Annul the input transformation.
         CALL DAT_ANNUL( LOCTR2, STATUS )

      END IF

*  Tidy up.
*  ========

*  Close the transformation system.
      CALL TRN_CLOSE( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANJOIN_ERR',
     :     'TRANJOIN: Unable to concatenate two transformations.',
     :     STATUS )
      END IF

      END
