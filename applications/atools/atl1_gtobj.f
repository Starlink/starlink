      SUBROUTINE ATL1_GTOBJ( PARAM, CLASS, ISA, IAST, STATUS )
*+
*  Name:
*     ATL1_GTOBJ

*  Purpose:
*     Get an AST Object from an NDF, FITS file or text file using an 
*     environment parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_GTOBJ( PARAM, CLASS, ISA, IAST, STATUS )

*  Description:

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     CLASS = CHARACTER * ( * ) (Given)
*        The required class. Used in error reports (see ISA). If Objects
*        of more than 1 class can be used, this should be supplied blank, and 
*        the calling routine should verify that the Object is usable.
*     ISA = EXTERNAL (Given)
*        A suitable AST "ISA.." function which returns .TRUE. if an Object
*        is of a suitable class. This is ignored if CLASS is blank.
*        Otherwise, an error is reported if th supplied Object is not of the
*        required class.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER CLASS*(*)
      LOGICAL ISA

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ISA
      LOGICAL CHR_SIMLR

*  Local Variables:
      INTEGER IAST2
      INTEGER IGRP
      INTEGER INDF
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to access the parameter as an NDF.
      CALL NDF_EXIST( PARAM, 'READ', INDF, STATUS )

*  If succesful, get the WCS FrameSet from it, and annul the NDF identifier.
      IF( INDF .NE. NDF__NOID ) THEN
         CALL KPG1_GTWCS( INDF, IAST, STATUS )
         CALL NDF_ANNUL( INDF, STATUS )

*  If it was not an NDF...
      ELSE

*  Obtain a GRP group containing text from which an Object is to be read.
         CALL ATL1_GTGRP( PARAM, IGRP, STATUS )

*  Tried to read an object form the group.
         CALL ATL1_RDGRP( IGRP, IAST, STATUS )

*  Delete the group.
         CALL GRP_DELET( IGRP, STATUS )

      END IF

*  Check the Object class if CLASS is not blank.
      IF( CLASS .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN 
         IF( .NOT. ISA( IAST, STATUS ) ) THEN
            CALL MSG_SETC( 'C', AST_GETC( IAST, 'CLASS', STATUS ) )
            CALL MSG_SETC( 'P', PARAM )
            CALL MSG_SETC( 'L', CLASS )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ATL1_GTOBJ_ERR1', '$^P contains a ^C. '//
     :                 'A ^L is required.', STATUS )
         END IF

*  If a FrameSet is supplied, but a Frame or Mapping is required, extract
*  a Frame or Mapping from the FrameSet.
         IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN

            IF( CHR_SIMLR( CLASS, 'Frame' ) ) THEN
               IAST2 = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
               CALL AST_ANNUL( IAST, STATUS )
               IAST = IAST2

            ELSE IF( CHR_SIMLR( CLASS, 'Mapping' ) ) THEN
               IAST2 = AST_GETMAPPING( IAST, AST__BASE, AST__CURRENT, 
     :                                 STATUS )
               CALL AST_ANNUL( IAST, STATUS )
               IAST = IAST2
            END IF

         END IF

      END IF

*  Annul the object if an error occurred.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( IAST, STATUS )

      END
