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
*     6-JAN-2005 (DSB):
*        Allow CLASS and ISA to specify particular subclasses of Frame.
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
      LOGICAL OK
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to access the parameter as an NDF.
      CALL NDF_EXIST( PARAM, 'READ', INDF, STATUS )

*  If succesful, get the WCS FrameSet from it.
      IF( INDF .NE. NDF__NOID ) THEN
         CALL KPG1_GTWCS( INDF, IAST, STATUS )

*  Tell the user where the object came from. 
         IF( IAST .NE. AST__NULL ) THEN
            CALL NDF_MSG( 'NDF', INDF ) 
            CALL ATL1_NOTIF( '   AST data read from NDF ''^NDF''.', 
     :                       STATUS )
         END IF

*  Annul the NDF identifer.
         CALL NDF_ANNUL( INDF, STATUS )

*  If it was not an NDF...
      ELSE

*  Obtain a GRP group containing text from which an Object is to be read.
         CALL ATL1_GTGRP( PARAM, IGRP, STATUS )

*  Tried to read an object from the group.
         CALL ATL1_RDGRP( IGRP, IAST, STATUS )

*  Delete the group.
         CALL GRP_DELET( IGRP, STATUS )

      END IF

*  Check the Object class if CLASS is not blank.
      IF( CLASS .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN 

*  See if the object is of the required class.
         OK = ISA( IAST, STATUS ) 

*  If not, and if the object is a FrameSet, see if the current Frame is
*  of the required class. If so return a pointer to the current Frame.
         IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN
            IF( .NOT. OK ) THEN
               IAST2 = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
               IF( ISA( IAST2, STATUS ) ) THEN
                  OK = .TRUE.
                  CALL AST_ANNUL( IAST, STATUS )
                  IAST = IAST2
               ELSE
                  CALL AST_ANNUL( IAST2, STATUS )
               END IF
            END IF

*  If not, see if the base to current Mapping is of the required class. If 
*  so return a pointer to the base to current Mapping.
            IF( .NOT. OK ) THEN
               IAST2 = AST_GETMAPPING( IAST, AST__BASE, AST__CURRENT, 
     :                                STATUS )
               IF( ISA( IAST2, STATUS ) ) THEN
                  OK = .TRUE.
                  CALL AST_ANNUL( IAST, STATUS )
                  IAST = IAST2
               ELSE
                  CALL AST_ANNUL( IAST2, STATUS )
               END IF
            END IF

         END IF

*  Report an error if we could not find an object of the correct class.
         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN
               IAST2 = AST_GETFRAME( IAST, AST__CURRENT, STATUS )
               CALL MSG_SETC( 'C', AST_GETC( IAST2, 'CLASS', STATUS ) )
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'L', CLASS )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ATL1_GTOBJ_ERR1', '$^P contains a '//
     :                       'FrameSet representing a ^C, but a '//
     :                       '^L is required.', STATUS )
               CALL AST_ANNUL( IAST2, STATUS )
            ELSE
               CALL MSG_SETC( 'C', AST_GETC( IAST, 'CLASS', STATUS ) )
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'L', CLASS )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ATL1_GTOBJ_ERR2', '$^P contains a ^C, '//
     :                       'but a ^L is required.', STATUS )
            END IF
         END IF

      END IF

*  Annul the object if an error occurred.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( IAST, STATUS )

      END
