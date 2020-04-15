      SUBROUTINE POLWCSCOPY( STATUS )
*+
*  Name:
*     POLWCSCOPY

*  Purpose:
*     Copies WCS from one catalogue to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLWCSCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a copy of the specified input catalogue.
*     If the input catalogue does not include any WCS information, then
*     the WCS information from a specified reference catalogue is
*     included in the output catalogue.
*
*     A typical use is to add WCS information back into a catalogue that
*     has been modified using STILTS or TOPCAT. For instance, if STILTS or
*     TOPCAT is used to create a new catalogue containing vectors selected
*     from an input POLPACK catalogue, then the new catalogue created by
*     STILTS or TOPCAT will not contain WCS information in a form that can
*     be used by POLPACK, KAPPA or GAIA. This application can then be used
*     to add WCS information back into the catalogue created by STILTS or
*     TOPCAT, copying the WCS from the original POLPACK catalogue.

*  Usage:
*     polwcscopy in out ref

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        The name of the input catalogue. A file type of .FIT is
*        assumed if none is provided.
*     OUT = LITERAL (Write)
*        The name of the output catalogue. A file type of .FIT is
*        assumed if none is provided. This will be a copy of IN. If IN
*        contains no WCS, then the WCS in OUT will be copied from REF.
*     REF = LITERAL (Read)
*        The name of the input catalogue from which WCS is to be read. A
*        file type of .FIT is assumed if none is provided. An error is
*        reported if this catalogue does not contain any WCS information.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'MSG_PAR'          ! MSG constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER FIELDS( 5 )*250  ! Individual fields of catalogue specification
      CHARACTER ONAME*250        ! Path to output catalogue
      INTEGER CIIN               ! CAT identifier for input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER CIREF              ! CAT identifier for reference catalogue
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      LOGICAL VERB               ! Verose errors required?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CIIN, FIELDS, STATUS )

*  Open the output catalogue.
      CALL CTG_CREA1( 'OUT', CIOUT, ONAME, STATUS )

*  Attempt to read an AST FrameSet from the input catalogue.
      CALL POL1_GTCTW( CIIN, IWCS, STATUS )

*  If the input catalogue already has WCS, tell the user.
      IF( IWCS .NE. AST__NULL ) THEN
         CALL MSG_SETC( 'IC', FIELDS(5) )
         CALL MSG_OUT( ' ', 'The Input catalogue (^IC) already '//
     :                 'contains WCS.', STATUS )
         CALL MSG_SETC( 'OC', ONAME )
         CALL MSG_OUT( ' ', 'The WCS in the output catalogue (^OC) '//
     :                 'will be a copy of the WCS in the input '//
     :                 'catalogue.', STATUS )

*  Otherwise, open the reference catalogue.
      ELSE
         CALL CTG_ASSO1( 'REF', VERB, 'READ', CIREF, FIELDS, STATUS )

*  Attempt to read an AST FrameSet from the reference catalogue.
         CALL POL1_GTCTW( CIREF, IWCS, STATUS )

*  Copy the polpack version number from reference to output.
         CALL POL1_CPVRC( CIREF, CIOUT, STATUS )

*  Close the reference catalogue.
         CALL CAT_TRLSE( CIREF, STATUS )

*  Report an error if no WCS was found in the reference catalogue.
         IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'The supplied reference catalogue '//
     :                    '(^C) does not contain any WCS. ', STATUS )

* Tell the user what is happening.
         ELSE
            CALL MSG_SETC( 'RC', FIELDS(5) )
            CALL MSG_SETC( 'OC', ONAME )
            CALL MSG_OUTIF( MSG__VERB, ' ', 'The WCS in the output '//
     :                    'catalogue (^OC) will be a copy of the '//
     :                    'WCS in the reference catalogue (^RC).',
     :                    STATUS )

         END IF
      END IF

* Copy the input catalogue to the output catalogue, excluding textual
* information.
      CALL POL1_CPCAT( CIIN, CIOUT, AST__NULL, .TRUE., STATUS )

*  Close the output catalogue, storing a copy of the WCS information
*  selected above.
      CALL POL1_CLCAT( IWCS, CIOUT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLWCSCOPY_ERR', 'POLWCSCOPY: Error copying '//
     :                 'WCS into a polarization catalogue.', STATUS )
      END IF

      END
