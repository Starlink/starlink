      SUBROUTINE POLRDTCL( STATUS )
*+
*  Name:
*     POLRDTCL

*  Purpose:
*     Reads a text file holding the contents of a specified catalogue in 
*     the form of a Tcl code frament and produces an output catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLRDTCL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads a description of a POLPACK catalogue supplied
*     in the form created by POLWRTCL, and creates an output catalogue. 
*     Other information (e.g. WCS etc) is copied from a second specified 
*     catalogue.

*  Usage:
*     polrdtcl in ref out 

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        The name of the input text file holding the Tcl code.
*     REF = LITERAL (Read)
*        The name of an existing catalogue from which extra information
*        should be copied. If none is supplied, no extra information is
*        stored in the output catalogue.
*     OUT = LITERAL (Read)
*        The name of the output catalogue.
*     TRANS = _LOGICAL (Read)
*        If TRUE, translate column names as specified by parameters I,
*        DI, Q, DQ, etc. Otherwise, these parameters are ignored. [FALSE]
*     I = LITERAL (Read)
*        The name of the column within REF holding the I data. The 
*        corresponding column in the output catalogue will be re-named as
*        "I". ["I"]
*     DI = LITERAL (Read)
*        The name of the column within REF holding the DI data. ["DI"]
*     Q = LITERAL (Read)
*        The name of the column within REF holding the Q data. ["Q"]
*     DQ = LITERAL (Read)
*        The name of the column within REF holding the DQ data. ["DQ"]
*     U = LITERAL (Read)
*        The name of the column within REF holding the U data. ["U"]
*     DU = LITERAL (Read)
*        The name of the column within REF holding the DU data. ["DU"]
*     V = LITERAL (Read)
*        The name of the column within REF holding the V data. ["V"]
*     DV = LITERAL (Read)
*        The name of the column within REF holding the DV data. ["DV"]

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-DEC-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants

*  Status:
      INTEGER STATUS

*  External References
      INTEGER CHR_LEN            ! Returns used length of a string

*  Local Variables:
      CHARACTER CLASS*50         ! Text class
      CHARACTER DI*20            ! Name of the column in REF holding DI values
      CHARACTER DQ*20            ! Name of the column in REF holding DQ values
      CHARACTER DU*20            ! Name of the column in REF holding DU values
      CHARACTER DV*20            ! Name of the column in REF holding DV values
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER FILE*255         ! Name of input text file
      CHARACTER I*20             ! Name of the column in REF holding I values
      CHARACTER Q*20             ! Name of the column in REF holding Q values
      CHARACTER TEXT*255         ! Text
      CHARACTER U*20             ! Name of the column in REF holding U values
      CHARACTER V*20             ! Name of the column in REF holding V values
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER CIREF              ! CAT identifier for reference catalogue
      INTEGER IWCS               ! Pointer to AST Object
      LOGICAL DONE               ! Finish looping?
      LOGICAL TRANS              ! Translate column names?
      LOGICAL VERB               ! Verose errors required?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the reference catalogue.
      CALL CTG_ASSO1( 'REF', VERB, 'READ', CIREF, FIELDS, STATUS )

*  See if column names are to be translated.
      CALL PAR_GET0L( 'TRANS', TRANS, STATUS )

*  If so, get the names of the columns holding the Stokes parameters and 
*  their errors.
      IF( TRANS ) THEN
         CALL PAR_GET0C( 'I', I, STATUS )
         CALL PAR_GET0C( 'Q', Q, STATUS )
         CALL PAR_GET0C( 'U', U, STATUS )
         CALL PAR_GET0C( 'V', V, STATUS )
         CALL PAR_GET0C( 'DI', DI, STATUS )
         CALL PAR_GET0C( 'DQ', DQ, STATUS )
         CALL PAR_GET0C( 'DU', DU, STATUS )
         CALL PAR_GET0C( 'DV', DV, STATUS )
      END IF

*  Create the output catalogue, propagating all meta data from the 
*  reference catalogue.
      CALL POL1_CTPRP( 'OUT', CIREF, TRANS, I, Q, U, V, DI, DQ, DU, DV, 
     :                 CIOUT, STATUS )

*  Get the name of the input text file.
      CALL PAR_GET0C( 'IN', FILE, STATUS )

*  Copy all data from the input text file tot he output catalogue.
      CALL POL1_RDTCL( FILE, CIREF, CIOUT, TRANS, I, Q, U, V, DI, DQ, 
     :                 DU, DV, STATUS )

*  Reset the pointer for the next item of textual information to be read
*  from the reference catalogue.
      CALL CAT_RSTXT( CIREF, STATUS )

*  Read Objects from the rference catalogue and write them to the output
*  catalogue.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK ) 
         CALL KPG1_RCATW( CIREF, IWCS, STATUS )
         IF( IWCS .NE. AST__NULL ) THEN 
            CALL KPG1_WCATW( IWCS, CIOUT, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
         ELSE
            DONE = .TRUE.
         END IF
      END DO

*  Copy other textual information from reference catalogue to output 
*  catalogue. 
      CALL CAT_GETXT( CIREF, DONE, CLASS, TEXT, STATUS )
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )
         CALL CAT_PUTXT( CIOUT, CLASS, TEXT( : MAX( 0, 
     :                   CHR_LEN( TEXT ) ) ), STATUS ) 
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL CAT_GETXT( CIREF, DONE, CLASS, TEXT, STATUS )
      END DO

*  Arrive here if an error occurs.
 999  CONTINUE

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Close the output catalogue.
      CALL CAT_TRLSE( CIOUT, STATUS )

*  Close the reference catalogue.
      CALL CAT_TRLSE( CIREF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  End the current error reporting environment.
      CALL ERR_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLRDTCL_ERR', 'POLRDTCL: Error producing a '//
     :                 'polarization catalogue from a Tcl description.',
     :                 STATUS )
      END IF

      END
