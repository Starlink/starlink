      SUBROUTINE KPG1_ASFIL( PARAM, FRM, NP, IPOUT, STATUS )
*+
*  Name:
*     KPG1_ASFIL

*  Purpose:
*     Read spatial positions from a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFIL( PARAM, FRM, NP, IPOUT, STATUS )

*  Description:
*     This routine obtains formatted positions from a text file specified by 
*     an environment parameter. The positions are assumed to represent
*     axis values in the supplied Frame. 
*
*     The file should contain 1 position per line. Each position is given by 
*     a set of strings delimited by comma, space or tab (the first gives the
*     value for axis 1, the second for axis 2, etc). The number of strings
*     per line should equal the number of axes in the supplied Frame. 
*
*     The file may contain blank lines, and comment lines commencing with 
*     "!" or "#".

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRM = INTEGER (Given)
*        A pointer to an AST Frame.
*     NP = INTEGER (Returned)
*        The number of positions read from the file.
*     IPOUT = INTEGER (Returned)
*        A pointer to an _DOUBLE array "COR( NP, * )" holding the obtained
*        co-ordinates. The second dimension of the array is equal to the
*        number of axes in the suppleid Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRM

*  Arguments Returned:
      INTEGER NP
      INTEGER IPOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*(GRP__SZNAM)! AST attribute name
      CHARACTER FNAME*(GRP__SZFNM)! File name
      INTEGER FD                 ! FIO file descriptor for input file
      INTEGER IGRP               ! GRP identifier for group holding file contents
      INTEGER NAX                ! No. of axes in supplied Frame
      INTEGER NCHAR              ! Length of text read from file
*.

*  Initialise.
      NP = 0
      IPOUT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain the input text file, and save its name.
      CALL FIO_ASSOC( PARAM, 'READ', 'LIST', 0, FD, STATUS )
      CALL FIO_FNAME( FD, FNAME, STATUS ) 

*  Create a GRP group to hold the file contents. 
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Loop round until an error is encountered (this will happen when the end of 
*  file is reached, if not before).
      DO WHILE ( STATUS .EQ. SAI__OK )

*  Read a record of the text file.
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  Remove leading spaces.
         CALL CHR_LDBLK( BUFFER )

*  Append the record to the end of the group if it is not a comment line.
         IF( BUFFER .NE. ' ' .AND. BUFFER( 1 : 1 ) .NE. '#' .AND.
     :                             BUFFER( 1 : 1 ) .NE. '!' ) THEN
            CALL GRP_PUT( IGRP, 1, BUFFER( : NCHAR ), 0, STATUS ) 

         END IF

      END DO

*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )

*  Get the number of positions in the file.
      CALL GRP_GRPSZ( IGRP, NP, STATUS )

*  Return a zero pointer if the file was empty.
      IF( NP .GT. 0 ) THEN

*  Get the number of axes in the supplied Frame.
         NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  Allocate memory to hold the returned co-ordinates.
         CALL PSX_CALLOC( NAX*NP, '_DOUBLE', IPOUT, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the positions from the group into the array allocated above.
         CALL KPG1_ASGRP( FRM, IGRP, NP, NAX, %VAL( IPOUT ), STATUS )

      END IF

 999  CONTINUE

*  Delete the GRP group.
      CALL GRP_DELET( IGRP, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occurrred, release the returned pointers.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL PSX_FREE( IPOUT, STATUS )      
         IPOUT = 0
         NP = 0

*  Give a context message.
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_ASFIL_ERR3', 'Error obtained a set of '//
     :                 'formatted positions using parameter %^PARAM.',
     :                  STATUS )

      END IF

      END
