      SUBROUTINE POL1_LNAM( PARAM, INDXLO, INDXHI, TITLE, IGRP,
     :                      COMMEN, STATUS )
*+
*  Name:
*     POL1_LNAM

*  Purpose:
*     Lists the contents of a GRP group to a file via an ADAM
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_LNAM( PARAM, INDXHI, INDXLO, TITLE, IGRP, COMMEN,
*                     STATUS )

*  Description:
*     This routine writes the names of the elements in the input GRP
*     group into a text file. The text file is opened via the ADAM
*     parameter PARAM.  The names in the group are taken from the index
*     range INDXLO to INDXHI. A title is written to the first line of
*     the file.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter used to obtain the text file.
*     INDXLO = INTEGER (Given)
*        The index of the first element of the group which is to be
*        written out.
*     INDXHI = INTEGER (Given)
*        The index of the last element of the group which is to be
*        written out.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title for the first line of the file. This must contain the
*        character # first (i.e. '#  then the actual comment').
*     IGRP = INTEGER (Given)
*        The GRP identifier of the group.
*     COMMEN = _LOGICAL (Given)
*        Whether to write a comment to the user about the name of the
*        output file or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (PDRAPER):
*        Original version.
*     16-JUN-1993 (PDRAPER):
*        Added COMMEN argument as $PARAMETER does not return name.
*     3-DEC-1997 (DSB):
*        CCDPACK version modified for inclusion in POLPACK.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters

*  Arguments Given:
      CHARACTER *  ( * ) PARAM
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER * ( * ) TITLE
      INTEGER IGRP
      LOGICAL COMMEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) NAME ! Buffer for name
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer for file name
      INTEGER I                  ! Loop variable
      INTEGER FD                 ! File descriptor
      LOGICAL OPEN               ! File is open flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the file via the named adam parameter.
      OPEN = .FALSE.
      CALL CCD1_ASFIO( PARAM, 'WRITE', 'LIST', GRP__SZNAM, FD, OPEN,
     :                 STATUS )

*  Write the title
      CALL FIO_WRITE( FD, TITLE( : CHR_LEN( TITLE ) ), STATUS )

*  Loop over the required index extracting the names and then writting
*  them into the file.
      DO 1 I = INDXLO, INDXHI
         NAME = ' '
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )

*  Now write out the name.
         CALL FIO_WRITE( FD, NAME( : CHR_LEN( NAME ) ), STATUS )
 1    CONTINUE

      IF ( COMMEN .AND. STATUS .EQ. SAI__OK ) THEN

*  Write a comment about the name of the list.
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_OUT( ' ',
     :   '  Namelist written to file: ^FNAME', STATUS )
      END IF

*  Close the file if it is open.
      IF ( OPEN ) CALL FIO_CLOSE( FD, STATUS )
      END
