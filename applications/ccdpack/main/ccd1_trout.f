      SUBROUTINE CCD1_TROUT( TR, STATUS )
*+
*  Name:
*     CCD1_TROUT

*  Purpose:
*     Output linear transformation coefficients in a tabular form.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_TROUT( TR, STATUS )

*  Description:
*     This routine prints the six coefficients submitted in the TR array
*     in a suitable form given that they represent the coefficients of 
*     a linear transformation:
*
*        X' = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        Y' = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*
*     Output is via the CCDPACK logging mechanism.

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients to be output.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     The coefficients are converted from DOUBLE PRECISION to REAL 
*     before being converted to strings.  This is so that the resulting
*     strings are short enough to be output three to a line without
*     truncation.  

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Standard MSG/ERR constants
      
*  Arguments Given:
      DOUBLE PRECISION TR( 6 )
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Buffer for output
      INTEGER IAT               ! Character position in buffer
      INTEGER NCHAR             ! Number of characters converted

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Output the coefficients.
      BUFFER = ' '
      IAT = 4
      BUFFER( IAT: ) = ' A ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 1 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 29
      BUFFER( IAT: ) = ' B ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 2 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 54
      BUFFER( IAT: ) = ' C ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 3 ) ), BUFFER( IAT: ), NCHAR )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )
      BUFFER = ' '
      IAT = 4
      BUFFER( IAT: ) = ' D ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 4 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 29
      BUFFER( IAT: ) = ' E ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 5 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 54
      BUFFER( IAT: ) = ' F ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 6 ) ), BUFFER( IAT: ), NCHAR )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

      END
* $Id$
