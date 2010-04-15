      SUBROUTINE POL1_KNEXT( NAME, OK, TYPE, STATUS )
*+
*  Name:
*     POL1_KNEXT

*  Purpose:
*     Checks that NAME is a known POLPACK extension item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_KNEXT( NAME, OK, TYPE, STATUS )

*  Description:
*     This routine contains a listing of all the currently known
*     POLPACK extension items and their data types. The NAME is
*     a full path excluding the NDF_NAME.MORE.POLPACK header.
*     (i.e. TIMES.EXPOSURE). It may be terminated by a question mark,
*     which is ignored.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the supposed POLPACK extension item.
*     OK = LOGICAL (Returned)
*        Set true if the NAME is recognised, otherwise is FALSE.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The HDS data type of the extension item (all extension items
*        are primitives).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1993 (PDRAPER):
*        Original version.
*     4-JAN-1994 (PDRAPER):
*        Changed extension item names to correspond more closely to the
*        global parameters used by CCDPACK.
*     3-DEC-1997 (DSB):
*        CCDPACK version modified for use in POLPACK.
*     12-FEB-1999 (DSB):
*        Added ANLANG, T, EPS. WPLATE changed from _CHAR to _REAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      LOGICAL OK
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      INTEGER NCOMP              ! No. of recognised components
      PARAMETER ( NCOMP = 10 )

      INTEGER NMLEN              ! Length of each name string
      PARAMETER ( NMLEN = 15 )

*  Local Variables:
      CHARACTER * ( NMLEN ) NAMES( NCOMP ) ! Known extension names
      CHARACTER * ( 8 )     TYPES( NCOMP ) ! Their types
      INTEGER I                      ! Loop variable
      INTEGER NAMLEN                 ! Used length of input string

*  Local Data:
      DATA NAMES / 'WPLATE',
     :             'IMGID',
     :             'FILTER',
     :             'RAY',
     :             'ANGROT',
     :             'STOKES',
     :             'ANLANG',
     :             'T',
     :             'EPS',
     :             'VERSION' /

      DATA TYPES / '_REAL',
     :             '_CHAR',
     :             '_CHAR',
     :             '_CHAR',
     :             '_REAL',
     :             '_CHAR',
     :             '_REAL',
     :             '_REAL',
     :             '_REAL',
     :             '_CHAR' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default is no match.
      TYPE = ' '
      OK = .FALSE.

*  Get length of NAME.
      NAMLEN = CHR_LEN( NAME )

*  If the last non-blank character is a question mark, ignore it.
      IF( NAME( NAMLEN : NAMLEN ) .EQ. '?' ) NAMLEN = NAMLEN - 1

*  If the name is not too long, compare it with each of the known
*  extension items.
      IF ( NAMLEN .LE. NMLEN ) THEN
         DO 1 I = 1, NCOMP
            IF ( NAMES( I )( 1 : NAMLEN) .EQ. NAME( 1 : NAMLEN ) ) THEN

*  Name is recognised.
               TYPE = TYPES( I )
               OK = .TRUE.
               GO TO 2
            END IF
 1       CONTINUE
 2       CONTINUE
      END IF

      END
