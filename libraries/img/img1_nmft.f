      SUBROUTINE IMG1_NMFT( SLOT, ESLOT, N, STATUS )
*+
* Name:
*    IMG1_NMFT

*  Purpose:
*     Returns the number of records in a FITS block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*  CALL IMG1_NMFT( SLOT, ESLOT, N, STATUS )

*  Description:
*     This routine access the FITS block associated with the NDF (SLOT)
*     and returns the number of valid records.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     ESLOT = INTEGER (Given)
*        The slot number of the extension.
*     N = INTEGER (Returned)
*        The number of records (may be zero).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1994 (PDRAPER):
*        Original version.
*     22-AUG-1994 (PDRAPER):
*        Fixed to take account of the 'END' keyword.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Read)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Read)
*        Number of entries in the FITS block.

*   Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Count the number of valid records directly. Cannot use any other
*  method as the position of the 'END' keyword is not known. Timing
*  tests indicate that the inefficiency of this approach isn't a
*  problem (at least for size ~few hundreds). Pass the mapped FITS
*  block to another routine to do the real work.  Note that the
*  %VAL( 80 ) appended after the last genuine argument is the length of
*  the mapped character strings. This is the usual method that UNIX
*  compilers use to pass this information.
      CALL IMG1_CKEY( ECB_FTSN( SLOT ), %VAL( ECB_FTSP( SLOT ) ), N,
     :                STATUS, %VAL( 80 ) )
      END
* $Id$
