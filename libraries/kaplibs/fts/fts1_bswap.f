      SUBROUTINE FTS1_BSWAP( NBYTE, BYTES, STATUS )
*+
*  Name:
*     FTS1_BSWAP

*  Purpose:
*     Swaps adjacent bytes in an array of bytes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_BSWAP( NBYTE, BYTES, STATUS )

*  Description:
*     This swaps adjacent pairs of bytes in an array in situ.  This is
*     VAX specific.

*  Arguments:
*     NBYTE = INTEGER (Returned)
*        Number of bytes.  An SAI__ERROR will be returned if this is not
*        an even number.
*     BYTES( NBYTE ) = BYTE (Given and Returned)
*        The 16-bit words whose bytes are to be swapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 11 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NBYTE

*  Arguments Given and Returned:
      BYTE BYTES( NBYTE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      BYTE
     :  DUMMY                    ! Dummy used for copying.

      INTEGER
     :  I                        ! Loop counter
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the number of bytes.
      IF ( MOD( NBYTE, 2 ) .NE. 0 ) THEN
         CALL ERR_REP( 'FTS1_BSWAP_ODDBYTE',
     :     'Unable to swap bytes because an odd number (^NB) has been '/
     :     /'supplied. (Probable programming error.)', STATUS )
      ELSE

*  Swap the bytes in each word.
         DO I = 1, NBYTE, 2
            DUMMY = BYTES( I )
            BYTES( I ) = BYTES( I + 1 )
            BYTES( I + 1 ) = DUMMY
         END DO
      END IF
        
      END
