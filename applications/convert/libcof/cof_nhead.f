      SUBROUTINE COF_NHEAD( FUNIT, FILE, NCARD, STATUS )
*+
*  Name:
*     COF_NHEAD

*  Purpose:
*     Obtains the number of FITS headers in the current HDU.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NHEAD( FUNIT, FILE, NCARD, STATUS )

*  Description:
*     This routine merely obtains the number of FITS headers in the
*     current header and data unit.  It takes care of any error
*     status.  It exists because this code is needed by many
*     subroutines.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or device being converted.  This is
*     NCARD = INTEGER (Returned)
*        The number of FITS header cards in the current HDU.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must be open.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 November 16 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE

*  Arguments Returned:
      INTEGER NCARD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      INTEGER FSTAT              ! FITSIO error status
      INTEGER KEYADD             ! Number of headers which can be added
      INTEGER NCF                ! Number of characters in the file name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of FITS headers.
      CALL FTGHSP( FUNIT, NCARD, KEYADD, FSTAT )

      IF ( FSTAT .NE. FITSOK ) THEN

*  Get the length of the filename.
         NCF = CHR_LEN( FILE )

*  Report an error if something has gone wrong.
         BUFFER = 'Error obtaining the number of header cards from '/
     :            /'the FITS file '//FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_NHEAD_NCARD', 
     :                  'FTGHSP', BUFFER, STATUS )
      END IF

      END
