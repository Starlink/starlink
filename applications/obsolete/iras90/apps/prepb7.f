      SUBROUTINE PREPB7( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB7

*  Purpose:
*     Create and write extra components to IMAGE_INFO of an ISSA map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB7( NCARD, FITS, LOC, STATUS )

*  Description:
*     This routine create following components in the IMAGE_INFO
*     structure of a IRAS Sky Survey Atlas NDF.
*            HCON <_INTEGER>
*            ISSAFLD <_INTEGER>
*     Where
*        HCON - the hours confirmation of the ISSA image.
*        ISSAFLD - Field number of the image.
*
*     The components will be assigned values according to the
*     information obtained from the FITS extension of the NDF.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER * ( * ) (Given)
*        The locator of the IMAGE_INFO structure of the NDF file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-AUG-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from SSINFO to PREPB7, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FLDSTR*5         ! Plate number string
      CHARACTER HCNSTR*5         ! Hour confirmation string
      CHARACTER OBJECT*20        ! Value of keyword OBJECT


      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER FLDNUM             ! Sky plate number of the image
      INTEGER FLDPSN             ! Position of plate number string
      INTEGER HCNPSN             ! Position of hour confirmation string
      INTEGER HCON               ! Hours confirmation of the image
      INTEGER STCARD             ! Start card number when search FITS


      LOGICAL THERE              ! FITS keyword flag
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0I( LOC, 'HCON', STATUS )
      CALL DAT_NEW0I( LOC, 'ISSAFLD', STATUS )

*  Get the value of FITS keyword OBJECT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, OBJECT, CARD,
     :             STATUS )

*  Remove all blanks in the string and convert to upper case.
      CALL CHR_RMBLK( OBJECT )
      CALL CHR_UCASE( OBJECT )

*  Find the positions of 'F' and 'H' in OBJECT string.
      FLDPSN = INDEX( OBJECT, 'F' )
      HCNPSN = INDEX( OBJECT, 'H' )

*  Extract the field number string and the hours confirmation string
*  from the OBJECT string.
      FLDSTR = OBJECT( FLDPSN + 1 : HCNPSN - 1 )
      HCNSTR = OBJECT( HCNPSN + 1 : )

*  Get the plate number and the hours confirmation from their string
*  expression.
      CALL CHR_CTOI( FLDSTR, FLDNUM, STATUS )
      CALL CHR_CTOI( HCNSTR, HCON, STATUS )

*  Write the ISSAFLD and HCON components of the IMAGE_INFO structure.
      CALL CMP_PUT0I( LOC, 'ISSAFLD', FLDNUM, STATUS )
      CALL CMP_PUT0I( LOC, 'HCON', HCON, STATUS )

      END
