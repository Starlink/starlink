      SUBROUTINE PREPB4( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB4

*  Purpose:
*     Create and write extra components in IMAGE_INFO of a Galatic
*     Plan Map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB4( NCARD, FITS, LOC, STATUS )

*  Description:
*     This routine creates following components in the IMAGE_INFO
*     structure of an IRAS Galactic Plane image NDF.
*                  HCON <_INTEGER>
*                  GALMAP <_INTEGER>
*     Where
*        HCON - the hours confirmation of the Galactic Plane Maps.
*        GALMAP - the map number of the Galactic Plane Maps.
*
*     These components will be assigned the values according to the
*     information obtained from the FITS extension of the NDF.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER*( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER*( * ) (Given)
*        The locator of the IMAGE_INFO structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-SEP-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from GPINFO to PREPB4, etc.
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
      CHARACTER HCNSTR*1         ! Hour confirmation string
      CHARACTER OBJECT*10        ! Value of keyword OBJECT
      CHARACTER PLNSTR*2         ! Plate number string


      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER HCNPSN             ! Position of hour confirmation string
      INTEGER HCON               ! Hours confirmation of the image
      INTEGER PLNNUM             ! Galactic Plane number of the image
      INTEGER PLNPSN             ! Position of plate number string


      LOGICAL THERE              ! FITS keyword flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0I( LOC, 'HCON', STATUS )
      CALL DAT_NEW0I( LOC, 'GALMAP', STATUS )

*  Get the value of FITS keyword OBJECT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, OBJECT, CARD,
     :             STATUS )

*  Remove all blanks in the string.
      CALL CHR_RMBLK( OBJECT )

*  Find the positions of 'GPL' and 'H' in OBJECT string.
      PLNPSN = INDEX( OBJECT, 'GPL' )
      HCNPSN = INDEX( OBJECT, 'H' )

*  Extract the plate number string and the hours confirmation string
*  from the OBJECT string.
      PLNSTR = OBJECT( PLNPSN + 3 : HCNPSN - 1 )
      HCNSTR = OBJECT( HCNPSN + 1 : )

*  Get the plate number and the hours confirmation from their string
*  expression.
      CALL CHR_CTOI( PLNSTR, PLNNUM, STATUS )
      CALL CHR_CTOI( HCNSTR, HCON, STATUS )

*  Write the GALMAP and HCON components of the IMAGE_INFO structure.
      CALL CMP_PUT0I( LOC, 'HCON', HCON, STATUS )
      CALL CMP_PUT0I( LOC, 'GALMAP', PLNNUM, STATUS )

      END
