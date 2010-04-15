      SUBROUTINE SPECX_CLOSE_FITS( IFAIL )
*+
*  Name:
*     SPECX_CLOSE_FITS

*  Purpose:
*     Close an output disk-FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPECX_CLOSE_FITS( IFAIL )

*  Description:
*     This routine serves the Specx command CLOSE-FITS-FILE to close a
*     disk file that has been used for FITS output. Tape units are not
*     supported.

*  Arguments:
*     IFAIL = INTEGER (Returned)
*        The global status. The status is reset on entry.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     22 Nov 1993 (hme):
*        Replace LIB$FREE_LUN with FIO_PUNIT.
*     09 Dec 1993 (rp):
*        Replace FIO_PUNIT with IFREELUN
*     27 Jan 1994 (hme):
*        Review to support only disk-FITS and to maximise use of FITS
*        related libraries in Portable Figaro.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:

*  Global Variables:
      INCLUDE 'SPECX_FITS'

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:

*  Internal References:
      INTEGER IFREELUN

*.

      IFAIL = 0
      CLOSE( LU )
      IFAIL = IFREELUN( LU )
      FITS_OPEN = .FALSE.

      END
