      SUBROUTINE PHO1_GPSF( XCEN, YCEN, SHAPE, CODE, CLIP, SEE,
     :                      ORIGIN, BUFFER, STATUS )

*+
*  Name :
*     PHO1_GPSF

*  Purpose :
*     Calculates photometry measurements.
*
*  Language :
*     Starlink Fortran-77
*
*  Invocation:
*     CALL PHO1_GPSF( XCEN, YCEN, SHAPE, CODE, CLIP, SEE,
*    :                ORIGIN, BUFFER, STATUS )
*
*  Description :
*     Calculates the final measurements for a PSF star and writes the
*     results into a string buffer.
*
*  Arguments :
*     XCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     YCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     SHAPE( 3 ) = REAL (Returned)
*        The three dimensions of the PSF
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of NDF axes
*     CODE = CHARACTER * ( 2 ) (Given)
*        Error code flag
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     SEE = REAL (Given)
*        Seeing in pixels.
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of NDF axes.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        The aperture measurements formatted into a string. The output
*        contains the following fields:
*           X Y FWHM1 FWHM2 ROT CODE CLIP SEE
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors :
*     AA: Alasdair Allan (STARLINK, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     1-FEB-1999 (AA):
*         Original version
*     17-JUN-2008 (PWD):
*         Scale "FWHM" by 1.665, corrects from gaussian sigma to FWHM.
*     2011 December 23 (MJC)
*        Tidied to near SGP/16 standard.  Removed unused PADU argument.
*     {enter_further_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given :
      REAL XCEN
      REAL YCEN
      REAL SHAPE(3)
      INTEGER ORIGIN( 2 )
      CHARACTER * ( 2 ) CODE
      REAL CLIP
      REAL SEE

* Arguments Returned:
      CHARACTER * ( * ) BUFFER

*  Status :
      INTEGER STATUS

*  Local Variables :
      CHARACTER * ( 9 ) CXCEN, CYCEN, CROT
      CHARACTER * ( 11 ) CFWHM1, CFWHM2
      CHARACTER * ( 5 ) CCLIP, CSEE

*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Xcen
      WRITE( CXCEN, '( F9.2 )' ) XCEN + REAL( ORIGIN( 1 ) - 1 )

*   Ycen
      WRITE( CYCEN, '( F9.2 )' ) YCEN + REAL( ORIGIN( 2 ) - 1 )

*   X-FWHM
      WRITE( CFWHM1, '( F9.4 )' ) SHAPE(1) * 1.665

*   Y-FWHM
      WRITE( CFWHM2, '( F9.4 )' ) SHAPE(2) * 1.665

*   Rotation
      WRITE( CROT, '( F7.4 )' ) SHAPE(3)

      WRITE( CCLIP, '( F5.1 )' ) CLIP
      WRITE( CSEE, '( F5.2 )' ) SEE

*   Concatenate these into the output string.
      BUFFER = CXCEN//' '//CYCEN//' '//CFWHM1//' '//CFWHM2//' '
     :         //CROT//' '//CODE//' '//CCLIP//' '//CSEE

  99  CONTINUE

      END

