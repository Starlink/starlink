************************************************************************

      SUBROUTINE OUTPSF ( FOUT, INDEX, XCEN, YCEN, ORIGIN, 
     :                    SHAPE, SKY, PADU, CODE, STATUS)

*+
*  Name :
*     OUTPSF
*
*  Purpose :
*     Output the results of the PSF star measurement
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OUTPSF (  FOUT, INDEX, XCEN, YCEN, ORIGIN, 
*    :               SHAPE, SKY, PADU, CODE, STATUS)
*
*  Description :
*     Output the results of the PSF star measurement
*
*  Arguments :
*     FOUT = INTEGER (Given)
*        Identifier for results file used by FIO_
*     INDEX = INTEGER (Given)
*        Index number of object (shold always be 0 for PSF star)
*     XCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     YCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of NDF axes
*     SHAPE( 3 ) = REAL (Given)
*        Parameters defining the shape of the profile
*     SKY = REAL (Given)
*        Sky value per pixel
*     PADU = REAL (Given)
*        Photons per data unit
*     CODE = CHARACTER*2 (Given)
*        Saturated star or bad pixel flag
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     25-JAN-1999 (AA)
*          Original version
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings.
*     17-JUN-2008 (PWD):
*         Scale "FWHM" by 1.665, corrects from gaussian sigma to FWHM.
*         Change header to show PSF columns, not ones for a STAR.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'


*  Arguments Given :
      INTEGER FOUT
      INTEGER INDEX
      REAL XCEN
      REAL YCEN
      INTEGER ORIGIN( 2 )
      REAL SHAPE( 3 )
      REAL SKY, PADU
      CHARACTER * ( 2 ) CODE

*  Status :
      INTEGER STATUS

*  Local Variables :

      REAL PSKY

      CHARACTER CTEMP * 80
      
      CHARACTER CINDEX * 5
      CHARACTER CXCEN * 9, CYCEN * 9
      CHARACTER CFWHM1 * 11, CFWHM2 * 11, CROT * 9      
      CHARACTER CSKY * 11     
      CHARACTER TEXT * 80

*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN


*   Code the output into seperate character strings
*   Index - limit to 5 characters
      WRITE( CTEMP, '( I80 )' ) INDEX
      CINDEX = CTEMP( 76:80 )

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

*   Sky - use E format if number is too large
      PSKY = SKY * PADU
      IF ( ( PSKY .GT. 1.0E6 ) .OR. ( PSKY .LT. -1.0E6 ) ) THEN
         WRITE( CSKY, '( E11.4 )' ) PSKY
      ELSE
         WRITE( CSKY, '( F11.3 )' ) PSKY
      ENDIF

*   Concatenate these into the output strings
      TEXT = CINDEX//CXCEN//CYCEN//CFWHM1//CFWHM2//
     :       CROT//' '//CODE
      CALL MSG_OUT( ' ', TEXT, STATUS )
      CALL FIO_WRITE( FOUT, TEXT, STATUS )

*   Write out the header to the terminal
      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', '          x        y      fwhm     fwhm'//
     :              '     angle  code', STATUS )
      
      END

