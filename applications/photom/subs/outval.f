************************************************************************

      SUBROUTINE OUTVAL ( A, E, THETA, CENTRO, CONCEN, PADU, MAGS,
     :                    SKYMAG, SKYEST, SKY, SKYSIG, INNER, OUTER,
     :                    PHOTON, BIASLE, SATURE, ETIME, USEMSK,
     :                    OPTIMA, CLIP, SEE, STATUS )

*+
*  Name :
*     OUTVAL
*
*  Purpose :
*     This outputs the current values of all the important parameters
*     to the terminal
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OUTVAL( A, E, THETA, CENTRO, CONCEN, PADU, MAGS, SKYMAG, SKYEST,
*    :             SKY, SKYSIG, INNER, OUTER, PHOTON, BIASLE, SATURE,
*    :             ETIME, USEMSK, OPTIMA, CLIP, SEE, STATUS )
*
*  Description :
*     This outputs the current values of all the important parameters
*     to the terminal
*
*  Arguments :
*     A = REAL (Given)
*        Semi-major axis of elliptical aperture
*     E = REAL (Given)
*        Eccentricity of elliptical aperture
*     THETA = REAL (Given)
*        Orientation of elliptical aperture
*     CENTRO = LOGICAL (Given)
*        Flag to indicate use of centroiding
*     CONCEN = LOGICAL (Given)
*        Flag to indicate use of concentric sky aperture
*     PADU = REAL (Given)
*        Photons per A.D.U.
*     MAGS = LOGICAL (Given)
*        If TRUE then output will be given in magnitudes.
*     SKYMAG = REAL (Given)
*        Zero point for sky magnitude
*     SKYEST = INTEGER (Given)
*        Choice of sky estimators
*     SKY = REAL (Given)
*        Value in sky aperture
*     SKYSIG = REAL (Given)
*        Deviation in sky aperture
*     INNER = REAL (Given)
*        Inner radius of concentric sky aperture
*     OUTER = REAL (Given)
*        Outer radius of concentric sky aperture
*     PHOTON = INTEGER (Given)
*        Choice of error estimator
*     BIASLE = REAL (Given)
*        Bias level
*     SATURE = REAL (Given)
*        Saturation level
*     ETIME = REAL (Given)
*        Exposure time
*     USEMSK = LOGICAL (Given)
*        Flag to indicate use of masking facility
*     OPTIMA = LOGICAL (Given)
*        Flag to indicate the use of optimal extraction
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     SEE = REAL (Given)
*        Approx seeing in pixels
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
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     10-APR-1988 (NE):
*        Original version.
*     10-SEP-1989
*        Added SKYEST and SKY to list
*     10-OCT-1989
*        Added ETIME, USEMSK and SKYSIG
*     10-AUG-1990
*        Changed format
*     10-JAN-1992
*        Added data variance to PHOTON
*     6-NOV-1996 (PWD)
*        Added MAGS parameter and made associated changes.
*     3-DEC-1998 (AA)
*        Added OPTIMA parameter and made associated changes.
*     6-DEC-1998 (AA)
*        Added CLIP and SEE paraemters and made associated changes.
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Increase
*        output buffer to stop overwrites.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given :
      REAL A
      REAL E
      REAL THETA
      LOGICAL CENTRO
      LOGICAL CONCEN
      REAL PADU
      LOGICAL MAGS
      REAL SKYMAG
      INTEGER SKYEST
      REAL SKY
      REAL SKYSIG
      REAL INNER
      REAL OUTER
      INTEGER PHOTON
      REAL BIASLE
      REAL SATURE
      REAL ETIME
      LOGICAL USEMSK
      LOGICAL OPTIMA
      REAL CLIP, SEE

*  Status :
      INTEGER STATUS

*  Local Variables :
      CHARACTER TEXT * ( MSG__SZMSG )
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF (.NOT. OPTIMA) THEN

            CALL MSG_OUT( ' ', ' ', STATUS )
            WRITE( TEXT, '('' Semim           ='', F6.1 )' ) A
            CALL MSG_OUT( ' ', TEXT, STATUS )
            WRITE( TEXT, '('' Eccen           ='', F7.2 )' ) E
            CALL MSG_OUT( ' ', TEXT, STATUS )
            WRITE( TEXT, '('' Angle           ='', F6.1 )' ) THETA
            CALL MSG_OUT( ' ', TEXT, STATUS )

	 ENDIF

         CALL MSG_OUT( ' ', ' ', STATUS )
         IF ( CENTRO ) THEN
            TEXT = ' Centroiding of star in aperture'
         ELSE
            TEXT = ' No centroiding of star in aperture'
         ENDIF
         CALL MSG_OUT( ' ', TEXT, STATUS )

         CALL MSG_OUT( ' ', ' ', STATUS )
         IF ( CONCEN ) THEN
            CALL MSG_OUT( ' ', ' Concentric sky aperture ', STATUS )
            WRITE( TEXT, '('' Inner radius    ='', F6.1 )' ) INNER
            CALL MSG_OUT( ' ', TEXT, STATUS )
	    IF ( .NOT. OPTIMA ) THEN
               WRITE( TEXT, '('' Outer radius    ='', F6.1, 4X,
     :                ''times object aperture radius '')' ) OUTER
            ELSE
               WRITE( TEXT, '('' Outer radius    ='', F6.1, 4X,
     :                ''times clipping radius '')' ) OUTER
            ENDIF
            CALL MSG_OUT( ' ', TEXT, STATUS )
         ELSE
            CALL MSG_OUT( ' ', ' Interactive sky aperture ', STATUS )
         ENDIF

*   Output the type of sky estimator used and whether a mask is in use
*   Skyest = 1 = Simple mean
*   Skyest = 2 = Mean with 2 sigma rejection
*   Skyest = 3 = Mode
*   Skyest = 4 = User supplied
         CALL MSG_OUT( ' ', ' ', STATUS )
         IF ( USEMSK ) THEN
            CALL MSG_OUT( ' ', ' Sky mask in use ', STATUS )
         ENDIF
         IF ( SKYEST .EQ. 1 ) THEN
            TEXT = ' Sky estimator   =  Simple mean'
         ELSEIF ( SKYEST .EQ. 2 ) THEN
            TEXT = ' Sky estimator   =  Mean with 2 sigma rejection'
         ELSEIF ( SKYEST .EQ. 3 ) THEN
            TEXT = ' Sky estimator   =  Mode'
         ELSEIF ( SKYEST .EQ. 4 ) THEN
            WRITE ( TEXT, '('' Sky estimator   =  User given   ='',
     :              F7.1, 3X, '' Sky variance ='', F6.1 )' ) SKY, SKYSIG
         ENDIF
         CALL MSG_OUT( ' ', TEXT, STATUS )

         IF ( MAGS ) THEN
            WRITE( TEXT, '('' Sky magnitude   ='', F6.1 )' ) SKYMAG
            CALL MSG_OUT( ' ', TEXT, STATUS )
         END IF

         CALL MSG_OUT( ' ', ' ', STATUS )
         WRITE ( TEXT, '('' Photons per ADU ='', F7.2 )' ) PADU
         CALL MSG_OUT( ' ', TEXT, STATUS )
         WRITE ( TEXT, '('' Exposure time   ='', F7.2 )' ) ETIME
         CALL MSG_OUT( ' ', TEXT, STATUS )
         WRITE ( TEXT, '('' Saturation level ( data units ) ='',
     :           E12.5 )' ) SATURE
         CALL MSG_OUT( ' ', TEXT, STATUS )

*   Output the type of error estimator used
*   Photon = 1 = photon statistics
*   Photon = 2 = sky variance
*   Photon = 3 = data variance
         CALL MSG_OUT( ' ', ' ', STATUS )
         IF ( PHOTON .EQ. 1 ) THEN
            CALL MSG_OUT(' ', ' Errors from photon statistics', STATUS)
            WRITE( TEXT,
     :      '('' Bias level ( data units )       ='', F7.1 )' ) BIASLE
            CALL MSG_OUT( ' ', TEXT, STATUS )
         ELSEIF ( PHOTON .EQ. 2 ) THEN
            CALL MSG_OUT( ' ', ' Errors from sky variance', STATUS )
         ELSEIF ( PHOTON .EQ. 3 ) THEN
            CALL MSG_OUT( ' ', ' Errors from data variance', STATUS )
         ENDIF

*   Remind user of output data values units.
         IF ( MAGS ) THEN
            CALL MSG_OUT( ' ', ' Results in magnitudes', STATUS )
         ELSE
            CALL MSG_OUT( ' ', ' Results in photon counts', STATUS )
         END IF

         CALL MSG_OUT( ' ', ' ', STATUS )

*   Optimal extraction?
         IF ( OPTIMA ) THEN
            CALL MSG_OUT( ' ', ' Using optimal extraction', STATUS )

            WRITE(TEXT, '(''    Clipping radius      ='', F5.1 )') CLIP
            CALL MSG_OUT( ' ', TEXT, STATUS )

            WRITE(TEXT, '(''    Approx seeing (pix)  ='', F5.1 )') SEE
            CALL MSG_OUT( ' ', TEXT, STATUS )
	 ELSE
            CALL MSG_OUT( ' ', ' Using aperture extraction', STATUS )
         ENDIF

      ENDIF

      END

