      SUBROUTINE PRPAR( STATUS )
*+
*  Name:
*     SUBROUTINE PRPAR

*  Purpose:
*     The available information used by STAK is printed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRPAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     11-JAN-88 (PCTR):
*       IUEDR Vn. 1.4
*     05-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     26-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMABS'
      INCLUDE 'CMRIP'
      INCLUDE 'CMFILE'
      INCLUDE 'CMCUT'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMGEOM'
      INCLUDE 'CMDISP'
      INCLUDE 'CMHAL'
      INCLUDE 'CMITFC'
      INCLUDE 'CMTEM'

*  Local Variables:
      INTEGER N             ! accumulator
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   File information.
      IF ( NODSN ) THEN
         GO TO 999
      END IF

*   Camera,Image,Aperture,Resolution,Geometry-Photometry,Exposure.
      CALL LINE_WRITS( '%p %2w%s\\', CAMERA )
      CALL LINE_WRITI( '%i, \\', IMAGE )
      CALL LINE_WRITS( '%s, \\', APER )
      CALL LINE_WRITS( '%s, \\', RESOL )
      CALL LINE_WRITS( '%s, \\', TYPE )

      CALL LINE_WCONT( 'THDA=\\' )
      IF ( THDA .NE. 0.0 ) THEN
         CALL LINE_WRITF( '%f(C), \\', THDA )

      ELSE
         CALL LINE_WCONT( '<undefined>, \\' )
      END IF

      CALL LINE_WCONT( 'DATE=\\' )
      IF ( DAY .GT. 0 ) THEN
         CALL LINE_WRITI( '%i\\', DAY )

      ELSE
         CALL LINE_WCONT( 'dd\\' )
      END IF

      CALL LINE_WCONT( '/\\' )
      IF ( MONTH .GT. 0 ) THEN
         CALL LINE_WRITI( '%i\\', MONTH )

      ELSE
         CALL LINE_WCONT( 'mm\\' )
      END IF

      CALL LINE_WCONT( '/\\' )
      IF ( YEAR .GT. 0 ) THEN
         CALL LINE_WRITI( '%i\\', YEAR )

      ELSE
         CALL LINE_WCONT( 'yy\\' )
      END IF

      CALL LINE_WCONT( ', \\' )
      CALL PRTBUF( STATUS )

*   OBJECT.
      CALL LINE_WRITS( '%p%2w OBJECT=''%S''\\', TITLE )
      CALL LINE_WCONT( ', \\' )

*   ITF.
      CALL LINE_WCONT( 'ITF=\\' )
      IF ( ITF .GT. 0 ) THEN
         CALL LINE_WRITI( '%i\\', ITF )

      ELSE
         CALL LINE_WCONT( '<undefined>\\' )
      END IF
      CALL PRTBUF( STATUS )

*   File Change Switches.
      IF (DACHAN .OR. DQCHAN .OR. CACHAN .OR. SPCHAN .OR. MECHAN) THEN
         CALL LINE_WCONT( '%p%2w The (\\' )
         N = 0

         IF ( CACHAN ) THEN
            CALL LINE_WCONT( 'UEC\\' )
            N = N + 1
         END IF

         IF ( DACHAN ) THEN
            IF ( N .GT. 0 ) THEN
               CALL LINE_WCONT( ',\\' )
            END IF
            CALL LINE_WCONT( 'UED\\' )
            N = N + 1
         END IF

         IF ( DQCHAN ) THEN
            IF ( N .GT. 0 ) THEN
               CALL LINE_WCONT( ',\\' )
            END IF
            CALL LINE_WCONT( 'UEQ\\' )
            N = N + 1
         END IF

         IF ( SPCHAN ) THEN
            IF ( N .GT. 0 ) THEN
               CALL LINE_WCONT( ',\\' )
            END IF
            CALL LINE_WCONT( 'UES\\' )
            N = N + 1
         END IF

         IF ( MECHAN ) THEN
            IF ( N .GT. 0 ) THEN
               CALL LINE_WCONT( ',\\' )
            END IF
            CALL LINE_WCONT( 'UEM\\' )
            N = N + 1
         END IF

         CALL LINE_WCONT( ') file(s) are marked for change.\\' )

      ELSE
         CALL LINE_WCONT( '%p%2w No files marked for change.\\' )
      END IF
      CALL PRTBUF( STATUS )

*   Throw a line.
      CALL PRTEOL( STATUS )

*   Which data components present.
      CALL LINE_WCONT( '%p%2w Contents:\\' )
      CALL PRTBUF( STATUS )

*   Image.
      IF ( NODATA ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Image.\\' )
      CALL PRTBUF( STATUS )

*   ITF Correction.
      IF ( NOITFC ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'ITF Adjustment.\\' )
      CALL PRTBUF( STATUS )

*   Fiducials.
      IF ( NOFIDS ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Fiducial Positions.\\' )
      CALL PRTBUF( STATUS )

*   Faceplate.
      IF ( NOFACE ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Faceplate Parameters.\\' )
      CALL PRTBUF( STATUS )

*   Geometry.
      IF ( NOGEOM ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Geometric Distortion Representation.\\' )
      CALL PRTBUF( STATUS )

*   Dispersion.
      IF ( NODISP ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Spectrograph Dispersion Parameters.\\' )
      CALL PRTBUF( STATUS )

*   Templates.
      IF ( NOTEM ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Spectrum Centroid Templates.\\' )
      CALL PRTBUF( STATUS )

*   Absolute Calibration.
      IF ( NOABS ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Absolute Flux Calibration.\\' )
      CALL PRTBUF( STATUS )

*   HIRES things.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN

*      Ripple Calibration.
         IF ( NORIP ) THEN
            CALL LINE_WCONT( '%p%4w No \\' )

         ELSE
            CALL LINE_WCONT( '%p%4w \\' )
         END IF
         CALL LINE_WCONT( 'Ripple Calibration.\\' )
         CALL PRTBUF( STATUS )

*      Wavelength Clipping.
         IF ( NOCUT ) THEN
            CALL LINE_WCONT( '%p%4w No \\' )

         ELSE
            CALL LINE_WCONT( '%p%4w \\' )
         END IF
         CALL LINE_WCONT( 'Echelle order Wavelength Limits.\\' )
         CALL PRTBUF( STATUS )

*      Order Overlap.
         IF ( NOHAL ) THEN
            CALL LINE_WCONT( '%p%4w No \\' )

         ELSE
            CALL LINE_WCONT( '%p%4w \\' )
         END IF
         CALL LINE_WCONT( 'Order Overlap Background Correction.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Spectrum.
      IF ( NOSPEC ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Uncalibrated Spectrum.\\' )
      CALL PRTBUF( STATUS )

*   Mean Spectrum.
      IF ( NOCOMB ) THEN
         CALL LINE_WCONT( '%p%4w No \\' )

      ELSE
         CALL LINE_WCONT( '%p%4w \\' )
      END IF
      CALL LINE_WCONT( 'Mean Spectrum.\\' )
      CALL PRTBUF( STATUS )

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )

*   Throw.
      CALL PRTEOL( STATUS )

 999  CONTINUE

      END
