      SUBROUTINE SCANLO( NAXIS1, NAXIS2, DATA, QUAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE SCANLO
*
*   Function:
*      Perform scan across LORES image perpendicular to dispersion
*      and store resultant data array in CMSCAN.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          16-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     14-SEP-94     IUEDR Vn. 3.1-3
*
*   Description:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NAXIS1                     ! size of axis 1 (sample)
      INTEGER NAXIS2                     ! size of axis 2 (line)

      INTEGER*2 DATA(NAXIS1, NAXIS2)     ! image

      BYTE QUAL(NAXIS1, NAXIS2)          ! quality

*   Export:
      INTEGER STATUS                     ! status return

*   External references:
      EXTERNAL UTOR                      ! (U,V) to (S,L) coordinate transform
      EXTERNAL RTOU                      ! (S,L) to (U,V) coordinate transform
      EXTERNAL WTOR                      ! (R,W) to (S,L) coordinate transform
      EXTERNAL RTOW                      ! (S,L) to (R,W) coordinate transform

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'

*   Local variables:
      REAL*8 R           !
      REAL*8 SCANAV      ! averaging half-width in geometric pixels
      REAL*8 U2          !
      REAL*8 X1          !
      REAL*8 Y1          !
      REAL*8 X2          !
      REAL*8 Y2          !

      INTEGER ACTVAL     ! parameter value count
      INTEGER IAPER      ! aperture index
      INTEGER IPOS       !
      INTEGER ISTAT      ! internal STATUS

*   Check that there is Data
      IF ( NODATA ) THEN
         CALL ERROUT( 'Error: no data\\', STATUS )
         RETURN
      END IF

*   Set NOGRID
      NOSCAN = .TRUE.

*   Perform checks, default assignments and prompts
      CALL DEFAPR( 1, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\', STATUS )
         RETURN
      END IF

*   Define template
      CALL LOTEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         RETURN
      END IF

*   SCANWV parameter
      CALL RTOW( DBLE(CENTRE(1)), DBLE(CENTRE(2)), R, V1SCAN )
      CALL RDPARF( 'SCANWV\\', .TRUE., 1, V1SCAN, ACTVAL, STATUS )
      CALL CNPAR( 'SCANWV\\', ISTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'SCANWV\\', STATUS )
         RETURN

      ELSE IF ( ISTAT .NE. SAI__OK ) THEN
         CALL PCANER( 'SCANWV\\', STATUS )
         RETURN
      END IF

*   SCANAV - half width of band in geometric pixels
 100  CONTINUE
      CALL RDPARF( 'SCANAV\\', .FALSE., 1, SCANAV, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'SCANAV\\', STATUS )
         RETURN

      ELSE IF ( SCANAV .LE. 0.5 ) THEN
         CALL ERRPAR( 'SCANAV\\' )
         CALL ERROUT( ': out of range\\', STATUS )

      ELSE
         GO TO 200
      END IF

      CALL CNPAR( 'SCANAV\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'SCANAV\\', STATUS )
         RETURN
      END IF

      GO TO 100
 200  CONTINUE

*   Convert SCANAV to a wavelength band
      CALL WTOG( 0.0d0, V1SCAN - 1.0d0, X1, Y1 )
      CALL WTOG( 0.0d0, V1SCAN + 1.0d0, X2, Y2 )
      DVSCAN = (SCANAV * 2.0) / SQRT((X2 - X1)**2 + (Y2 - Y1)**2)

*   Define U-grid
      U1SCAN = -30.0
      U2 = -U1SCAN
      DUSCAN = 0.5
      NSCAN = NINT(REAL((U2 - U1SCAN) / DUSCAN)) + 1

*   Print details
      CALL LINE_WRITF( '%p Scan grid (%.1f,\\', U1SCAN )
      CALL LINE_WRITF( '%.1f,\\', U2 )
      CALL LINE_WRITF( '%.1f),\\', DUSCAN )
      CALL LINE_WRITF( ' wavelength %.2f,\\', V1SCAN )
      CALL LINE_WRITF( ' half-width %.1f (pixels).\\', SCANAV )
      CALL PRTBUF( STATUS )

*   Scan in (R,W) space
      CALL MSC_MAP( NSCAN, U1SCAN, DUSCAN, 1, V1SCAN, DVSCAN, WTOR,
     :              RTOW, 1, NAXIS1, NAXIS2, DATA, QUAL, LMIN, LMAX,
     :              SMIN, SMAX, FSCAN, WSCAN, QSCAN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: mapping image\\', STATUS )
         RETURN
      END IF

*   Some labels
      IPOS = 0
      CALL STR_WRITF( '%pWAVELENGTH=%.3f,\\', V1SCAN, 80, STITLE,
     :                IPOS )
      CALL STR_WRITF( ' FWHM=%.1f (pixels)\\', DVSCAN, 80, STITLE,
     :                IPOS )
      CALL STR_MOVE( 'Net\\', 40, SLABEL )

      IF ( PHOT ) THEN
         CALL STR_MOVE( '(FN/pixel)\\', 40, SUNITS )

      ELSE
         CALL STR_MOVE( '(DN/pixel)\\', 40, SUNITS )
      END IF

      CALL STR_MOVE( 'R\\', 40, ULABEL )
      CALL STR_MOVE( '(pixels)\\', 40, UUNITS )
      NOSCAN = .FALSE.
      END
