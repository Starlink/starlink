      SUBROUTINE SCANHI( NAXIS1, NAXIS2, DATA, QUAL, STATUS )
*+
*  Name:
*     SUBROUTINE SCANHI

*  Purpose:
*     Perform scan across HIRES image perpendicular to dispersion
*     and store resultant data array in CMSCAN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCANHI( NAXIS1, NAXIS2, DATA, QUAL, STATUS )

*  Arguments:
*     NAXIS1 = INTEGER (Given)
*        X-axis size of array.
*     NAXIS2 = INTEGER (Given)
*        Y-axis size of array.
*     DATA = INTEGER*2 ( NAXIS1, NAXIS2 ) (Given)
*        The Image data.
*     QUAL = BYTE ( NAXIS1, NAXIS2 ) (Given)
*        Image Quality information.
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
*     16-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXTITLE = 80 )

*  Arguments Given:
      INTEGER NAXIS1      ! Size of axis 1 (sample).
      INTEGER NAXIS2      ! Size of axis 2 (line).
      INTEGER*2 DATA( NAXIS1, NAXIS2 ) ! Image.

      BYTE QUAL( NAXIS1, NAXIS2 ) ! Quality information.

*  Status:
      INTEGER STATUS      ! Status return.

*  External References:
      EXTERNAL UTOR       ! (U,V) to (S,L) Coordinate transform.
      EXTERNAL RTOU       ! (S,L) to (U,V) Coordinate transform.

*  Local Variables:
      REAL*8 SCANAV       ! Folding half-width in geometric pixels.
      REAL*8 U2

      INTEGER ACTVAL      ! Parameter value count.
      INTEGER IAPER       ! Aperture index.
      INTEGER IPOS
      INTEGER ORDERS( 2 ) ! Range of echelle orders.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is Data.
      IF ( NODATA ) THEN
         CALL ERROUT( 'Error: no data\\', STATUS )
         GO TO 999
      END IF

*  Set NOGRID.
      NOSCAN = .TRUE.

*  Perform checks, default assignments and prompts.
      CALL DEFAPR( 2, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\', STATUS )
         GO TO 999
      END IF

*  Define template.
      CALL HITEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: defining spectrum template\\', STATUS )
         GO TO 999
      END IF

*  V parameter.
      V1SCAN = 0.0
      CALL RDPARF( 'SCANDIST\\', .TRUE., 1, V1SCAN, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'SCANDIST\\', STATUS )
         GO TO 999
      END IF

*  SCANAV - half-width of band in geometric pixels.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'SCANAV\\', .FALSE., 1, SCANAV, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'SCANAV\\', STATUS )
            GO TO 999

         ELSE IF ( SCANAV .LE. 0.5 ) THEN
            CALL ERRPAR('SCANAV\\')
            CALL ERROUT(': out of range\\', STATUS)

         ELSE
            GO TO 100
         END IF

         CALL CNPAR( 'SCANAV\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'SCANAV\\', STATUS )
            GO TO 999
         END IF
      END DO
 100  CONTINUE

*  ORDERS.
      CALL GET_ORDERS( .FALSE., ORDERS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Dvscan IS SCANAV.
      DVSCAN = 2.0 * SCANAV

*  Define U-grid using ORDERS.
      CALL ORSET( ORDERS( 1 ) - 1 )
      CALL VTOU( V1SCAN, U1SCAN )
      U1SCAN = MAX( - DBLE( RADIUS ),
     :              MIN( DBLE( RADIUS ), U1SCAN ) )

      CALL ORSET( ORDERS( 2 ) + 1 )
      CALL VTOU( V1SCAN, U2 )
      U2 = MAX( - DBLE( RADIUS ),
     :          MIN( DBLE( RADIUS ), U2 ) )

      IF ( U1SCAN .EQ. U2 ) THEN
         CALL ERROUT('Error: U-range undefined\\', STATUS)
         GO TO 999

      ELSE IF ( U1SCAN .GT. U2 ) THEN
         CALL MSC_DSWAP( U1SCAN, U2 )
      END IF

      DUSCAN = 0.5
      NSCAN = NINT( ( U2 - U1SCAN ) / DUSCAN ) + 1

*  Print details.
      CALL LINE_WRITF( '%p Scan grid (%.1f, \\', U1SCAN )
      CALL LINE_WRITF( '%.1f,\\', U2 )
      CALL LINE_WRITF( '%.1f),\\', DUSCAN )
      CALL LINE_WRITF( ' offset %.1f,\\', V1SCAN )
      CALL LINE_WRITF( ' half-width %.1f\\', SCANAV )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI( '%p Includes echelle orders (%i,\\',
     :                 ORDERS( 1 ) )
      CALL LINE_WRITI( ' %i).\\', ORDERS( 2 ) )
      CALL PRTBUF( STATUS )

*  Map image.
      CALL MSC_MAP( NSCAN, U1SCAN, DUSCAN, 1, V1SCAN, DVSCAN, UTOR,
     :              RTOU, 1, NAXIS1, NAXIS2, DATA, QUAL, LMIN, LMAX,
     :              SMIN, SMAX, FSCAN, WSCAN, QSCAN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: mapping image\\', STATUS )
         GO TO 999
      END IF

*  Some labels.
      IPOS = 0
      CALL STR_WRITF( '%pV=%.3f(pixels),\\', V1SCAN, MAXTITLE, STITLE,
     :                IPOS )
      CALL STR_WRITF( ' FWHM=%.1f (pixels),\\', DVSCAN, MAXTITLE,
     :                STITLE, IPOS )
      CALL STR_WRITI( ' ORDERS=(%i,\\', ORDERS( 2 ), MAXTITLE, STITLE,
     :                IPOS )
      CALL STR_WRITI( '%i)\\', ORDERS( 1 ), MAXTITLE, STITLE, IPOS )
      CALL STR_MOVE( 'Net\\', MAXLABEL, SLABEL )

      IF ( PHOT ) THEN
         CALL STR_MOVE( '(FN/pixel)\\', MAXLABEL, SUNITS )
      ELSE
         CALL STR_MOVE( '(DN/pixel)\\', MAXLABEL, SUNITS )
      END IF

      CALL STR_MOVE( 'U\\', MAXLABEL, ULABEL )
      CALL STR_MOVE( '(pixels)\\', MAXLABEL, UUNITS )
      NOSCAN = .FALSE.

 999  CONTINUE

      END
