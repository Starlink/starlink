      SUBROUTINE USR_DRIMAGE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_DRIMAGE

*  Purpose:
*     Draw data array on image diplay.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_DRIMAGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The data array is displayed using GKS Cell Array (GCA).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       AT4 version.
*     02-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     10-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     23-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Finalised inclusion of COLOUR parameter for false colour
*       image display.
*     19-SEP-94 (MJC):
*       IUEDR Vn. 3.1-4
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Constants:
      INTEGER ERR      ! Error status.
      PARAMETER (ERR=-3)

*  Local Constants:
      INTEGER LUTMIS   ! LUT level for missing data.
      PARAMETER (LUTMIS=0)

*  Status:
      INTEGER STATUS   ! Global status.

*  External References:
      REAL SNX_AGUGX   ! SNX User to Grid X-datum conversion.
      REAL SNX_AGUGY   ! SNX User to Grid Y-datum conversion.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPAL'
      INCLUDE 'CMIDEV'
      INCLUDE 'CMPSX'

*  Local Variables:
      REAL GX1         ! Grid X-axis start for image.
      REAL GX2         ! Grid X-axis end for image.
      REAL GY1         ! Grid Y-axis start for image.
      REAL GY2         ! Grid Y-axis end for image.
      REAL X1          ! User X-axis start for image.
      REAL X2          ! User X-axis end for image.
      REAL Y1          ! User Y-axis start for image.
      REAL Y2          ! User Y-axis end for image.

      INTEGER ACTVAL   ! Parameter value count.
      INTEGER DATA_VM  ! VM address for DATA array.
      INTEGER I        ! Loop index.
      INTEGER IXP      ! Number of X-axis pixels in image.
      INTEGER IYP      ! Number of Y-axis pixels in image.
      INTEGER LUTBOT   ! LUT level for pixels below LUTS(1).
      INTEGER LUTSEV( 8 )  ! LUT values for severity codes (bits 5-8).
      INTEGER LUTTOP   ! LUT level for pixels above LUTS(2).
      INTEGER LUTUSR   ! User-marked (bit 2) pixel LUT level.
      INTEGER NAXIS1   ! Size of axis 1.
      INTEGER NAXIS2   ! Size of axis 2.
      INTEGER QUAL_VM  ! VM address for QUAL array.
      INTEGER PLOT_VM  ! VM address for IMAGE array.

      LOGICAL NEWCOL   ! Whether colour LUT is required.
      LOGICAL QUAL     ! Whether data quality plotted.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   Map image.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         GO TO 999
      END IF

*   COLOUR.
      CALL RDPARL( 'COLOUR\\', .FALSE., 1, NEWCOL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'COLOUR\\', STATUS )
         GO TO 999
      END IF

*   Look-up table reload required?
      IF ( NEWCOL .NEQV. COLUT ) THEN
         COLUT = NEWCOL
         NEWLUT = .TRUE.
      END IF

*   Open image display.
      CALL GRF_OPIDEV( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   Change to SGS base zone.
      ZONE = 0
      CALL WRPARI( 'ZONE\\', 1, ZONE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CNPAR( 'ZONE\\', STATUS )
      END IF

      CALL GRF_TZONE( ZBASE, ZONE, ZCLEAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   Modify plotting flags.
      DRAWN = .FALSE.
      ERASED = .FALSE.

*   FLAG.
      CALL RDPARL( 'FLAG\\', .FALSE., 1, QUAL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'FLAG\\', STATUS )
         GO TO 999
      END IF

*   Design and draw axes.
      CALL PIDEIM( NAXIS1, NAXIS2, DZERO, DSCALE, DLIM, 'FLUX\\',
     :             '(FN)\\', 'S\\', '(PIXEL)\\', 'L\\', '(PIXEL)\\',
     :             TITLE, STATUS )

*   Branch on COLUT.
      IF ( .NOT. COLUT ) THEN

*      Set up LUT for severity codes
         DO I = 1, 8
            LUTSEV( I ) = LUTB
         END DO

*      Grey scale image representation - reseaux are GREEN.
         LUTSEV( 5 ) = TILUT( 4 )

*      Truncated ITF are ORANGE.
         LUTSEV( 6 ) = TILUT( 10 )

*      Saturated are RED.
         LUTSEV( 7 ) = TILUT( 5 )

*      USER marked are YELLOW.
         LUTUSR = TILUT( 3 )

*      LUTBOT is BLACK.
         LUTBOT = 0

*      LUTTOP is blue.
         LUTTOP = TILUT( 6 )

*   False colour image representation - set up LUT for severity codes.
      ELSE
         DO I = 1, 8
            LUTSEV( I ) = 0
         END DO

         LUTBOT = 0
         LUTUSR = 0
         LUTTOP = TILUT( 1 )
      END IF

*   Get VM for plot image.
      IXP = ABS( XP( 2 ) - XP( 1 ) ) + 1
      IYP = ABS( YP( 2 ) - YP( 1 ) ) + 1
      CALL ALADR( 'INT\\', IXP * IYP, PLOT_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*      Problems finding VM for image
         CALL ERROUT( 'Error: obtaining memory for image\\',
     :                STATUS )
      ELSE

*      Load image array.
         CALL PIDRI2( NAXIS1, NAXIS2, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :                DBLANK, XP, YP, IXP, IYP, ZLIM, DZERO, DSCALE,
     :                LUTS, LUTBOT, LUTTOP, LUTMIS, LUTUSR, LUTSEV,
     :                QUAL, %VAL( PLOT_VM ), STATUS )

*      Get GKS world coordinates of plot boundaries.
         X1 = MIN( XP( 1 ), XP( 2 ) ) - 0.5
         X2 = MAX( XP( 1 ), XP( 2 ) ) + 0.5
         Y1 = MIN( YP( 1 ), YP( 2 ) ) - 0.5
         Y2 = MAX( YP( 1 ), YP( 2 ) ) + 0.5

         GX1 = SNX_AGUGX( X1 )
         GX2 = SNX_AGUGX( X2 )
         GY1 = SNX_AGUGY( Y1 )
         GY2 = SNX_AGUGY( Y2 )

*      Call GCA.
         IF ( SYSNAME .EQ. 'VMS' ) THEN
           CALL GCA( GX1, GY1, GX2, GY2, IXP, IYP, IXP,
     :               %VAL( PLOT_VM ) )

         ELSE
           CALL GCA( GX1, GY1, GX2, GY2, IXP, IYP, 1, 1, IXP, IYP,
     :               %VAL( PLOT_VM ) )
         END IF

*      Flush.
         CALL SGS_FLUSH

*      Release image VM.
         CALL DLADR( PLOT_VM, STATUS )
      END IF

 999  CONTINUE

      END
