      SUBROUTINE IRM_GROPN( PDEV, PXSZ, PYSZ, CLEAR, COMMNT, PIC0,
     :                      PIC1, ZONE, COLOUR, CURSOR, CLRBLK, STATUS )
*+
*  Name:
*     IRM_GROPN

*  Purpose:
*     Open an SGS workstation for use with NCAR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GROPN( PDEV, PXSZ, PYSZ, CLEAR, COMMNT, PIC0, PIC1,
*                  ZONE, COLOUR, CURSOR, CLRBLK, STATUS )

*  Description:
*     This routine gets the name of an SGS workstation from the user and
*     opens it, using AGI to create a zone corresponding to the current
*     AGI picture. The zone will be cleared upon opening if requested.
*     A new zone is reated within this first zone of a size specified by
*     the user. This new zone is stored in the AGI database as the
*     'FRAME' picture for future plotting and is the current SGS zone on
*     on exit from this routine. A call to SNX_AGWV is made before
*     returning to ensure that AUTOGRAPH uses the full zone for
*     plotting. Some attributes of the graphic device, such as whether
*     the cursor and colour are available on the device and whether the
*     zone can be partially cleared, is also returned. If colour is
*     available on the device and the colour representation is
*     changeable, the routine will set the colour representation to the
*     standard SGS pen colour.
*

*  Arguments:
*     PDEV = CHARACTER*( * ) (Given)
*        The name of an ADAM parameter used to get a graphics
*        workstation.
*     PXSZ = CHARACTER*( * ) (Given)
*        The name of an ADAM parameter used to get the required X extent
*        of the FRAME picture in metres.
*     PYSZ = CHARACTER*( * ) (Given)
*        The name of an ADAM parameter used to get the required Y extent
*        of the FRAME picture in metres.
*     CLEAR = LOGICAL (Given)
*        If true, the display will be cleared upon opening.
*     COMMNT = CHARACTER*( * )
*        Comment write to the AGI database with the FRAME picture.
*     PIC0 = INTEGER (Returned)
*        The picture identifier for the picture on entry.
*     PIC1 = INTEGER (Returned)
*        The picture identifier for the new FRAME picture.
*     ZONE = INTEGER (Returned)
*        The SGS zone identifier for the current zone on exit.
*     COLOUR = LOGICAL (Returned)
*        If the graphics workstation has colour available, it is true.
*        Otherwise it is false.
*     CURSOR = LOGICAL (Returned)
*        If the cursor is available on the device, it is true.
*        Otherwise, it is false.
*     CLRBLK = LOGICAL (Returned)
*        If the zone can be partially cleared, it is true. Otherwise, it
*        is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*( * ) PDEV
      CHARACTER*( * ) PXSZ
      CHARACTER*( * ) PYSZ
      LOGICAL CLEAR
      CHARACTER*( * ) COMMNT

*  Arguments Returned:
      INTEGER PIC0
      INTEGER PIC1
      INTEGER ZONE
      LOGICAL COLOUR
      LOGICAL CURSOR
      LOGICAL CLRBLK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NSGSP              ! Number of predefined SGS pens
      PARAMETER( NSGSP = 5 )

*  Local Variables:
      INTEGER COL                ! 1 if colour available on the device,
                                 ! 0 otherwise
      INTEGER CONID              ! GKS connection identifier
      REAL DEFX                  ! Default x size
      REAL DEFY                  ! Default y size
      INTEGER GSTAT              ! GKS status
      INTEGER NCOL               ! Number of colours available
      INTEGER NCPI               ! Number of predefined colour indices
      INTEGER TSTAT              ! Temporary status
      REAL X1, X2, Y1, Y2        ! Lower and upper limits of X and Y in
                                 ! current zone
      REAL XM, YM                ! X & Y size of current zone in metres
      REAL XSZ, YSZ              ! X & Y size of the plotting zone
      INTEGER WKID               ! Workstation identifier
      INTEGER WTYPE              ! GKS workstation type
      INTEGER ZONE0              ! SGS zone associated with picture
                                 ! current on entry to this routine

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a device to plot on and open AGI.
      IF ( CLEAR ) THEN
         CALL AGI_ASSOC( PDEV, 'WRITE', PIC0, STATUS )
      ELSE
         CALL AGI_ASSOC( PDEV, 'UPDATE', PIC0, STATUS )
      END IF

*  Open SGS, and get an indetifier for the SGS zone containing the
*  current picture.
      CALL AGS_ACTIV( STATUS )
      CALL AGS_NZONE( ZONE0, STATUS )

*  Check status. If error, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_GROPN_ERR1',
     :          'IRM_GROPN: Unable to open a graphic device properly.',
     :                 STATUS )
         GOTO 999
      END IF

*  Get the GKS workstation identifier for the given graphic device.
      CALL SGS_ICURW( WKID )

*  Get the GKS workstation type of the graphic device.
      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*  Inquire whether GKS has reported an error..
      CALL GKS_GSTAT( STATUS )

*  If error happened, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_GROPN_ERR2',
     :          'IRM_GROPN: Unable to obtain graphic workstation type.',
     :                 STATUS )
         GOTO 999
      END IF

*  Inquire colour facilities of the workstation.
      CALL GQCF( WTYPE, GSTAT, NCOL, COL, NCPI )

*  If colour is available, set colour flag.
      IF ( COL .EQ. 1 ) THEN
         COLOUR = .TRUE.

*  If colour is not available, set its flag to false.
      ELSE
         COLOUR = .FALSE.
      END IF

*  Inquire whether cursor is available on the device.
      CALL SGS_ICUAV( CURSOR )

*  Inquire whether the workstation is capable of clearing selected areas
*  of the display.
      CALL SGS_ISLER( CLRBLK )

*  Get the size of the current zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Set default size, considering the round-off error to make sure
*  they are within the given range.
      DEFX = 0.999999 * XM
      DEFY = 0.999999 * YM

*  Get the required size for the plot, allowing for rounding errors
*  introduced by type conversion in the parameter system.
      CALL PAR_GDR0R( PXSZ, DEFX, 0.005, XM, .FALSE., XSZ, STATUS )
      CALL PAR_GDR0R( PYSZ, DEFY, 0.005, YM, .FALSE., YSZ, STATUS )

*  Create and select a zone of the specified size within the current
*  zone.
      CALL SGS_ZSIZE( XSZ, YSZ, 'BL', ZONE, STATUS )
      CALL SGS_SELZ( ZONE, STATUS )

*  Release the original SGS zone.
      CALL SGS_RELZ( ZONE0 )

*  Save the current SGS zone in the AGI database as a 'FRAME' picture.
      CALL AGS_SZONE( 'FRAME', COMMNT, PIC1, STATUS )

*  If no error happen so far, make AUTOGRAPH use the whole of the
*  current SGS zone for plotting.
      IF ( STATUS .EQ. SAI__OK ) CALL SNX_AGWV

*  Re-instate the original current picture.
      IF ( STATUS .NE. SAI__OK ) THEN

*  In case error has happened, set status to non-error temporary so as
*  to let re-instate performs.
         TSTAT = STATUS
         STATUS = SAI__OK
         CALL AGI_SELP( PIC0, STATUS )
         STATUS = TSTAT
      ELSE
         CALL AGI_SELP( PIC0, STATUS )
      END IF

 999  CONTINUE

      END
