      SUBROUTINE IRA_DEMO2( STATUS )
*+
*  Name:
*     IRA_DEMO2

*  Purpose:
*     Demonstration program for the IRA package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRA_DEMO2( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program accepts an NDF as input containing an astrometry
*     structure. The program transforms coordinate data given by the
*     user in reponse to parameter prompts, from sky coordinate to
*     image coordinates, or vica-versa.

*  ADAM Parameters:
*     FORWRD = _LOGICAL (Read)
*        True if a forward mapping is required ( from image coordinates
*        to sky coordinates). False is an inverse mapping is required.
*     SCS = LITERAL (Read)
*        The Sky Coordinate System in which to read sky coordinates from
*        the user, and write sky coordinates to the screen. This need
*        not be the same as the SCS stored in the astrometry structure.
*     XNAME = LITERAL (Read)
*        The name of the NDF extension in which to look for the
*        astrometry structure.
*     ASNAME = LITERAL (Read)
*        The name of the astrometry structure to be used. This must be a
*        component of the NDF extension specified by XNAME.
*     IN = NDF (Read)
*        The input NDF.
*     A = LITERAL (Read)
*        A string containing the sky longitude value at a
*        position which is to be transformed to image coordinates. See
*        IRA_GETCO.
*     B = LITERAL (Read)
*        A string containing the sky latitude value.
*     XIMAGE = _REAL (Read)
*        The value of the first (X) image coordinate at a position
*        which is to be transformed to sky coordinates.
*     YIMAGE = _REAL (Read)
*        The value of the second (Y) image coordinate at a position
*        which is to be transformed to sky coordinates.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1991 (DSB):
*        Original version.
*     2-MAY-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER 	ASNAME*(DAT__SZNAM)! Astrometry structure name.
      CHARACTER 	ATEXT*(IRA__SZFSC)! Formated first sky coordinate.
      CHARACTER 	BTEXT*(IRA__SZFSC)! Formated second sky coordinate.
      DOUBLE PRECISION 	EPOCH    ! Epoch stored in the astrometry
                                 ! structure.
      LOGICAL  		FORWRD   ! True if forward mapping required.
      INTEGER  		IDA      ! IRA identifier for astrometry.
      DOUBLE PRECISION  IN1      ! First input coordinate value
				 ! (longitude or X.)
      DOUBLE PRECISION  IN2      ! Second input coordinate value
                                 ! (latitude or Y.)
      INTEGER  		INDF     ! NDF identifier for input NDF.
      INTEGER  		LBND( 2 )! Lower pixel bounds of NDF.
      INTEGER  		NDIM     ! No. of dimensions in NDF.
      DOUBLE PRECISION  OUT1     ! First output coordinate value
				 ! (longitude or X.)
      DOUBLE PRECISION  OUT2     ! Second output coordinate value
                                 ! (latitude or Y.)
      CHARACTER 	SCS*(IRA__SZSCS)! SCS selected by user.
      INTEGER  		UBND( 2 )! Upper pixel bounds of NDF.
      CHARACTER 	XNAME*(DAT__SZNAM)! NDF extension name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRA package.
      CALL IRA_INIT( STATUS )

*  Get an NDF identifier for the input image, and get image bounds.
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Get the name of the NDF extension containing the astrometry
*  structure.
      CALL PAR_GET0C( 'XNAME', XNAME, STATUS )

*  Get the name of the astrometry structure.
      CALL PAR_GET0C( 'ASNAME', ASNAME, STATUS )

*  Set the current location for IRA astrometry structures.
      CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Import the astrometry structure into the IRA system.
      CALL IRA_IMPRT( INDF, IDA, STATUS )

*  See if a forward or inverse transformation is required.
      CALL PAR_GET0L( 'FORWRD', FORWRD, STATUS )

*  Get the sky coordinate system in which the user wants to work. This
*  can be different to the SCS used by the projection stored in the
*  astrometry structure. The defualt is the SCS used by the projection.
      CALL IRA_SCSEP( IDA, SCS, EPOCH, STATUS )
      CALL IRA_GTSCS( 'SCS', .TRUE., SCS, STATUS )

*  Set the default input position to the middle of the image.
      IN1 = 0.5*( LBND(1) + UBND(1) - 1.0 )
      IN2 = 0.5*( LBND(2) + UBND(2) - 1.0 )
      IF( .NOT. FORWRD ) CALL IRA_TRANS( 1, IN1, IN2, .TRUE., SCS, IDA,
     :                                   IN1, IN2, STATUS )

*  Loop round until the user gives a null or error value.
      DO WHILE( STATUS .EQ. SAI__OK )
         CALL MSG_BLANK( STATUS )

*  If a forward mapping is required, get a pair of image coordinates
*  from the user.
         IF( FORWRD ) THEN
            CALL PAR_GET0D( 'XIMAGE', IN1, STATUS )
            CALL PAR_GET0D( 'YIMAGE', IN2, STATUS )

*  If an inverse mapping is required, get a pair of sky coordinates.
         ELSE
            CALL IRA_GETCO( 'A', 'B', '.', SCS, .FALSE.,IN1, IN2,
     :                       STATUS )
         END IF

*  Transform the data point.
         CALL IRA_TRANS( 1, IN1, IN2, FORWRD, SCS, IDA, OUT1, OUT2,
     :                      STATUS )

*  Display the transformed point, and cancel the ADAM parameters.
         CALL MSG_BLANK( STATUS )

         IF( FORWRD ) THEN
            CALL IRA_DTOC( OUT1, OUT2, SCS, 1, ATEXT, BTEXT, STATUS )
            CALL MSG_OUT( 'IRA_DEMO2_MSG1', '   '//ATEXT//BTEXT,
     :                     STATUS )
            CALL PAR_CANCL( 'XIMAGE', STATUS )
            CALL PAR_CANCL( 'YIMAGE', STATUS )

         ELSE
            CALL MSG_SETD( 'X', OUT1 )
            CALL MSG_SETD( 'Y', OUT2 )
            CALL MSG_OUT( 'IRA_DEMO2_MSG2',
     :                    '   X image = ^X   Y image = ^Y ', STATUS )
            CALL PAR_CANCL( 'A', STATUS )
            CALL PAR_CANCL( 'B', STATUS )

         END IF

      END DO

*  Close IRA.
      CALL IRA_CLOSE( STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )

      END
