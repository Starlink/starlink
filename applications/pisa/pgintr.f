      SUBROUTINE PGINTR( V1, V2, NULL, ID0, ID1, ID2, STATUS )
*+
*  Name:
*     PGINTR

*  Purpose:
*     Accesses a device, starts up AGI, activates PGPLOT and set the
*     plot line width. After accessing the current picture the routine
*     also sets up two sub-pictures to contain the data plots.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGINTR( NULL, ID0, ID1, ID2, STATUS )

*  Description:
*     This routine calls AGI_ASSOC to access a device. It activates
*     PGPLOT and sets up a base viewport. After establishing the base
*     viewport two AGI pictures are scaled and created within it. These
*     pictures will contain the data plots. The routine sets the
*     line width for plotting and returns with parameter NULL set if
*     a device is not activated. The routine PGASK is also called
*     to inhibit prompting by PGPLOT.

*  Arguments:
*     V1( 4 ) = REAL (Given)
*        Scaling factors to ensure room for fit data plot borders.
*     V2( 4 ) = REAL (Given)
*        Scaling factors to ensure room for fit data plot borders.
*     NULL = LOGICAL (Returned)
*        This parameter signifies if a device has been activated or not.
*     ID0 = INTEGER (Returned)
*        The AGI current picture identifier.
*     ID1 = INTEGER (Returned)
*        The AGI picture identifier, for the picture created within the
*        base viewport.
*     ID2 = INTEGER (Returned)
*        The AGI picture identifier, for the picture created within the
*        base viewport.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The windows are set up using scaling factors as AGP_NVIEW does not
*     scale each axis independently, the leads to a mismatch in the
*     size of the radius axis between the plots.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (PDRAPER):
*        Original version.
*     22-NOV-1990 (PDRAPER):
*        Added AGI_BEGIN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants

*  Arguments Given:
      REAL V1( 4 )
      REAL V2( 4 )

*  Arguments Returned:
      LOGICAL NULL
      INTEGER ID0
      INTEGER ID1
      INTEGER ID2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ILW                ! plot line width
      REAL WX1, WX2, WY1, WY2    ! world coordinates for base viewport
      REAL WWX1, WWX2, WWY1, WWY2 ! world coordinates for new viewports
                                  ! ( pictures ) within base viewport
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open AGI and access device parameter
      CALL AGI_ASSOC( 'DEVICE', 'WRITE', ID0, STATUS )
      NULL = .FALSE.
      IF ( STATUS .EQ. PAR__NULL ) THEN

*  Have request to use no device set flag and reset status
         NULL = .TRUE.
         STATUS = SAI__OK
      END IF
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. NULL ) THEN

*  Begin an AGI scope
         CALL AGI_BEGIN

*  See if a picture of type 'PISA_FITPLOT', already exists within
*  the current picture
         CALL AGI_SELP( ID0, STATUS )
         CALL AGI_RCS( 'PISA_FITPLOT', ID0, ID1, STATUS )

*  Test status to see if picture exists, if not annul status and
*  create new picture.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  Get the world coordinates of the base viewport
            CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )

*  Scale the world coordinates to leave a border around the picture
            WWX1 = WX1 + ( WX2 - WX1 ) * V1(1)
            WWX2 = WX1 + ( WX2 - WX1 ) * V1(2)
            WWY1 = WY1 + ( WY2 - WY1 ) * V1(3)
            WWY2 = WY1 + ( WY2 - WY1 ) * V1(4)

*  Create a picture with these world coordinates
            CALL AGI_NUPIC( WWX1, WWX2, WWY1, WWY2, 'PISA_FITPLOT',
     :                      'FIT', WWX1, WWX2, WWY1, WWY2, ID1, STATUS )
         END IF

*  Check status at this point to stop spurious annuling
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL AGI_SELP( ID0, STATUS )
            CALL AGI_RCS( 'PISA_RESPLOT', ID0, ID2, STATUS )

*  Test status to see if picture exists, if not annul status and
*  create new picture.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  Scale the world coordinates to leave a border around the picture
               WWX1 = WX1 + ( WX2 - WX1 ) * V2(1)
               WWX2 = WX1 + ( WX2 - WX1 ) * V2(2)
               WWY1 = WY1 + ( WY2 - WY1 ) * V2(3)
               WWY2 = WY1 + ( WY2 - WY1 ) * V2(4)

*  Create the second picture in the top partition.
               CALL AGI_NUPIC( WWX1, WWX2, WWY1, WWY2, 'PISA_RESPLOT',
     :                         'RESIDS', WWX1, WWX2, WWY1, WWY2, ID2,
     :                          STATUS )

            END IF
         END IF
*  Activate the device and set up the base viewport
         CALL AGI_SELP( ID0, STATUS)
         CALL AGP_ACTIV( STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )

*  Get the line width
         CALL PAR_GET0I( 'LINEW', ILW, STATUS )

*  Set it up
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL PGSLW( ILW )
         END IF

*  Inhibit PGPLOT prompting
         CALL PGASK( .FALSE. )
      END IF
      CONTINUE
      END
* $Id$
