      SUBROUTINE IDINVISIBLE( STATUS )
*+
*  Name:
*     IDINVISIBLE

*  Purpose:
*     Makes memory planes of an image-display device invisible.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDINVISIBLE( STATUS )

*  Description:
*     This routine makes invisible nominated planes of an IDI-supported
*     image display, such as X-windows.

*  Usage:
*     idinvisible [planes] [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose memory plane is to
*        be made invisible.  The name of the base plane should be given
*        even if an overlay plane is to be made invisible.
*        [Current image display]
*     PLANES() = _INTEGER (Read)
*        The numbers of the memory planes to be made invisible.  All
*        unspecified planes become visible.  If it is null the base
*        (image) memory is made invisible.  The base memory is 0 and
*        overlays are numbered consecutively from 1.  For an Ikon the
*        only overlay plane is 1.
*        [0]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  On some devices making a memory invisible may have the effect
*     of making other memories visible.
*     -  On the Ikon the visibilities are set to visible on start up,
*     so that any set up change introduced by an application calling
*     GKS are not lost, therefore all planes to be made invisible must
*     be given in one invocation.

*  Examples:
*     idinvisible [0,1]
*        Makes only planes 0 and 1 invisible on the current image
*        display device.
*     idinvisible device=xwindows
*        Makes the base plane invisible on the xwindows device.
*     idinvisible 1 ikon
*        Makes the first overlay plane invisible on the Ikon device.

*  Algorithm:
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Obtain the number of memory planes.
*     -  Obtain the numbers of planes to be made invisible.
*     -  Make all the memories visible.
*     -  Make the memories invisible.
*     -  Annul the device.

*  Related Applications:
*     KAPPA: IDCLEAR, IDSTATE.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 July 23 (MJC):
*        Original version.
*     1992 February 21 (MJC):
*        Made all the memories visible first to cope with the changed
*        behaviour of the IDI implementation.
*     1992 March 26 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'PAR_ERR'          ! Parameter-system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )

*  Local Variables:
      INTEGER
     :  DID,                     ! Display identifier
     :  IDSTAT,                  ! IDI status
     :  ITTDEP( NMEMAX ),        ! ITT depths of available memories
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
     :  MEMID( NMEMAX ),         ! Identifiers of memories to be made
                                 ! invisible
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON                   ! Configuration mode

      INTEGER
     :  NCONF,                   ! Configuration number
     :  NMEM,                    ! Number of image memories
     :  NVAL,                    ! Number of memories numbers returned
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YSIZ( NMEMAX )           ! Y sizes of available memories

      LOGICAL                    ! True if:
     :  DEVCAN                   ! Device is to be cancelled on exit

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the image-display device.
*    ================================

*    Assume that the application will work and the chosen device is
*    to be annulled rather than cancelled upon completion.

      DEVCAN = .FALSE.

*    Just want to rotate what is already there.  It may have
*    been produced by GKS without updating the IDI workstation
*    status table.  Without IDI context, IDI would normally
*    reset the device.  The following call prevents the reset.

      CALL IDI_CLRFG( 1 )

*    Open IDI for a device obtained through the parameter system.

      CALL IDI_ASSOC( 'DEVICE', 'READ', DID, STATUS )

      IF ( STATUS .NE. IDI__OK ) THEN

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Make sure we have selected the base configuration.
*    ==================================================

      NCONF = 0
      CALL IIDSEL( DID, NCONF, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDSEL_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Find the number of image (MEMTYP=1) planes available for the
*    image-display device.  Only NMEM and MEMIDS are required.

      MEMTYP = 1
      CALL IIDQDC( DID, NCONF, MEMTYP, NMEMAX, MODCON, MEMIDS, XSIZ,
     :             YSIZ, MEMDEP, ITTDEP, NMEM, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQDC_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Get the memories required within the allowed range.
*    ===================================================

*    Default is the base plane.  There is no guarantee that the chosen
*    planes are not obscured by another overlay plane.

      NVAL = 1
      CALL PAR_GDRVI( 'PLANES', NMEM, 0, NMEM-1, MEMID, NVAL, STATUS )

*    A null status indicates that the base image plane is to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         MEMID( 1 ) = 0
         NVAL = 1
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 999
      END IF

*    Set all the memories to be visible.
*    ===================================

*    Note there is no checking that the ITT and VLUT are suitable.

      CALL IIMSMV( DID, MEMIDS, NMEM, 1, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSMV_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Make the nominated planes invisible.
*    ====================================

      CALL IIMSMV( DID, MEMID, NVAL, 0, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSMV_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Closedown sequence.
*    ===================
*
*    Errors, either obtaining the device or because the device does not
*    support the required capabilities, fall to this point.

 999  CONTINUE

      IF ( DEVCAN ) THEN

*       Close down IDI using the parameter system.

         CALL IDI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL IDI_ANNUL( DID, STATUS )
      END IF

      END
