      SUBROUTINE LUTFLIP( STATUS )
*+
*  Name:
*     LUTFLIP

*  Purpose:
*     Flips the colour table of an image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTFLIP( STATUS )

*  Description:
*     This routine `flips' the colour table of a nominated plane of
*     an IDI-supported image display, such as X-windows.  The flip
*     reverses the order of the colours, so that the first colour
*     becomes the last and vice versa, etc.

*  Usage:
*     lutflip [device] [plane]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose colour table is to
*        be flipped.  The name of the base plane should be given even if
*        the overlay colour table is to be flipped.
*        [Current image display]
*     PLANE = _INTEGER (Read)
*        The number of the memory plane whose colour table is to be
*        flipped.  If it is null the base (image) memory's colour table
*        is reversed.  The base memory is 0 and overlays are numbered
*        consecutively from 1.  For an Ikon the only overlay plane is 1.
*        PLANE is only permitted to have a value in the range 0 to the
*        number of planes minus one. [0]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     lutflip
*        This reverses the colour table on the current image-display
*        device.
*     lutflip xwindows
*        This reverses the colour table on the xwindows device.
*     lutflip ikon 1
*        This flips the colour table on the Ikon overlay plane.

*  Notes:
*     -  Only Ikons and X-windows are supported.
*     -  Reserved pens are not flipped.

*  Algorithm:
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Obtain the number of memory planes and the maximum number of
*        LUT entries.
*     -  Obtain the plane number whose LUT is to be flipped.
*     -  Read the input LUT.
*     -  Flip the values
*     -  Write the flipped LUT.
*     -  Annul the device.

*  Related Applications:
*     KAPPA: CRELUT, LUTHILITE, LUTROT, LUTTWEAK.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Apr 13 (MJC):
*        Original version.
*     1991 April 10 (MJC):
*        Modified for colour-table management, specifically the reserved
*        pens are not flipped.
*     1991 June 5 (MJC):
*        Altered for new, X-windows, IDI.  Capability 14 is 2**n rather
*        than n.
*     1992 March 26 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 August 5 (MJC):
*        Used new capability 18 to obtain the true number of
*        colour-table entries, which may not be a power of 2, as
*        returned by capability 14.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table-management constants
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'PAR_ERR'          ! Parameter-system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXLUTE
      PARAMETER ( MXLUTE = 1024 )! Maximum number of LUT pens
      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )

*  Local Variables:

      INTEGER
     :  ACTVAL,                  ! Number of values returned
     :  DID,                     ! Display identifier
     :  I, J,                    ! Loop counters
     :  IDSTAT,                  ! IDI status
     :  ITTDEP( NMEMAX ),        ! ITT depths of available memories
     :  JF,                      ! Complement loop counter to J
     :  LUTCAP,                  ! Max. depth of LUTS capability code
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
     :  MEMID,                   ! Memory identifier whose LUT is to be
                                 ! rotated
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON                   ! Configuration mode

      INTEGER
     :  NCONF,                   ! Configuration number
     :  NINTS( 1 ),              ! Number of LUT entries
     :  NLUTE,                   ! Actual number of LUT entries to be
                                 ! reversed.
     :  NMEM,                    ! Number of image memories
     :  NVAL,                    ! Number of values expected
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YSIZ( NMEMAX )           ! Y sizes of available memories

      REAL
     :  DUMMY( 3 ),              ! Dummy array to store the RGB
                                 ! intensities when flipping the LUT
     :  VLUT( 3, MXLUTE )        ! Video lookup table

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
*    ============================================================

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

*    Get the memory required within the allowed range.
*    =================================================

      CALL PAR_GDR0I( 'PLANE', 0, 0, NMEM-1, .FALSE., MEMID, STATUS )

*    A null status indicates that the base image plane is to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         MEMID = 0
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 999
      END IF

*    Select the current lookup table for the specified memory plane.
*    ===============================================================

*    The use of VLUT = memory identifier is arbitrary.  It does work
*    for GKS, and will be changed once an IDI problem is resolved.

      CALL IIMSLT( DID, MEMID, MEMID, 0, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSLT_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Find the maximum number of lookup-table entries.
*    ================================================

      LUTCAP = 18
      NVAL = 1
      CALL IIDQCI( DID, LUTCAP, NVAL, NINTS, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .FALSE.
         GOTO 999
      END IF

*    Read the current LUT, only storing what is possible.
*    ====================================================

*    The buffer is generous as most devices have 256 levels.
*    Colour-table entries start at 0 if there were no reserved pens,
*    therefore there is no plus-one term.

      NLUTE = MIN( NINTS( 1 ), MXLUTE ) - CTM__RSVPN
      CALL IILRLT( DID, MEMID, CTM__RSVPN, NLUTE, VLUT, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IILRLT_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Flip the LUT.
*    =============

*    Flip the LUT by copying the half of the red, green and blue
*    components via a dummy array.  Allow for the reserved pens at the
*    start of the colour-table read from the device.

      DO  J = 1, NLUTE / 2
         JF = NLUTE - J + 1
         DO  I = 1, 3
            DUMMY( I ) = VLUT( I, J )
            VLUT( I, J ) = VLUT( I, JF )
            VLUT( I, JF ) = DUMMY( I )
         END DO
      END DO

*    Write the reversed lookup table to the memory.
*    ==============================================

      CALL IILWLT( DID, MEMID, CTM__RSVPN, NLUTE, VLUT, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IILWLT_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Closedown sequence.
*    ===================

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
