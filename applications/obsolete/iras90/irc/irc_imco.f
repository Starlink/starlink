      SUBROUTINE IRC_IMCO( IDC, SAMP, DETIN, IDA, INIT, C, STATUS )
*+
*  Name:
*     IRC_IMCO

*  Purpose:
*     Locate a given sample within an image frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_IMCO( IDC, SAMP, DETIN, IDA, INIT, C, STATUS )

*  Description:
*     This routine finds the position and orientation of a given sample
*     within an image frame defined by an IRA identifier (see ID2). The
*     information is returned in the form of six values giving the
*     coefficients of a linear transformation which maps an offset from
*     the sample centre given in focal plane coordinates, to the
*     corresponding pixel coordinates within the image frame defined by
*     the supplied IRA identifier. Specifically, the returned vector C
*     holds values C(1) to C(6) where:
*
*        X = C(1) + C(2)*DZfp + C(3)*DYfp
*
*        Y = C(4) + C(5)*DZfp + C(6)*Dyfp
*
*     In these expressions, DZfp and DYfp are displacements (in
*     radians, NOT arc-minutes!) parallel to the focal plane Z and Y
*     axes, such that (DZfp,DYfp)=(0,0) corresponds to the detector
*     centre. (X,Y) are the image coordinates which correspond to the
*     position with focal plane offset (DZfp,DYfp). Thus C(1) and C(4)
*     are the image coordinates of the detector centre, and the
*     anticlockwise angle from the image Y axis to the focal plane Y
*     axis is ATAN2( C(5), C(6) ), or equivalently ATAN2( -C(3),C(2) ).
*     The tendency for pixel size to vary across a large image is taken
*     into account in the returned coefficients.
*
*     The use of a linear transformation is only valid for small
*     displacements away from the detector centre. This routine is
*     intended for situations in which the displacement is less than
*     an arc-degree.
*
*     Various forms of initialisation need to be performed each time a
*     new combination of CRDD file (as specified by IDC) and astrometry
*     information (as specified by IDA) is used. Setting the argument
*     INIT to .TRUE. causes this initialisation to be done, before
*     going on to calculate the returned information. On subsequent
*     calls to this routine INIT should set to .FALSE., until either a
*     new CRDD file or new astrometry information is used. Note, the
*     initialisation procedure is also performed (even if INIT is
*     false) if the supplied detector index is different to the value
*     supplied for the previous call to this routine. For this reason,
*     applications should use this routine to process all samples from
*     one detector before moving on to do another detector. The
*     alternative approach (processing the same sample from all
*     detectors before moving on to do another sample) would cause much
*     time to be wasted doing unnecessary initialisations.
*
*     ID2 should be consulted for information about setting up
*     astrometry information.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP = REAL (Given)
*        The fractional sample number. If this has the Starlink "BAD"
*        value (VAL__BADR) then the returned coefficients are all set to
*        the bad value.
*     DETIN = INTEGER (Given)
*        The detector index to which SAMP refers.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information which defines
*        the mapping from sky coordinates to image coordinates (see
*        ID2).
*     INIT = LOGICAL (Given)
*        INIT should be set true on the first call to this routine, and
*        false on subsequent calls. If either a new CRDD file, or new
*        astrometry information is supplied, then INIT should be set
*        true again for one call to this routine.
*     C( 6 ) = REAL (Returned)
*        The six coefficients defining the linear transformation from
*        focal plane offsets (in radians) to image coordinates (in
*        pixels).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     11-NOV-1991 (DSB):
*        Original version.
*     13-NOV-1991 (DSB):
*        Returned information changed to take the form of a linear
*        transformation.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC
      REAL    SAMP
      INTEGER DETIN
      INTEGER IDA
      LOGICAL INIT

*  Arguments Returned:
      REAL    C( 6 )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_IMCO_ERR1',
     :                 'IRC_IMCO: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the detector index is within the bounds of the NDF
*  second dimension.
      IF( DETIN .LT. CCM_DLOW( IDC ) .OR.
     :    DETIN .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIN )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_IMCO_ERR2',
     :        'IRC_IMCO: Detector index ^D (DETIN1) is '//
     :        'out of bounds [^DL,^DH]', STATUS )
         GO TO 999
      END IF

*  Call an internal routine to do the work.
      CALL IRC1_IMCOI( IDC, SAMP, DETIN, IDA, INIT, C, STATUS )

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_IMCO_ERR3',
     :    'IRC_IMCO: Unable to locate a CRDD sample within a 2D image ',
     :                 STATUS )
      END IF


      END
