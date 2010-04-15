      SUBROUTINE IRC1_PNTSB( IDC, STATUS )
*+
*  Name:
*     IRC1_PNTSB

*  Purpose:
*     Initialise pointing data for a SURVEY_BSIGHT CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_PNTSB( IDC, STATUS )

*  Description:
*     Firstly, the required values are obtained form the DETAILS
*     structure within the CRDD_INFO structure. A lower level routine is
*     called to produce linear fits of solar longtitude and clock angle
*     against time since sample number 1. The solar longitude values are
*     refered to the mean equinox and ecliptic of date before obtaining
*     the fit. The gardients and intercepts of these fits are stored in
*     common.
*
*     A matrix is calculated which rotates sky positions (given in
*     Cartesian form) from the equatorial (B1950, FK4) system, to
*     the ecliptic (mean of date) system. This matrix is stored in
*     common.
*
*     Finally, a flag is set in common to indicate that pointing
*     information is available for this CRDD file.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1991 (DSB):
*        Original version.
*     25-AUG-1993 (DSB):
*        Added assignment to CCM_UTCS1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_CRDDL( IRC__MAX ) = CHARACTER (Read)
*           HDS locator for CRDD_INFO component.
*        CCM_POINT( IRC__MAX ) = LOGICAL (Read and Write)
*           True if pointing information has been generated.
*        CCM_CAGRD( IRC__MAX ) = REAL (Read and Write)
*           The rate of change of clock angle (PSI) with time, in
*           radians per second (assumed constant).
*        CCM_CAZER( IRC__MAX ) = REAL (Read and Write)
*           The clock angle (PSI) at sample number 1, in radians.
*        CCM_SLGRD( IRC__MAX ) = REAL (Read and Write)
*           The rate of change of solar longitude with time, in radians
*           per second (assumed constant).
*        CCM_SLZER( IRC__MAX ) = REAL (Read and Write)
*           The solar longitude at sample number 1, in radians.
*        CCM_THETA( IRC__MAX ) = REAL (Read and Write)
*           The cone angle at which the scan was taken, in radians
*           (assumed constant).
*        CCM_RMAT( 9, IRC__MAX ) = DOUBLE PRECISION (Read and Write)
*           Rotation matrices which rotate equatorial (1950) coordinates
*           expressed in Cartesian form, to ecliptic coordinates (of
*           date), also expressed in Cartesian form.
*        CCM_UTCS1( IRC__MAX ) = DOUBLE PRECISION (Write)
*           The UTCS of detector sample number 1, obtained from DETAILS
*           component BASE_UTCS.

*  Arguments Given:
      INTEGER IDC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BEPOCH    ! Besselian epoch at data sample no. 1.
      INTEGER   BPOSNS           ! No. of boresight samples stored in
                                 ! the CRDD file.
      CHARACTER CLOC*(DAT__SZLOC)! HDS locator to a single cell in the
                                 ! cone angle array THETA.
      CHARACTER DLOC*(DAT__SZLOC)! HDS locator to the DETAILS structure.
      INTEGER   IPPSI            ! Pointer to the mapped PSI array.
      INTEGER   IPSUN            ! Pointer to the mapped LAMBDA_SUN array.
      INTEGER   IPUTC            ! Pointer to the mapped UTCS_OFFSET array.
      INTEGER   IPW              ! Pointer to the temporary work array.
      DOUBLE PRECISION MJD       ! Modified Julian Date at sample number
                                 ! 1. Obtained from the DETAILS
                                 ! structure.
      INTEGER   NEL              ! No. of elements mapped from the
                                 ! arrays of satellite coordinates.
      DOUBLE PRECISION R1(9)     ! Matrix describing rotation  from
                                 ! equatorial (B1950, FK4) coordinates
                                 ! to equatorial (mean of date, FK4)
                                 ! coordinates.
      DOUBLE PRECISION R2(9)     ! Matrix describing rotation  from
                                 ! equatorial to ecliptic coordinates.
      DOUBLE PRECISION SLA_EPB   ! SLALIB function converting Modified
                                 ! Julian Date to Besselian Epoch.
      CHARACTER TLOC*(DAT__SZLOC)! HDS locator to the cone angle array
                                 ! THETA.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If pointing information has already been set up, return immediately.
      IF( CCM_POINT( IDC ) ) GO TO 999

*  Get a locator to the DETAILS structure. The name of the DETAILS
*  component is given in the symbolic constant IRC__DNAME.
      CALL DAT_FIND( CCM_CRDDL( IDC ), IRC__DNAME, DLOC, STATUS )

*  Get the value of the scalar component BASE_UTCS and store in common.
      CALL CMP_GET0D( DLOC, 'BASE_UTCS', CCM_UTCS1( IDC ), STATUS )

*  Get the values of the scalar components BORE_POSNS and BASE_MJD.
      CALL CMP_GET0I( DLOC, 'BORE_POSNS', BPOSNS, STATUS )
      CALL CMP_GET0D( DLOC, 'BASE_MJD', MJD, STATUS )

*  Get the value of the cone angle at the centre boresight sample.
      CALL DAT_FIND( DLOC, 'THETA', TLOC, STATUS )
      CALL DAT_CELL( TLOC, 1, BPOSNS/2, CLOC, STATUS )
      CALL DAT_GET0R( CLOC, CCM_THETA( IDC ), STATUS )
      CALL DAT_ANNUL( CLOC, STATUS )
      CALL DAT_ANNUL( TLOC, STATUS )

*  Map the vectors UTCS_OFFSET, LAMBDA_SUN and PSI
      CALL CMP_MAPV( DLOC, 'UTCS_OFFSET', '_REAL', 'READ', IPUTC, NEL,
     :               STATUS )
      IF( STATUS. EQ. SAI__OK .AND. NEL .NE. BPOSNS )
     :                                    STATUS = IRC__SBER1

      CALL CMP_MAPV( DLOC, 'LAMBDA_SUN', '_REAL', 'READ', IPSUN, NEL,
     :               STATUS )
      IF( STATUS. EQ. SAI__OK .AND. NEL .NE. BPOSNS )
     :                                    STATUS = IRC__SBER1

      CALL CMP_MAPV( DLOC, 'PSI', '_REAL', 'READ', IPPSI, NEL, STATUS )
      IF( STATUS. EQ. SAI__OK .AND. NEL .NE. BPOSNS )
     :                                    STATUS = IRC__SBER1

*  If any error occured getting these arrays, give an error report.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC1_PNTSB_ERR1',
     :         'IRC1_PNTSB: Unable to access CRDD information', STATUS )
      END IF

*  Get a temporary array to use as workspace in IRC1_SBFIT.
      CALL PSX_CALLOC( 3*BPOSNS, '_DOUBLE', IPW, STATUS )

*  Call a lower level routine to produce linear fits to the solar
*  longitude (of date) and clock angle, and store the corresponding
*  gradients and intercepts in common.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL IRC1_SBFIT( IDC, BPOSNS, MJD, %VAL(IPUTC), %VAL(IPSUN),
     :                    %VAL(IPPSI), %VAL(IPW), STATUS )
      END IF

*  Release the temporary array.
      CALL PSX_FREE( IPW, STATUS )

*  Unmap the vectors UTCS_OFFSET, LAMBDA_SUN and PSI.
      CALL CMP_UNMAP( DLOC, 'UTCS_OFFSET', STATUS )
      CALL CMP_UNMAP( DLOC, 'LAMBDA_SUN', STATUS )
      CALL CMP_UNMAP( DLOC, 'PSI', STATUS )

*  Annul the locator to the DETAILS structure.
      CALL DAT_ANNUL( DLOC, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now calculate the matrix which rotates a 3-vector representing an
*  equatorial (B1950, FK4) position to a 3-vector representing an
*  ecliptic (mean of date) position. The matrix is stored in common.
      BEPOCH = SLA_EPB( MJD )
      CALL SLA_PREBN( 1950.0D0, BEPOCH, R1 )
      CALL SLA_ECMAT( MJD, R2 )
      CALL SLA_DMXM( R2, R1, CCM_RMAT( 1, IDC ) )

*  Indicate that pointing information is now available for this CRDD
*  file.
      CCM_POINT( IDC ) = .TRUE.

 999  CONTINUE

      END
