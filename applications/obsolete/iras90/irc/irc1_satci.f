      SUBROUTINE IRC1_SATCI( IDC, NVAL, SAMPLE, DETIND, PSI, THETA,
     :                       SOLONG, UTCS, STATUS )
*+
*  Name:
*     IRC1_SATCI

*  Purpose:
*     Returns satellite coordinates at a set of detector samples, with
*     no argument verification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_SATCI( IDC, NVAL, SAMPLE, DETIND, PSI, THETA, SOLONG,
*                      UTCS, STATUS )

*  Description:
*     This routine provides the functionality for routine IRC_SATCO.
*     No checks are made on the validity of the input arguments.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample number is
*        BAD, then the corresponding elements in the output arrays are
*        set BAD.
*     DETIND( NVAL ) = INTEGER (Given)
*        A list of detector indices.
*     PSI( NVAL ) = REAL (Returned)
*        An array holding the clock angle of the boresight at the
*        moment each sample specified in the input lists was taken
*        (radians).
*     THETA( NVAL ) = REAL (Returned)
*        An array holding the cone angle of the boresight at the moment
*        each sample specified in the input lists was taken (radians).
*     SOLONG( NVAL ) = REAL (Returned)
*        An array holding the solar longitude at the moment each sample
*        specified in the input lists was taken (radians), refered to
*        mean of date.
*     UTCS( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the UTCS at the moment each sample specified
*        in the input lists was taken (seconds).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The relationships between clock angle, cone angle, solar
*     longitude and UTCS are desribed in the ID/1 appendix, "Satellite
*     Coordinates".

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-AUG-1993 (DSB):
*        Orginal version.
*     {original_version_entry}

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
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Write)
*           HDS type of the DETAILS component.

*  Arguments Given:
      INTEGER IDC
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )

*  Arguments Returned:
      REAL PSI( NVAL )
      REAL THETA( NVAL )
      REAL SOLONG( NVAL )
      DOUBLE PRECISION UTCS( NVAL )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_SATSB( IDC, NVAL, SAMPLE, DETIND, PSI, THETA,
     :                       SOLONG, UTCS, STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_SATCI_ERR1',
     :                 'IRC1_SATCI does not yet support ^T data',
     :                 STATUS )
      END IF

      END
