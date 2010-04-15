      SUBROUTINE IRC1_SATSB( IDC, NVAL, SAMPLE, DETIND, PSI, THETA,
     :                       SOLONG, UTCS, STATUS )
*+
*  Name:
*     IRC1_SATSB

*  Purpose:
*     Returns satellite coordinates at a set of detector samples,
*     assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_SATSB( IDC, NVAL, SAMPLE, DETIND, PSI, THETA, SOLONG,
*                      UTCS, STATUS )

*  Description:
*     Straight lines are fitted to the solar longitude and the clock
*     angle (PSI) samples stored in the DETAILS structure contained in
*     the CRDD file. These fits are evaluated at the times of the
*     required samples. Cone angle (THETA) is assumed constant
*     throughout the scan. See figure III.B.7 in the IRAS Catalogues
*     and Atlases Explanatory Supplement for definitions of THETA and
*     PSI. (Note, this routine uses PSI, not PHI, as the clock angle.
*     PSI = 2.PI - PHI ).
*
*     If this is the first time pointing information has been requested
*     from the given CRDD file, then the linear fits are found and the
*     gradients and intercepts stored in common. Also, the matrix which
*     rotates 3-vectors representing Cartesian equatorial (1950)
*     positions to Cartesian ecliptic (of date) positions is calculated
*     and stored in common. Later calls to this routine use the stored
*     information rather than calculating it all again.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample numebr is
*        BAD, then the corresponding elements of the output arrays are
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

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

*  Local Variables:
      INTEGER IVAL               ! Sample counter.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each required position.
      DO IVAL = 1, NVAL

*  Get the satellite coordinates and solar longitude at this sample.
         CALL IRC1_SCOSB( IDC, SAMPLE( IVAL ), PSI( IVAL ),
     :                    THETA( IVAL ), SOLONG( IVAL ), UTCS( IVAL ),
     :                    STATUS )

*  Do the next required position.
      END DO

      END
