      SUBROUTINE IRA_SCNAM( SCS, NC, DESCR, LD, ABBREV, LA, STATUS )
*+
*  Name:
*     IRA_SCNAM

*  Purpose:
*     Get full name and abbreviation of a sky coordinate.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_SCNAM( SCS, NC, DESCR, LD, ABBREV, LA, STATUS )

*  Description:
*     A full description of the name of the longitude or latitude axis
*     of the given sky coordinate system is returned, together with an
*     abbreviation of the name. See the "Notes" section below for a
*     list of the values returned.

*  Arguments:
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system. Any unambiguous abbreviation will
*        do.
*     NC = INTEGER (Given)
*        The axis required; 1 for the longitude axis, 2 for the latitude
*        axis.
*     DESCR = CHARACTER * ( * ) (Returned)
*        A full description of the axis name (see "Notes" below). This
*        should have a declared length of IRA__SZSCD.
*     LD = INTEGER (Returned)
*        No. of used characters in DESCR.
*     ABBREV = CHARACTER * ( * ) (Returned)
*        An abbreviation for the axis name (see "Notes" below). This
*        should have a declared length of IRA__SZSCA.
*     LA = INTEGER (Returned)
*        No. of used characters in ABBREV.
*     STATUS = INTEGER (Given and Returned)
*        Status value.

*   Notes:
*     -  For equatorial coordinates, the descriptions are "Right
*     Ascension (epoch)" and "Declination (epoch)", and the
*     abbreviations are "RA" and "DEC".
*
*     -  For galactic coordinates, the descriptions are "Galactic
*     Longitude" and "Galactic Latitude", and the abbreviations are
*     "l" and "b".
*
*     -  For ecliptic coordinates, the descriptions are "Ecliptic
*     Longitude (epoch)" and "Ecliptic Latitude (epoch)", and the
*     abbreviations are "Lambda" and "Beta".

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     2-APR-1992 (DSB):
*        Epochs included in full names.
*     12-NOV-1992 (DSB):
*        New abbreviations for ECLIPTIC and GALACTIC coords described.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER SCS*(*)
      INTEGER   NC

*  Arguments Returned:
      CHARACTER DESCR*(*)
      INTEGER   LD
      CHARACTER ABBREV*(*)
      INTEGER   LA

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Verify the argument NC.
      IF( NC .NE. 1 .AND. NC .NE. 2 ) THEN
         STATUS = IRA__BADNC
         CALL MSG_SETI( 'NC', NC )
         CALL ERR_REP( 'IRA_SCNAM_ERR1',
     :       'IRA_SCNAM; Invalid value supplied for argument NC: ^NC',
     :                 STATUS )
         GO TO 999
      END IF

*  Call IRA1_ISCNM to do the work.
      CALL IRA1_ISCNM( SCS, NC, DESCR, LD, ABBREV, LA, STATUS )

 999  CONTINUE

      END
