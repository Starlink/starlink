      SUBROUTINE IRA1_ISCNM( SCS, NC, DESCR, LD, ABBREV, LA, STATUS )
*+
*  Name:
*     IRA1_ISCNM

*  Purpose:
*     Get full name and abbreviation of a sky coordinate.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ISCNM( SCS, NC, DESCR, LD, ABBREV, LA, STATUS )

*  Description:
*     This routine provides the functionality of IRA_SCNAM . No checks
*     are made on arguments SCS, and NC.

*  Arguments:
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system (an abbreviation is OK).
*     NC = INTEGER (Given)
*        The axis required; 1 for longitude, 2 for latitude.
*     DESCR = CHARACTER * ( * ) (Returned)
*        A full description of the axis name (see routine IRA_SCNAM).
*        This should have a declared length of IRA__SZSCD.
*     LD = INTEGER (Returned)
*        No. of used characters in DESCR.
*     ABBREV = CHARACTER * ( * ) (Returned)
*        An abbreviation for the axis name (see routine IRA_SCNAM). This
*        should have a declared length of IRA__SZSCA.
*     LA = INTEGER (Returned)
*        No. of used characters in ABBREV.
*     STATUS = INTEGER (Given and Returned)
*        Status value.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2
*     2-APR-1992 (DSB):
*        Epochs added to full names.
*     12-NOV-1992 (DSB):
*        Abbreviations ELONG,ELAT,GLONG,GLAT changed to Lambda,Beta,l,b
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
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

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER BJ*1             ! Epoch type specifier (Besselian or
                                 ! Julian) in argument SCS.
      DOUBLE PRECISION EQU       ! Epoch of reference equinox
                                 ! specified in argument SCS.
      CHARACTER NAME*(IRA__SZSCS)! Full name of SCS.
      INTEGER NLEN               ! Position of end of equinox specifier.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the SCS.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Format the equinox specifier.
      CALL IRA_SETEQ( EQU, BJ, NAME, STATUS )
      NLEN = CHR_LEN( NAME )

*  Equatorial systems:
      IF( NAME( 1 : 10 ) .EQ. 'EQUATORIAL' ) THEN
         IF( NC .EQ. 1 ) THEN
            ABBREV = 'RA'
            LA = 2
            DESCR = 'Right Ascension'//' '//NAME( 11 : NLEN )
            LD = 6 + NLEN
         ELSE
            ABBREV = 'DEC'
            LA = 3
            DESCR = 'Declination'//' '//NAME( 11 : NLEN )
            LD = 2 + NLEN
         END IF

*  Galactic:
      ELSE IF( NAME( 1 : 8 ) .EQ. 'GALACTIC' ) THEN
         IF( NC .EQ. 1 ) THEN
            ABBREV = 'l'
            LA = 1
            DESCR = 'Galactic Longitude'
            LD = 18
         ELSE
            ABBREV = 'b'
            LA = 1
            DESCR = 'Galactic Latitude'
            LD = 17
         END IF

*  Ecliptic:
      ELSE IF( NAME( 1 : 8 ) .EQ. 'ECLIPTIC' ) THEN
         IF( NC .EQ. 1 ) THEN
            ABBREV = 'Lambda'
            LA = 6
            DESCR = 'Ecliptic Longitude'//' '//NAME( 9 : NLEN )
            LD = 11 + NLEN
         ELSE
            ABBREV = 'Beta'
            LA = 4
            DESCR = 'Ecliptic Latitude'//' '//NAME( 9 : NLEN )
            LD = 10 + NLEN
         END IF

*  Unrecognised:
      ELSE
         STATUS = IRA__BADSC
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'IRA1_ISCNM_ERR1',
     :      'IRA1_ISCNM: Sky coordinates ^NAME not yet supported',
     :                  STATUS )

      END IF

 999  CONTINUE

      END
