      SUBROUTINE IRA_DTOC( A, B, SCS, STYLE, ATEXT, BTEXT, STATUS )
*+
*  Name:
*     IRA_DTOC

*  Purpose:
*     Converts a pair of double-precision sky co-ordinate values to
*     character form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DTOC( A, B, SCS, STYLE, ATEXT, BTEXT, STATUS )

*  Description:
*     This routine creates a pair of text strings containing formatted
*     versions of the given sky co-ordinate values.  The exact format
*     depends on the type of sky co-ordinate system in use and the value
*     of STYLE (see the "Notes" section below). The input co-ordinate
*     values are shifted into their first order ranges before being used
*     (see IRA_NORM).

*  Arguments:
*     A = DOUBLE PRECISION  (Given)
*        The value of the sky longitude to be formatted, in radians. If
*        A has the Starlink "BAD" value (VAL__BADD) then the output
*        string ATEXT is set blank.
*     B = DOUBLE PRECISION  (Given)
*        The value of the sky latitude to be formatted, in radians. If
*        B has the "BAD" value then the output string BTEXT is set
*        blank.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system in use (see ID2 section "Sky
*        Coordinates"). Any unambiguous abbreviation will do.
*     STYLE = INTEGER (Given)
*        A value in the range 1 to 5 which specifies the style of
*        output formatting required.  Additionally, a value of zero can
*        specified which causes a default style to be used dependant on
*        the value of SCS.  See the "Notes" section below for a
*        description of the individual styles and defaults for each
*        SCS.
*     ATEXT = CHARACTER * ( * ) (Returned)
*        The string containing the formatted description of the sky
*        longitude value A. The variable supplied for ATEXT should
*        have a declared length equal to the value of parameter
*        IRA__SZFSC.
*     BTEXT = CHARACTER * ( * ) (Returned)
*        The string containing the formatted description of the sky
*        latitude value B. The variable supplied for BTEXT should
*        have a declared length equal to the value of parameter
*        IRA__SZFSC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*
*     - SCS = "EQUATORIAL"
*       Default is style 2 (used if argument STYLE is zero on entry).
*
*        STYLE = 1:  (a full description)
*
*           "RA = 12hrs 3m 0.02s" and  "DEC = -33deg 23m 0.00s"
*
*        STYLE = 2:  (a more brief form readable by IRA_CTOD)
*
*           "12h 3m 0.02s" and "-33d 23m 0.00s"
*
*        STYLE = 3:  ( a very brief form readable by IRA_CTOD)
*
*           "120300.00" and "-332300.00" (e.g. hhmmss.ss and ddmmss.ss)
*
*        STYLE = 4:  (a brief form readable by IRA_CTOD)
*
*           "12 03 0.02" and "-33 23 0.00"
*
*        STYLE = 5:  (a brief form readable by IRA_CTOD)
*
*           "12.050006" and "-33.383333" (e.g. fractional values in
*                                         hours (RA) and degrees (DEC))
*
*     - SCS = "GALACTIC"
*       Default is style 5 (used if argument STYLE is zero on entry).
*
*        STYLE = 1:
*
*           "l = 12deg 3m 0.02s" and  "b = -33deg 23m 0.00s"
*
*        STYLE = 2:
*
*           "12deg 3m 0.02s" and "-33d 23m 0.00s"
*
*        STYLE = 3:
*
*           "0120300.00" and "-332300.00" (e.g. dddmmss.ss and 
*                                          ddmmss.ss )
*
*        STYLE = 4:  (a brief form readable by IRA_CTOD)
*
*           "12 03 0.02" and "-33 23 0.00"
*
*        STYLE = 5:  (a brief form readable by IRA_CTOD)
*
*           "12.050006" and "-33.383333" (e.g. fractional values in
*                                         degrees )
*
*     - SCS = "ECLIPTIC"
*       Default is style 5 (used if argument STYLE is zero on entry).
*
*        STYLE = 1:
*
*           "Lambda = 12deg 3m 0.02s" and  "Beta = -33deg 23m 0.00s"
*
*        STYLE = 2:
*
*           "12deg 3m 0.02s" and "-33d 23m 0.00s"
*
*        STYLE = 3:
*
*           "0120300.00" and "-332300.00" (e.g. dddmmss.ss and
*                                          ddmmss.ss)
*
*        STYLE = 4:  (a brief form readable by IRA_CTOD)
*
*           "12 03 0.02" and "-33 23 0.00"
*
*        STYLE = 5:  (a brief form readable by IRA_CTOD)
*
*           "12.050006" and "-33.383333" (e.g. fractional values in
*                                         degrees)

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     7-MAR-1991 (DSB):
*        Style 4 added.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2
*     8-JUL-1991 (DSB):
*        Leading zeros included in style 4 fields.
*     12-NOV-1992 (DSB):
*        New abbreviations for GALACTIC and ECLIPTIC co-ords described.
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
      DOUBLE PRECISION  A
      DOUBLE PRECISION  B
      CHARACTER SCS*(*)
      INTEGER STYLE

*  Arguments Returned:
      CHARACTER ATEXT*(*)
      CHARACTER BTEXT*(*)

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      DOUBLE PRECISION AA        ! Local copy of A.
      DOUBLE PRECISION BB        ! Local copy of B.
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      CHARACTER NAME*(IRA__SZSCS)! Tidied version of argument SCS.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the output strings are long enough.
      IF( LEN( ATEXT ) .LT. IRA__SZFSC .OR.
     :    LEN( BTEXT ) .LT. IRA__SZFSC ) THEN
         STATUS = IRA__TOOSH
         CALL ERR_REP( 'IRA_DTOC_ERR1',
     :         'IRA_DTOC: Supplied text string is too short',
     :         STATUS )
      END IF

*  Check that STYLE value is legal.
      IF( STYLE .LT. 0 .OR. STYLE .GT. 5 ) THEN
         STATUS = IRA__BADST
         CALL MSG_SETI( 'ST', STYLE )
         CALL ERR_REP( 'IRA_DTOC_ERR2',
     :         'IRA_DTOC: Supplied STYLE value (=^ST) is illegal',
     :         STATUS )
      END IF

*  Identify the SCS.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )

*  Shift the input sky co-ordinate values into their first order ranges.
      AA = A
      BB = B
      CALL IRA_NORM( AA, BB, STATUS )

*  Convert the first sky co-ordinate value.
      CALL IRA1_IDTC1( AA, NAME, 1, STYLE, ATEXT, STATUS )

*  Convert the second sky co-ordinate value.
      CALL IRA1_IDTC1( BB, NAME, 2, STYLE, BTEXT, STATUS )

*  If an error occurred, give a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DTOC_ERR3',
     :                 'IRA_DTOC: Unable to format sky co-ordinates',
     :                 STATUS )
      END IF

      END
