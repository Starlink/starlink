      SUBROUTINE IRA1_IDTC1( VALUE, NAME, NC, STYLE, TEXT, STATUS )
*+
*  Name:
*     IRA1_IDTC1

*  Purpose:
*     Convert a single floating point sky coordinate value to
*     character form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IDTC1( VALUE, NAME, NC, STYLE, TEXT, STATUS )

*  Description:
*     This routine provides the functionality of IRA_DTOC1. No
*     arguments are verified, the input value is not shifted into its
*     first order range, and no context message is given if the routine
*     fails.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value of the sky coordinate to be formatted, in radians.
*        If VALUE has the "BAD" value (VAL__BADD) then the output
*        string TEXT is set blank.
*     NAME = CHARACTER * ( * ) (Given)
*        The full sky coordinate system name (with no equinox
*        specifier).
*     NC = INTEGER (Given)
*        Determines which sky coordinate is given. If a value of 1 is
*        supplied, VALUE is interpreted as a longitude value (eg RA if
*        an equatorial system is being used).  If a value of 2 is
*        supplied, VALUE is interpreted as a latitude value.
*     STYLE = INTEGER (Given)
*        Specifies the style of output formatting required. These are
*        described in routione IRA_DTOC. STYLE must be in the range 1 to
*        5. Additionally, the value 0 can be specified which causes a
*        default style to be used dependant on the value of NAME. The
*        defaults are: 2 for EQUATORIAL, 5 for GALACTIC and ECLIPTIC.
*     TEXT = CHARACTER * ( * ) (Returned)
*        The string containing the formatted description of the sky
*        coordinate value. The variable supplied for TEXT should have a
*        declared length equal to the value of parameter IRA__SZFSC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     20-SEP-1990 (DSB):
*        Original version.
*     7-MAR-1991 (DSB):
*        Style 4 added.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2
*     8-JUL-1991 (DSB):
*        Leading zeros included in style 4 fields.
*     4-SEP-1991 (DSB):
*        Provision provided for 3 digits in first field when using
*        style 4.
*     19-MAY-1991 (DSB):
*        Use right justification for style 5.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      DOUBLE PRECISION VALUE
      CHARACTER NAME*(*)
      INTEGER NC
      INTEGER STYLE

*  Arguments Returned:
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      CHARACTER AXIS*(IRA__SZSCA)! The name of the sky coordinate axis.
      CHARACTER DESCR*1           ! Description of sky coordinate.
      CHARACTER EQSIGN*4          ! Equals and sign string.
      INTEGER   F1DIG             ! No. of digits in leading field of
                                  ! the sky coordinate string.
      INTEGER   FLD(4)            ! Integers giving the four fields of
                                  ! the sky coordinate value. The
                                  ! seconds field is split into integer
                                  ! part and fractional part.
      INTEGER   LAXIS             ! Used length of AXIS.
      INTEGER   LEQ               ! Used length of EQSIGN.
      INTEGER   LSTYLE            ! The style number. Values of zero are
                                  ! replaced by the corresponding
                                  ! default value.
      CHARACTER LTEXT*(IRA__SZFSC)! Local copy of the output text.
      INTEGER   ND                ! No. of used characters in DESCR.
      CHARACTER S1*3              ! Units for leading field of the sky
                                  ! coordinate string, in style 1.
      CHARACTER S2*1              ! Units for leading field of the sky
                                  ! coordinate string, in style 2.
      CHARACTER SIGN*1            ! Sign of the sky coordinate.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the STYLE value to a local variable.
      LSTYLE = STYLE

*  If equatorial coordinates are being used, convert longitude values
*  (RA) to hours, minutes and seconds, and latitude values (DEC) to
*  degrees, minutes and seconds (unless the value is bad). Also store
*  the text strings needed for different styles.
      IF( NAME .EQ. 'EQUATORIAL' ) THEN
         IF( STYLE .EQ. 0 ) LSTYLE = 2

         IF( VALUE .NE. VAL__BADD ) THEN
            IF( NC .EQ. 1 ) THEN
               CALL SLA_DR2TF( 2, VALUE, SIGN, FLD )
               S1 = 'hrs'
               S2 = 'h'
               F1DIG = 2
            ELSE
               CALL SLA_DR2AF( 2, VALUE, SIGN, FLD )
               S1 = 'deg'
               S2 = 'd'
               F1DIG = 2
            END IF
         END IF


*  If any other coordinates are being used, convert both sky
*  coordinate value to degrees, minutes and seconds.
      ELSE
         IF( STYLE .EQ. 0 ) LSTYLE = 5

         IF( VALUE .NE. VAL__BADD ) CALL SLA_DR2AF( 2, VALUE, SIGN,
     :                                              FLD )

         S1 = 'deg'
         S2 = 'd'

         IF( NC .EQ. 1 ) THEN
            F1DIG = 3
         ELSE
            F1DIG = 2
         END IF

      END IF

*  If the coordinate value is bad, set TEXT to blank.
      IF( VALUE .EQ. VAL__BADD ) THEN
         TEXT = ' '

*  Otherwise, if STYLE 1 is required...
      ELSE
         IF( LSTYLE .EQ. 1 ) THEN

*  Get the name of the coordinate axis.
            CALL IRA1_ISCNM( NAME, NC, DESCR, ND, AXIS, LAXIS, STATUS )

*  Format a minus sign if required.
            IF( SIGN .EQ. '+') THEN
               EQSIGN = ' = '
               LEQ = 3
            ELSE
               EQSIGN = ' = -'
               LEQ = 4
            END IF

*  Form the output string.
            WRITE(TEXT,10) AXIS(:LAXIS), EQSIGN(:LEQ), FLD(1), S1,
     :                      FLD(2), FLD(3), FLD(4)
 10         FORMAT( A, A, I3, A, ' ', I2, 'm ', I2, '.', I2.2, 's' )

*  Remove unwanted spaces infront of numeric fields.
            CALL CHR_LDBLK( TEXT( LAXIS+LEQ+12: ) )
            CALL CHR_LDBLK( TEXT( LAXIS+LEQ+8: ) )
            CALL CHR_LDBLK( TEXT( LAXIS+LEQ+1: ) )

*  If STYLE 2 is required...
         ELSE IF( LSTYLE .EQ. 2 ) THEN

*  Form the output string.
            WRITE(TEXT,20) FLD(1), S2, FLD(2), FLD(3), FLD(4)
 20         FORMAT( I3, A, ' ', I2, 'm ', I2, '.', I2.2, 's' )

*  Remove unwanted spaces infront of numeric fields.
            CALL CHR_LDBLK( TEXT( 10: ) )
            CALL CHR_LDBLK( TEXT( 6: ) )
            CALL CHR_LDBLK( TEXT )

*  If required, add a minus sign at the front.
            IF( SIGN .EQ. '-' ) THEN
               LTEXT = '-'//TEXT
               TEXT = LTEXT
            END IF

*  If STYLE 3 is required...
         ELSE IF( LSTYLE .EQ. 3 ) THEN

*  Form the output string.
            WRITE(TEXT,30) FLD(1), FLD(2), FLD(3), FLD(4)
 30         FORMAT( I3.3, I2.2, I2.2, '.', I2.2 )

*  Remove unwanted leading characters.
            LTEXT = TEXT( 4 - F1DIG: )
            TEXT = LTEXT

*  If required, add a minus sign at the front.
            IF( SIGN .EQ. '-' ) THEN
               LTEXT = '-'//TEXT
               TEXT = LTEXT
            END IF

*  If STYLE 4 is required...
         ELSE IF( LSTYLE .EQ. 4 ) THEN

*  Form the output string with the correct number of digits (2 or 3 )
*  in the first field.
            IF( F1DIG .EQ. 2 ) THEN
               WRITE(TEXT,40) FLD(1), FLD(2), FLD(3), FLD(4)
 40            FORMAT( I2.2,' ', I2.2,' ', I2.2, '.', I2.2 )

            ELSE
               WRITE(TEXT,45) FLD(1), FLD(2), FLD(3), FLD(4)
 45            FORMAT( I3.3,' ', I2.2,' ', I2.2, '.', I2.2 )

            END IF

*  If required, add a minus sign at the front.
            IF( SIGN .EQ. '-' ) THEN
               LTEXT = '-'//TEXT
               TEXT = LTEXT
            ELSE
               LTEXT = ' '//TEXT
               TEXT = LTEXT
            END IF

*  If STYLE 5 is required...
         ELSE IF( LSTYLE .EQ. 5 ) THEN

*  Form the output string as a decimal value; units of hours if NC=1 and
*  EQUATORIAL coordinates specified; units of degrees otherwise.
            IF( S1 .EQ. 'hrs' ) THEN
               WRITE( TEXT, 50 ) VALUE*IRA__RTOD/15.0D0
 50            FORMAT( F10.6 )

            ELSE
               WRITE( TEXT, 50 ) VALUE*IRA__RTOD

            END IF

*  If a bad STYLE value was given, give an error message and quit.
         ELSE
            STATUS = IRA__BADST
            CALL MSG_SETI( 'S', STYLE )
            CALL ERR_REP( 'IRA1_IDTC1_ERR1',
     :            'IRA1_IDTC1: Illegal style requested for formatted '//
     :            'sky coordinates: ^S', STATUS )
         END IF

      END IF

 999  CONTINUE

      END
