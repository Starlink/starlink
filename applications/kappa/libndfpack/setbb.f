      SUBROUTINE SETBB( STATUS )
*+
*  Name:
*     SETBB

*  Purpose:
*     Sets a new value for the quality bad-bits mask of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETBB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application sets a new value for the bad-bits mask
*     associated with the quality component of an NDF.  This 8-bit mask
*     is used to select which of the bits in the quality array should
*     normally be used to generate "bad" pixels when the NDF is
*     accessed.
*
*     Wherever a bit is set to 1 in the bad-bits mask, the
*     corresponding bit will be extracted from the NDF's quality array
*     value for each pixel (the other quality bits being ignored).  A
*     pixel is then considered "bad" if any of the extracted quality
*     bits is set to 1.  Effectively, the bad-bits mask therefore allows
*     selective activation of any of the eight 1-bit masks which can be
*     stored in the quality array.

*  Usage:
*     setbb ndf bb

*  ADAM Parameters:
*     AND = _LOGICAL (Read)
*        By default, the value supplied via the BB parameter will be
*        used literally as the new bad-bits mask value.  However, if a
*        TRUE value is given for the AND parameter, then a bit-wise
*        `AND' will first be performed with the old value of the mask.
*        This facility allows individual bits in within the mask to be
*        cleared (i.e. reset to zero) without affecting the current
*        state of other bits (see the "Examples" section).
*
*        The AND parameter is not used if a TRUE value is given for the
*        OR parameter. [FALSE]
*     BB = LITERAL (Read)
*        The new integer value for the bad-bits mask.  This may either
*        be specified in normal decimal notation, or may be given using
*        binary, octal or hexadecimal notation by adding a "B", "O" or
*        "Z" prefix (respectively) to the appropriate string of digits.
*        The value supplied should lie in the range 0 to 255 decimal (or
*        8 bits of binary).
*
*        It may also be specified as a comma-separated list of quality
*        names. A quality name is a symbolic name that identifies a
*        specific quality bit (quality names can be defined using
*        SETQUAL, and displayed using SHOWQUAL).
*
*        If the AND and OR parameters are both FALSE, then the value
*        supplied will be used directly as the new mask value.
*        However, if either of these logical parameters is set to TRUE,
*        then an appropriate bit-wise `AND' or `OR' operation with the
*        old mask value will first be performed.
*
*        The default value suggested when prompting for this value is
*        chosen so as to leave the original mask value unchanged.
*     NDF = NDF (Read and Write)
*        The NDF whose bad-bits mask is to be modified.
*     OR = _LOGICAL (Read)
*        By default, the value supplied via the BB parameter will be
*        used literally as the new bad-bits mask value.  However, if a
*        TRUE value is given for the OR parameter, then a bit-wise `OR'
*        will first be performed with the old value of the mask.  This
*        facility allows individual bits in within the mask to be set
*        to 1 without affecting the current state of other bits (see
*        the "Examples" section).  [FALSE]

*  Examples:
*     setbb myframe 3
*        Sets the bad-bits mask value for the quality component of the
*        NDF called myframe to the value 3.  This means that bits 1 and
*        2 of the associated quality array will be used to generate bad
*        pixels.
*     setbb myframe "SKY,BACK"
*        Sets the bad-bits mask value for the quality component of the
*        NDF called myframe so that any pixel that is flagged with
*        either of the two qualities "SKY" or "BACK" will be set bad.
*        The NDF should contain information that associates each of
*        these quality names with a specific bit in the quality array.
*        Such information can for instance be created using the SETQUAL
*        command.
*     setbb ndf=myframe bb=b11
*        This example performs the same operation as above, but in this
*        case the new mask value has been specified using binary
*        notation.
*     setbb xspec b10001000 or
*        Causes the bad-bits mask value in the NDF called xspec to
*        undergo a bit-wise `OR' operation with the binary value
*        10001000.  This causes bits 4 and 8 to be set without changing
*        the state of any other bits in the mask.
*     setbb quasar ze7 and
*        Causes the bad-bits mask value in the NDF called quasar to
*        undergo a bit-wise `AND' operation with the hexadecimal value
*        E7 (binary 11100111).  This causes bits 4 and 5 to be cleared
*        (i.e. reset to zero) without changing the state of any other
*        bits in the mask.

*  Notes:
*     The bad-bits value will be disregarded if the NDF supplied does
*     not have a quality component present.  A warning message will be
*     issued if this should occur.

*  Related Applications:
*     KAPPA: QUALTOBAD, REMQUAL, SETQUAL, SHOWQUAL; Figaro: Q2BAD.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     15-APR-1991 (RFWS):
*        Original version.
*     17-APR-1991 (RFWS):
*        Added the AND and OR parameters and changed to allow binary,
*        octal and hexadecimal notation.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     4-APR-2008 (DSB):
*        Add facility for specifying the mask using quality names.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      BYTE BB                    ! Old bad-bits value (unsigned byte)
      CHARACTER BBC*150          ! Bad-bits value as characters
      CHARACTER BBD*9            ! Default bad-bits value
      CHARACTER COMMNT*200       ! Comment associated with quality name
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to the quality name info
      CHARACTER XNAME*(DAT__SZNAM)! NDF enstension holding quality names
      INTEGER BBI                ! Bad-bits value as an integer
      INTEGER BIT                ! The bit number
      INTEGER COMMA              ! Index of comma at end of quality name
      INTEGER DIGVAL             ! Value of binary digit
      INTEGER IDIG               ! Loop counter for binary digits
      INTEGER NDF                ! NDF identifier
      INTEGER START              ! Index 1st character in quality name
      LOGICAL AND                ! Perform a bit-wise AND operation?
      LOGICAL FIXED              ! Is the quality value constant?
      LOGICAL OR                 ! Perform a bit-wise OR operation?
      LOGICAL THERE              ! Quality component present?
      LOGICAL VALUE              ! The constant quality value

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Obtain the current bad-bits value.
      CALL NDF_BB( NDF, BB, STATUS )

*  Obtain logical values for the OR and AND parameters.
      CALL PAR_GET0L( 'OR', OR, STATUS )
      AND = .FALSE.
      IF ( .NOT. OR ) CALL PAR_GET0L( 'AND', AND, STATUS )

*  Calculate a default value for the bad-bits mask which corresponds to
*  no change. If OR is .TRUE., the default is no bits set.
      IF ( OR ) THEN
         BBD = 'B00000000'

*  If AND is .TRUE., the default is all bits set.
      ELSE IF ( AND ) THEN
         BBD = 'B11111111'

*  Otherwise, encode the current bad-bits value as a binary string.
      ELSE
         BBI = NUM_UBTOI( BB )
         DIGVAL = 2 ** 7
         BBD = 'B'
         DO 1 IDIG = 1, 8
            IF ( BBI .GE. DIGVAL ) THEN
               BBD( IDIG + 1 : IDIG + 1 ) = '1'
               BBI = BBI - DIGVAL
            ELSE
               BBD( IDIG + 1 : IDIG + 1 ) = '0'
            END IF
            DIGVAL = DIGVAL / 2
 1       CONTINUE
      END IF

*  Set the default value for the BB parameter and then obtain a new
*  value.
 2    CONTINUE                   ! Start of DO WHILE loop
      CALL PAR_DEF0C( 'BB', BBD, STATUS )
      CALL PAR_GET0C( 'BB', BBC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert the value to upper case and test the first character to
*  determine the number format used. Decode the number into an
*  integer.
         CALL CHR_UCASE( BBC )

*  Binary:
         IF ( BBC( 1 : 1 ) .EQ. 'B' ) THEN
            CALL CHR_BTOI( BBC( 2 : ), BBI, STATUS )

*  Octal:
         ELSE IF ( BBC( 1 : 1 ) .EQ. 'O' ) THEN
            CALL CHR_OTOI( BBC( 2 : ), BBI, STATUS )

*  Hexadecimal:
         ELSE IF ( BBC( 1 : 1 ) .EQ. 'Z' ) THEN
            CALL CHR_HTOI( BBC( 2 : ), BBI, STATUS )

*  Decimal:
         ELSE
            CALL CHR_CTOI( BBC, BBI, STATUS )
         END IF

*  If the number could not be decoded, then see if it is a list
*  of quality names.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOC is returned holding a set of
*  five HDS locators which identify the NDF and the various components
*  of the quality information. XNAME is returned holding the name of the
*  NDF extension in which the information was found. If no quality name
*  information is found, then an error is reported.
            CALL IRQ_FIND( NDF, LOCS, XNAME, STATUS )

*  Clear all bits in the bad bits mask.
            BBI = 0

*  Loop round every word in the BB parameter value that ends with a
*  comma.
            START = 1
            COMMA = INDEX( BBC, ',' )
            DO WHILE ( COMMA .GT. 0 .AND. STATUS == SAI__OK )
               COMMA = COMMA + START - 1

*  If the word has non-zero length, see if it is a defined quality name.
*  If so, get the 1-based bit number corresponding to the quality name.
               IF ( COMMA .GT. START ) THEN
                  CALL IRQ_GETQN( LOCS, BBC( START : COMMA - 1 ), FIXED,
     :                            VALUE, BIT, COMMNT, STATUS )

*  Set the corresponding bit in the bad bits mask.
                  BBI = BBI + 2**( BIT - 1 )

               END IF

*  Find the comma at the end of the next word.
               START = COMMA + 1
               COMMA = INDEX( BBC( START: ), ',' )
            END DO

*  Handle any quality name following the final comma.
            IF ( START .LE. LEN( BBC ) ) THEN
               CALL IRQ_GETQN( LOCS, BBC( START : ), FIXED, VALUE, BIT,
     :                         COMMNT, STATUS )
               BBI = BBI + 2**( BIT - 1 )
            END IF

*  Release the quality name information.
            CALL IRQ_RLSE( LOCS, STATUS )
         END IF

*  If the number could not be decoded, and it was not a list of quality
*  names, then report an error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'BB', BBC )
            CALL ERR_REP( 'SETBB_SYNTAX',
     :                    'Invalid bad-bits value ''^BB'' ' //
     :                    'specified (bad syntax).', STATUS )

*  Flush the error, cancel the BB parameter and return to get a new
*  value.
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'BB', STATUS )
            GO TO 2

*  If the number was decoded successfully, then check that is is in
*  range. If not, then report an error.
         ELSE IF ( ( BBI .LT. 0 ) .OR. ( BBI .GT. 255 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'BB', BBC )
            CALL ERR_REP( 'SETBB_XRANGE',
     :                    'Invalid bad-bits mask value ''^BB'' ' //
     :                    'specified (out of range).', STATUS )
            CALL ERR_REP( 'SETBB_RANGE',
     :                    'Value should lie in the range 0 to 255 ' //
     :                    'decimal (8 bits binary).', STATUS )

*  Flush the error, cancel the BB parameter and return to get a new
*  value.
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( 'BB', STATUS )
            GO TO 2
         END IF
      END IF

*  If the OR or AND parameters were set to .TRUE., perform the required
*  AND or OR operation between the old bad-bits value and the new
*  value.
      IF ( OR ) THEN
         BBI = IOR( BBI, NUM_UBTOI( BB ) )
      ELSE IF ( AND ) THEN
         BBI = IAND( BBI, NUM_UBTOI( BB ) )
      END IF

*  Set the new bad-bits value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_SBB( NUM_ITOUB( BBI ), NDF, STATUS )
      END IF

*  See if the NDF has a quality component. If not, then display a
*  warning message.
      CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         CALL MSG_BLANK( STATUS )
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_OUT( 'WARN1',
     :                 '   WARNING: The NDF structure ^NDF has no ' //
     :                 'quality component.', STATUS )
         CALL MSG_OUT( 'WARN2',
     :                 '            The new bad-bits mask value ' //
     :                 'will be ignored.', STATUS )
        CALL MSG_BLANK( STATUS )
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( NDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETBB_ERR',
     :     'SETBB: Error setting a bad-bits mask value for the ' //
     :     'quality component of an NDF.', STATUS )
      END IF

      END
