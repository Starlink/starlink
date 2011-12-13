      SUBROUTINE COF_CHKXT( FUNIT, EXTN, MATCH, STATUS )
*+
*  Name:
*     COF_CHKXT

*  Purpose:
*     Check FITS header against an extension specifier

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_CHKXT( FUNIT, EXTN, MATCH, STATUS )

*  Description:
*     Checks FITS header currently selected on FUNIT against the extension
*     specifier in EXTN.
*     The extension specifier may consist of one or more keyword=value pairs
*     optionally enclosed in [].
*     If all the keywords in the header match the given values, MATCH is
*     returned .TRUE.; otherwise it is returned .FALSE.
*     A required keyword missing from the header is a mismatch, not an error.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITS unit number
*     EXTN = CHARACTER*(*) (Given)
*        The extension specifier
*     MATCH = LOGICAL (Returned)
*        Whetehr the header matched the extension specifier.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}
*     [routine_example]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  External Routines Used:
*     CHR
*        CHR_FIWS
*        CHR_LEN
*        CHR_SIMLR
*     FITSIO
*        FTGKYS
*        FTCMSG

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Prior Requirements:
*     -  All insignificant spaces should have been removed from the extension
*        specifiers
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      9-MAY-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Global Variables:

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER*(*) EXTN

*  Arguments Returned:
      LOGICAL MATCH

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN           ! Used length of string
      LOGICAL CHR_SIMLR         ! Caseles string comparison

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      INTEGER LEN                    ! Length of EXTN
      INTEGER I1, I2, I3             ! Indexes
      INTEGER FSTAT                  ! FITSIO status
      LOGICAL DONE                   ! If completed
      CHARACTER*16 KEYWRD            ! Required keyword
      CHARACTER*70 REQVAL            ! Required keyword value
      CHARACTER*70 ACTVAL            ! Actual keyword value
      CHARACTER*70 COMMENT           ! Keyword comment

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LEN = CHR_LEN( EXTN )
      I1 = 1
*  If EXTN starts [, skip it and remove any terminating ]
      IF( EXTN(I1:I1) .EQ. '[' ) THEN
         I1 = I1 + 1
         IF ( EXTN( LEN:LEN ) .EQ. ']' ) LEN = LEN -1
      END IF

      IF ( I1 .GT. LEN ) THEN
         STATUS = SAI__ERROR

      ELSE
*  Now treat each keyword pair (separated by ,) in EXTN
*  Allow spaces
         DONE = .FALSE.
         DOWHILE ( (STATUS .EQ. SAI__OK) .AND. (.NOT. DONE ) )
*  I1 is start of this specifier element
*  Set I2 to end of this specifier element
            I2 = INDEX( EXTN(I1:LEN), ',' )
            IF ( I2 .EQ. 1 ) THEN
*  Null specifier
               STATUS = SAI__ERROR

            ELSE
*  Non-null specifier
               IF ( I2 .EQ. 0 ) THEN
                  DONE = .TRUE.
                  I2 = LEN
               ELSE
                  I2 = I1 + I2 - 2
               END IF

*  Set the keyword; i.e the bit before =, or if there is no = set EXTNAME
               I3 = INDEX( EXTN(I1:I1+I2-1), '=' )
               IF ( I3 .LE. 1 ) THEN
*  There was no =, assume keyword EXTNAME
                  KEYWRD = 'EXTNAME'
               ELSE
*  Get the given keyword
                  KEYWRD = EXTN( I1:I1+I3-2 )
               END IF
*  Set I1 to start of value
               I1 = I1 + I3

*  Now get the value
               IF ( I2 .LT. I1 ) THEN
                  STATUS = SAI__ERROR

               ELSE
*  Non-null value
                  REQVAL = EXTN(I1:I2)
                  I1 = I2 + 2

*  keyword = value pair obtained OK
*  Attempt to get the KEYWORD from the FITS header and check for a match.
*  Failure is not regarded as an error.
*  We set MATCH .FALSE. as only the this one counts - the fact that we got
*  this far indicates all previous ones matched.
                  MATCH = .FALSE.
                  FSTAT = FITSOK
                  CALL FTGKYS( FUNIT, KEYWRD, ACTVAL, COMMENT, FSTAT )
                  IF ( FSTAT .LE. FITSOK ) THEN
*  Keyword found - check aganst value
                     IF ( CHR_SIMLR( ACTVAL, REQVAL ) ) THEN
                        MATCH = .TRUE.
                     ELSE
                        DONE = .TRUE.
                     END IF
                  ELSE
*  Keyword not found in this HDU
*  Cancel FITS error message and
                     CALL FTCMSG
                     DONE = .TRUE.
                  END IF  ! Keyword got from header

               END IF  ! keyword = value pair got from specifier
            END IF  ! Non-null specifier element
         END DO  ! for each specifier element

      END IF  ! Non-null specifier

*  Check and report if error
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'EXTN', EXTN )
         CALL ERR_REP( 'COF_CHKXT_SPEC',
     :     'Illegal extension specifier ^EXTN', STATUS )
      END IF

      END
