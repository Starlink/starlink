      SUBROUTINE SUBPAR_UNSET( NAMECODE, KEYS, STATUS )
*+
*  Name:
*     SUBPAR_UNSET

*  Purpose:
*     To cancel certain dynamic values associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_UNSET( NAMECODE, KEYS, STATUS )

*  Description:
*     The subroutine allows certain dynamic values associated with a
*     parameter to be unset.
*     Which values are to be unset is decided by the value of KEYS,
*     which may be a list of zero or more keywords, separated by commas
*     or spaces. The keywords may be:
*          DEFAULT to cancel the dynamic default (set by SUBPAR_DEFnx
*              or DAT_DEF),
*          MINIMUM to cancel the minimum value (set by SUBPAR_MIN) or
*          MAXIMUM to cancel the maximum value (set by SUBPAR_MAX).
*     The keywords may be abbreviated to their minimum unambiguous form.
*     The case of KEYS is unimportant. If it is blank, none of the values
*     will be unset.
*     The routine will operate regardless of the given STATUS value and will
*     not report or set STATUS if the specified values have not been set or
*     are already unset.
*
*     STATUS SUBPAR__ERROR will be returned if STATUS was SAI__OK on entry
*     and an illegal or ambiguous keyword is given.

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        The namecode for the parameter
*     KEYS = CHARACTER*(*) (Given)
*        A list of keywords specifying the values to be unset
*     STATUS = INTEGER (Returned)
*        The global status.
*        The routine is executed regardless of the import value of STATUS.
*
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1993 (AJC):
*        Original version.
*     03-MAY-2006 (TIMJ):
*        Initialise KEYDO.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS Constants
      INCLUDE 'SUBPAR_ERR'       ! SUBPAR error codes
      INCLUDE 'CHR_ERR'          ! CHR error codes

*  Arguments Given:
      INTEGER NAMECODE
      CHARACTER*(*) KEYS

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! SUBPAR common block

*  Local Constants:
      INTEGER NKEYS              ! Number of defined keywords
      PARAMETER (NKEYS=3)

*  Local Variables:
      INTEGER START              ! Index to start of word
      INTEGER END                ! Index to end of word
      INTEGER KEYLEN             ! Length of given key
      INTEGER NMATCH             ! Index to matched keyword
                                 ! -1 if more than one match
      INTEGER I                  ! Loop index
      CHARACTER*10 UKEY          ! KEYS in upper case
      CHARACTER*10 KEYWORDS(NKEYS) ! The defined keywords
      LOGICAL KEYMATCH(NKEYS)    ! Whether the keyword is matched
      LOGICAL KEYDO(NKEYS)       ! Whether the keyword is requested

*  Local Data:
      DATA KEYWORDS/'MINIMUM','MAXIMUM','DEFAULT'/
      DATA KEYDO / NKEYS * .FALSE. /
*.

*  Start new error environment
      CALL EMS_BEGIN( STATUS )

*  Find the start of the first word
      START = 1
      CALL CHR_FIWS( KEYS, START, STATUS )

      DOWHILE ( STATUS .EQ. SAI__OK )

*     Find the end of the word
         END = START
         CALL CHR_FIWE( KEYS, END, STATUS )
*     Ignore errors - we expect CHR_ENDOFSENT at some stage
         CALL EMS_ANNUL( STATUS )

*     Get an upper case copy of the word
         UKEY = KEYS(START:END)
         CALL CHR_UCASE( UKEY )
         KEYLEN = END - START + 1

*     Look for matches
         NMATCH = 0

*     For each defined keyword in turn
         DO 10 I = 1, NKEYS

*        If the given keyword matches it
            IF ( UKEY .EQ. KEYWORDS(I)(1:KEYLEN) ) THEN
*           Flag the match
               KEYMATCH(I) = .TRUE.
*           and if this is the first match, note the index
               IF ( NMATCH .EQ. 0 ) THEN
                  NMATCH = I
*           Otherwise set NMATCH to negative of first match to indicate
*           ambiguity
               ELSE IF ( NMATCH .GT. 0 ) THEN
                  NMATCH = -NMATCH
               ENDIF

*        Otherwise flag no match
            ELSE
               KEYMATCH(I) = .FALSE.
            ENDIF

10       END DO

*     Now check results of matching

*     Check for OK keyword flag it to be done
         IF ( NMATCH .GT. 0 ) THEN
            KEYDO(NMATCH) = .TRUE.

*     else check for no match
         ELSE IF ( NMATCH .EQ. 0 ) THEN
            STATUS = SUBPAR__ERROR
            CALL EMS_SETC( 'KEY', KEYS(START:END) )
            CALL EMS_REP( 'SUP_UNSET1',
     :      'SUBPAR_UNSET: Argument KEYS Invalid keyword - ''^KEY''',
     :       STATUS )

*     else ambiguous keyword given
         ELSE
            STATUS = SUBPAR__ERROR
            CALL EMS_REP( 'SUP_UNSET2a',
     :      'SUBPAR_UNSET: Argument KEYS - ambiguous keyword',
     :       STATUS )
            CALL EMS_SETC( 'KEY', KEYS(START:END) )
            CALL EMS_SETC( 'KEYS', KEYWORDS(-NMATCH) )
            DO I = -NMATCH+1, NKEYS
               IF ( KEYMATCH(I) ) THEN
                  CALL EMS_SETC( 'KEYS', ' and' )
                  CALL EMS_SETC( 'KEYS', ' ' )
                  CALL EMS_SETC( 'KEYS', KEYWORDS(I) )
               END IF
            ENDDO
            CALL EMS_REP( 'SUP_UNSET2b', '''^KEY'' matches ^KEYS',
     :       STATUS )
         END IF

*     Find the start of next keyword
         START = END + 1
         CALL CHR_FIWS( KEYS, START, STATUS )

      END DO

*  Annul expected end status
      IF ( STATUS .EQ. CHR__WRDNOTFND ) CALL EMS_ANNUL( STATUS )



*  Now, if OK, act on keyword
      IF ( STATUS .EQ. SAI__OK ) THEN

         DO 20 I = 1, NKEYS

            IF ( KEYDO(I) ) THEN
               IF ( KEYWORDS(I) .EQ. 'DEFAULT' ) THEN
                  IF ( PARDYN( 3, NAMECODE ) .GT. 0 )
     :             PARDYN( 3, NAMECODE ) = - PARDYN( 3, NAMECODE )

               ELSE IF ( KEYWORDS(I) .EQ. 'MINIMUM' ) THEN
                  PARMIN( 2, NAMECODE ) = -1

               ELSE IF ( KEYWORDS(I) .EQ. 'MAXIMUM' ) THEN
                  PARMAX( 2, NAMECODE ) = -1

               END IF

            END IF

20       END DO

      END IF

*  End error environment
      CALL EMS_END( STATUS )

      END
