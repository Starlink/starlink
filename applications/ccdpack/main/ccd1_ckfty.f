      SUBROUTINE CCD1_CKFTY( ID, TYPE, ACCEPT, IGNORE, STATUS )
*+
*  Name:
*     CCD1_CKFTY

*  Purpose:
*     Checks the CCDPACK frame type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKFTY( ID, TYPE, ACCEPT, IGNORE, STATUS )

*  Description:
*     This routine checks that the type of a single frame is equivalent
*     to TYPE given the values of ACCEPT and IGNORE. The ACCEPT
*     argument specifies the number of underscores up to which the name
*     must be equivalent. IGNORE specifies how many underscores in the
*     NDF type are ignored. So for instance if we have.
*
*       TYPE='MASTER_BIAS' ACCEPT=1 IGNORE=0
*
*     Then the NDF frame type must be equal to 'MASTER_BIAS'. If we have
*
*       TYPE='TARGET' ACCEPT=0 IGNORE=0
*
*     Then the NDF frame type must be equal to 'TARGET'. If we have
*
*       TYPE='TARGET_PROCESSED' ACCEPT=0 IGNORE=0
*
*     Then the NDF frame type must be 'TARGET'. If we have
*
*       TYPE ='PROCESSED_FLATTENED' ACCEPT=2 IGNORE=1
*
*     The the NDF type must be 'XXXX_PROCESSED_FLATTENED' and the
*     XXXX_ in the name will be ignored etc.
*
*     Failure to meet any of these requirements is not critical
*     and a warning message is just issued.

*  Arguments:
*     ID = INTEGER (Given)
*        The identifier of the NDF whose typing is to be checked.
*     TYPE = CHARACTER * ( * ) (Given)
*        String specifying the characters expected to be found in the
*        NDF frame type.
*     ACCEPT = INTEGER (Given)
*        The number of underscores which should be included in the NDF
*        name when extracted from the NDF extension.
*     IGNORE = INTEGER (Given)
*        How many underscores (and the characters in between them) are
*        to be not considered in the match (from the front of the NDF
*        type string)>
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Just because this subroutine doesn't issue a warning isn't a
*     guarantee that everything is ok. This is just a courtesy function.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     3-OCT-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER ID
      CHARACTER * ( * ) TYPE
      INTEGER ACCEPT
      INTEGER IGNORE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are same except for case

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! The NDF frame type
      INTEGER I                  ! Loop variable
      INTEGER LOCAT              ! Position of _ in string
      INTEGER START              ! Position to start using string from
      INTEGER UPTO               ! Position in string to use up to
      LOGICAL OK                 ! Read extension item

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to get the type of the NDF.
      CALL CCG1_FCH0C( ID, 'FTYPE', FTYPE, OK, STATUS )
      IF ( OK ) THEN

*  Find out the position of the ACCEPT'th underscore. If ACCEPT is 0
*  then we don't have to check for any underscores. If ACCEPT is 1 we
*  expect to find 1 underscore.
         UPTO = 1
         DO 1 I = 1, ACCEPT + 1
            LOCAT = INDEX( FTYPE( UPTO: ), '_' )
            IF ( LOCAT .EQ. 0 ) THEN

*  Failed to find an underscore set to length of string if this is the
*  last loop.
               IF ( I .EQ. ACCEPT + 1 ) THEN
                  UPTO = CHR_LEN( FTYPE )
               ELSE

*  Difficult one. Insufficient underscores located, but don't want to be
*  too pedantic about this. Leave UPTO at current position.
                  UPTO = MAX( UPTO, 1 )
               END IF
            ELSE

*  Increment UPTO to the new underscore +1.
               UPTO = UPTO + LOCAT
            END IF
 1       CONTINUE

*  Now see if we can remove any characters up to the IGNORE underscore.
         START = 1
         IF ( IGNORE .NE. 0 ) THEN
            DO 2 I = 1, IGNORE
               LOCAT = INDEX( FTYPE( START: UPTO ), '_' )
               IF ( LOCAT .EQ. 0 ) THEN

*  Failed to find underscore. Do not increment START.
                  START = MAX( START, 1 )
               ELSE

*  Increment current starting point.
                  START = LOCAT + START
               END IF
 2          CONTINUE
         END IF

*  Now check the NDF type (at least what's left) against the input
*  string.
         IF ( .NOT. CHR_SIMLR( TYPE, FTYPE( START : UPTO ) ) ) THEN

*  Types are dissimilar.
               CALL MSG_SETC( 'TYPE', FTYPE )
               CALL NDF_MSG( 'NDF', ID )
               CALL CCD1_MSG( ' ', ' Warning - the NDF: ^NDF has a'//
     :' frame type (^TYPE) which is not recognised by this application',
     :         STATUS)
         END IF
      END IF

      END
* $Id$
