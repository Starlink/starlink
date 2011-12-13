      SUBROUTINE CCD1_GTBDS( USEEXT, ID, LIMIT, ORIGIN, MAXENT,
     :                       BOUNDS, NBOUND, FRMEXT, STATUS )
*+
*  Name:
*     CCD1_GTBDS

*  Purpose:
*     To get pairs of CCD bounds subject to limits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTBDS( USEEXT, ID, LIMIT, ORIGIN, MAXENT, BOUNDS,
*                      NBOUND, FRMEXT, STATUS )

*  Description:
*     This routine attempts to get pairs of bounds which place the bias
*     strips of a CCD. The bounds are obtained in two ways. If USEEXT is
*     true then an attempt to get the values directly from the NDF
*     extension is made. The bounds are usually stored in the structure
*     BOUNDS (.START1, .END1, .START2, .END2). If this fails for
*     any reason then an attempt to get values via the parameter system
*     is made.
*
*     The routine checks that all values are paired and are >= origin
*     and <= to the limit value (plus origin). If the values are
*     incorrectly ordered they are swapped around. If any given values
*     exceed the permissable bounds then they are truncated to within
*     the allowed bounds. The output values are array bounds not pixel
*     indices, so they can be directly used to reference array elements
*     (i.e. they are corrected for the origin offset of an NDF).

*  Arguments:
*     USEEXT = LOGICAL (Given)
*        Whether to attempt to get the values from the NDF extension.
*     ID = INTEGER (Given)
*        The identifier of the NDF -- not used if USEEXT is false.
*     LIMIT = INTEGER (Given)
*        The range of values that output bounds are allowed to have.
*        The actual upper limit is LIMIT+ORIGIN-1, the lower limit is
*        ORIGIN. If this value is negative then no bounds checking
*        takes place.
*     ORIGIN = INTEGER (Given)
*        The origin of the first valid element in the direction of
*        increasing LIMIT values (i.e. the NDF origin ).
*     MAXENT = INTEGER (Given)
*        Maximum number of entries which can be returned.
*     BOUNDS( MAXENT ) = INTEGER (Returned)
*        The bounds returned from the user.
*     NBOUND = INTEGER (Returned)
*        The number of bounds returned.
*     FRMEXT = LOGICAL (Returned)
*        Whether the values were obtained from the NDF extension or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  ADAM Parameters:
*     BOUNDS( * ) = _INTEGER (Read)
*        The bounds returned from the user.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-APR-1991 (PDRAPER):
*        Original version.
*     1-JUL-1991 (PDRAPER):
*        Added no bounds check on negative LIMIT.
*     1-AUG-1991 (PDRAPER):
*        Changed to truncate, without reprompting.
*     9-SEP-1991 (PDRAPER):
*        Changed to use ORIGIN to properly take care of pixel and array
*        indices.
*     29-SEP-1993 (PDRAPER):
*        Added USEEXT and ID.
*     18-JAN-1994 (PDRAPER):
*        Added FRMEXT argument.
*     17-AUG-2000 (PDRAPER):
*        Fixed problem obtaining just two values from NDF extension
*        (this wasn't allowed and a prompt followed).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL USEEXT
      INTEGER ID
      INTEGER LIMIT
      INTEGER ORIGIN
      INTEGER MAXENT

*  Arguments Returned:
      INTEGER BOUNDS( MAXENT )
      INTEGER NBOUND
      LOGICAL FRMEXT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  I                 ! Loop variable
      INTEGER  J                 ! Loop variable
      INTEGER IBUFF              ! Integer buffer
      INTEGER LOWER( 2 )         ! Lower bounds
      INTEGER UPPER( 2 )         ! Upper bounds
      LOGICAL OK                 ! Got value ok

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default FRMEXT value is USEEXT.
      FRMEXT = USEEXT

*  Try to get the bounds.
      OK = .FALSE.
      IF ( USEEXT ) THEN

*  Look in extension first.
         CALL CCG1_FCH1I( ID, 'BOUNDS', 'START1,END1,', 2,
     :                    LOWER, OK, STATUS )

*  If ok look for second lot.
         IF ( OK ) THEN
            NBOUND = 2
            BOUNDS( 1 ) = LOWER( 1 )
            BOUNDS( 2 ) = LOWER( 2 )
            CALL CCG1_FCH1I( ID, 'BOUNDS', 'START2,END2,', 2,
     :                       UPPER, OK, STATUS )
            IF ( OK ) THEN
               NBOUND = 4
               BOUNDS( 3 ) = UPPER( 1 )
               BOUNDS( 4 ) = UPPER( 2 )
            ELSE

*  Second lot not present, but first are, so that's OK.
               OK = .TRUE.
            END IF
         END IF
      END IF

*  If we havn't gotten a set of bounds by now prompt the user.
      IF ( .NOT. OK ) THEN

*  Values not from extension.
         FRMEXT = .FALSE.
 1       CONTINUE
            CALL PAR_GET1I( 'BOUNDS', MAXENT, BOUNDS, NBOUND, STATUS )

*  Make sure that can get out.
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check the number of returns, must be even.
            IF ( ( NBOUND / 2 ) * 2 .NE. NBOUND ) THEN
               CALL MSG_OUT( 'GTBDS_PAIRS',
     :      ' Warning - must supply the bounds in pairs', STATUS )
               CALL PAR_CANCL( 'BOUNDS', STATUS )
            GO TO 1
         END IF
      END IF

*  Ok have bounds are they in increasing pairs ?
      DO 2 I = 1, NBOUND, 2
         IF ( BOUNDS( I + 1 ) .LT. BOUNDS( I ) ) THEN

*  If not then swap them.
            IBUFF = BOUNDS( I )
            BOUNDS( I ) = BOUNDS( I + 1 )
            BOUNDS( I + 1 ) = IBUFF
         END IF

*  Right do the real work, are they all less than the upper most
*  value?
*  Only check this if the limit is greater than zero (this indicates no
*  bounds checking required ).
         DO 3 J = I, I + 1
            IF ( LIMIT .GT. 0 ) THEN
               IF ( BOUNDS( J ) .GT. LIMIT + ORIGIN - 1 ) THEN

*  Truncate to fit.
                  BOUNDS( J ) = LIMIT + ORIGIN - 1

*  Offer warning.
                  CALL MSG_SETI( 'GTBDS_LIMIT', LIMIT + ORIGIN - 1 )
                  CALL MSG_OUT( 'GTBDS_GTLIMIT',
     :' Warning - given bounds greater than upper limit; '//
     :'truncated to ^GTBDS_LIMIT', STATUS )


*  Check lower limit.
               ELSE IF ( BOUNDS( J ) .LT. ORIGIN ) THEN
                  BOUNDS( J ) = ORIGIN
                  CALL MSG_SETI( 'GTBDS_LIMIT', ORIGIN )
                  CALL MSG_OUT( 'GTBDS_LTLIMIT',
     :' Warning - given bounds less than lower limit, bound '//
     :'set to ^GTBDS_LIMIT', STATUS )
               END IF
            END IF

*  Correct for origin offset.
            BOUNDS( J ) = BOUNDS( J ) - ORIGIN + 1
 3       CONTINUE
 2    CONTINUE
 99   END
* $Id$
