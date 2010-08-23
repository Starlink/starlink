      SUBROUTINE IRA_TRANS( NVAL, IN1, IN2, FORWRD, SCS, IDA,
     :                      OUT1, OUT2, STATUS )
*+
*  Name:
*     IRA_TRANS

*  Purpose:
*     Transforms co-ordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_TRANS( NVAL, IN1, IN2, FORWRD, SCS, IDA,
*                     OUT1, OUT2, STATUS )

*  Description:
*     Co-ordinate data are transformed from sky co-ordinates to image
*     co-ordinates, or vice-versa, using the projection information
*     identified by IDA. The direction of the transformation is
*     determined by the argument FORWRD.  If any input co-ordinate
*     values are equal to the Starlink "BAD" value (VAL__BADD) then
*     both the output values are set to the bad value.

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of co-ordinate points to be transformed.
*     IN1( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN1 holds values of the first image
*        co-ordinate (X), otherwise IN1 holds values of the sky
*        longitude.
*     IN2( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN2 holds values of the second image
*        co-ordinate (Y), otherwise IN2 holds values of the sky
*        latitude.
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping is used from image co-ordinate
*        to sky co-ordinate. Otherwise, the inverse mapping from sky
*        co-ordinate to image co-ordinates is used.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the sky co-ordinate system in which sky
*        co-ordinates are required (if FORWRD is true), or supplied (if
*        FORWRD is false). Any unambiguous abbreviation will do. This
*        need not be the same as the SCS identified by IDA.  See ID2
*        section "Sky Coordinates" for more information on Sky
*        Co-ordinate Systems.  A blank value will cause the system
*        associated with IDA to be used.
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     OUT1( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT1 holds values of the sky longitude
*        corresponding to the image co-ordinates given in arrays IN1 and
*        IN2. Otherwise, OUT1 holds values of the first image
*        co-ordinate (X) corresponding to the input sky co-ordinates.
*     OUT2( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT2 holds values of the sky latitude
*        corresponding to the image co-ordinates given in arrays IN1 and
*        IN2. Otherwise, OUT2 holds values of the second image
*        co-ordinate (Y) corresponding to the input sky co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1991 (DSB):
*        Original version.
*     23-APR-1991 (DSB):
*        Orthographic projection included (and "flat" removed).
*     14-FEB-1991 (DSB):
*        Conversion of input sky co-ordinates to projection SCS
*        included.
*     12-FEB-1993 (DSB):
*        Storage of locators in common removed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Read)
*           Julian epoch of observation from the associated AS.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Read)
*           Full projection name from the associated AS.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameter values from the associated AS.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           Full sky co-ordinate system (SCS) name from the associated
*           AS, with optional equinox specifier.

*  Arguments Given:
      INTEGER          NVAL
      DOUBLE PRECISION IN1( NVAL )
      DOUBLE PRECISION IN2( NVAL )
      LOGICAL          FORWRD
      CHARACTER        SCS*(*)
      INTEGER          IDA

*  Arguments Returned:
      DOUBLE PRECISION OUT1( NVAL )
      DOUBLE PRECISION OUT2( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   I                ! Loop count.
      INTEGER   NPREQ            ! No. of projection parameters required
      CHARACTER PROJ*(IRA__SZPRJ)! Full projection name.
      CHARACTER LSCS*(IRA__SZSCS)! Local copy of SCS.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If a blank SCS was given, use the value associated with IDA.
      IF( SCS .EQ. ' ' ) THEN
         LSCS = ACM_SCS( IDA )
      ELSE
         LSCS = SCS
      END IF

*  If the input values are sky co-ordinates, convert them to the
*  sky co-ordinate system used by the projection, and temporarily
*  store them in the output arrays.
      IF( .NOT. FORWRD ) THEN
         CALL IRA_CONVT( NVAL, IN1, IN2, LSCS, ACM_SCS(IDA),
     :                   ACM_EPOCH(IDA), OUT1, OUT2, STATUS )

*  Otherwise copy the input co-ordinates to temporary storage in the
*  output arrays.
      ELSE

         DO I = 1, NVAL
            OUT1( I ) = IN1( I )
            OUT2( I ) = IN2( I )
         END DO

      END IF

*  Identify the projection and get the enumber of projection parameters.
      CALL IRA1_CHPRJ( ACM_PROJN( IDA ), PROJ, NPREQ, STATUS )

*  Call IRA1_IPRJ to do the projection.
      CALL IRA1_IPRJ( NVAL, OUT1, OUT2, FORWRD, PROJ, NPREQ,
     :                ACM_PROJP(1,IDA), OUT1, OUT2, STATUS )

*  If the output values are sky co-ordinates, convert them to the
*  requested sky co-ordinate system.
      IF( FORWRD ) THEN
         CALL IRA_CONVT( NVAL, OUT1, OUT2, ACM_SCS(IDA), LSCS,
     :                   ACM_EPOCH(IDA), OUT1, OUT2, STATUS )
      END IF

*  If an error occurred, give a context message.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_TRANS_ERR1',
     :      'IRA_TRANS: Unable to transform co-ordinate values',
     :      STATUS )
      END IF

      END
