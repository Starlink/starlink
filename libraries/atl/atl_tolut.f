      SUBROUTINE ATL_TOLUT( INMAP, XLO, XHI, DX, OPTS, OUTMAP, STATUS )
*+
*  Name:
*     ATL_TOLUT

*  Purpose:
*     Approximate a supplied Mapping by one or more LutMaps.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_TOLUT( INMAP, XLO, XHI, DX, OPTS, OUTMAP, STATUS )

*  Description:
*     This routine creates a Mapping that uses one or more LutMaps to
*     approximate the supplied Mapping. The supplied Mapping must have 1
*     input but can have up to ATL__MXDIM outputs. One LutMap will be
*     created for each output and combined in parallel in the output
*     Mapping. The range of input VALUE over which the approximation is
*     to be valid is specified, together with the input step size for
*     the LutMaps.

*  Arguments:
*     INMAP = INTEGER (Given)
*        The Mapping to be approximated. Must have only 1 input, and up
*        to ATL__MXDIM outputs.
*     XLO = DOUBLE PRECISION (Given)
*        The lowest value of the INMAP input value for which the returned
*        Mapping will be used.
*     XHI = DOUBLE PRECISION (Given)
*        The highest value of the INMAP input value for which the returned
*        Mapping will be used.
*     DX = DOUBLE PRECISION (Given)
*        The increment in INMAP input value to be used when creating the
*        LutMaps.
*     OPTS = CHARACTER * ( * ) (Given)
*        Options to pass to the LutMap constructor.
*     OUTMAP = INTEGER (Returned)
*        An AST pointer to the returned Mapping, or AST__NULL if no Mapping
*        could be created. This will have the same number of inputs and
*        outputs as INMAP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     18-APR-2007 (DSB):
*        Original version.
*     25-APR-2007 (DSB):
*        Add missing argument NOUT in call to ATL1_TOLUT.
*     30-APR-2007 (DSB):
*        Added OPTS argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ATL_PAR'          ! ATL constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CNF_PAR'          ! CNF constants and functions

*  Arguments Given:
      INTEGER INMAP
      DOUBLE PRECISION XLO
      DOUBLE PRECISION XHI
      DOUBLE PRECISION DX
      CHARACTER OPTS*(*)

*  Arguments Returned:
      INTEGER OUTMAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER IPW1
      INTEGER IPW2
      INTEGER LUTSIZ
      INTEGER NOUT
*.

*  Initialise.
      OUTMAP = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mapping has only 1 input.
      IF( AST_GETI( INMAP, 'Nin', STATUS ) .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NIN', AST_GETI( INMAP, 'Nin', STATUS ) )
         CALL ERR_REP( ' ', 'ATL_TOLUT: Supplied Mapping has ^NIN '//
     :                 'inputs - only 1 input is allowed (programming'//
     :                 ' error).', STATUS )
         GO TO 999
      END IF

*  Check the supplied Mapping has no ore than ATL__MXDIM outputs.
      NOUT = AST_GETI( INMAP, 'Nout', STATUS )
      IF( NOUT .GT. ATL__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NOUT', NOUT )
         CALL MSG_SETI( 'MX', ATL__MXDIM )
         CALL ERR_REP( ' ', 'ATL_TOLUT: Supplied Mapping has ^NOUT '//
     :                 'outputs - up to ^MX are allowed (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Check the step size is not zero.
      IF( DX .EQ. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'ATL_TOLUT: Supplied step size is zero '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Check the range is not zero.
      IF( XHI .EQ. XLO ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'ATL_TOLUT: Supplied upper and lower '//
     :                 'bounds are equal (programming error).',
     :                 STATUS )
         GO TO 999
      END IF

*  Calculate the number of entries in each LutMap.
      LUTSIZ = INT( ABS( ( XHI - XLO )/DX ) ) + 1

*  Create a work array large enough to hold the X value at every LUT sample
      CALL PSX_CALLOC( LUTSIZ, '_DOUBLE', IPW1, STATUS )

*  Create a work array large enough to hold the result of transforming
*  these positions using the supplied Mapping.
      CALL PSX_CALLOC( LUTSIZ*NOUT, '_DOUBLE', IPW2, STATUS )

*  Create the required Mapping.
      CALL ATL1_TOLUT( INMAP, NOUT, XLO, DX, LUTSIZ, OPTS,
     :                 %VAL( CNF_PVAL( IPW1 ) ),
     :                 %VAL( CNF_PVAL( IPW2 ) ), OUTMAP, STATUS )

*  Free workspae.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

 999  CONTINUE

*  Return a null AST identifier if anything went wrong.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( OUTMAP, STATUS )

      END



      SUBROUTINE ATL1_TOLUT( INMAP, NOUT, XLO, DX, LUTSIZ, OPTS, W1, W2,
     :                       OUTMAP, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ATL_PAR'          ! ATL constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER INMAP              ! Input Mapping to be approximated
      INTEGER NOUT               ! Number of outputs for INMAP
      DOUBLE PRECISION XLO       ! Lowest INMAP input value
      DOUBLE PRECISION DX        ! INMAP input value step size
      INTEGER LUTSIZ             ! No. of points in each LutMap
      CHARACTER OPTS*(*)         ! Options for LutMap constructor

*  Arguments Returned:
      DOUBLE PRECISION W1( LUTSIZ ) ! Work space
      DOUBLE PRECISION W2( LUTSIZ, NOUT ) ! Work space
      INTEGER OUTMAP             ! Returned Mapping

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER INPRM
      INTEGER MAP
      INTEGER NEWMAP
      INTEGER OUTPRM( ATL__MXDIM )

      DATA INPRM  / 1 /,
     :     OUTPRM / ATL__MXDIM*1 /

*.

*  Initialise.
      OUTMAP = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the equally spaced input values.
      DO I = 1, LUTSIZ
         W1( I ) = XLO + ( I - 1 )*DX
      END DO

*  Transform these into the output coordinate system.
      CALL AST_TRANN( INMAP, LUTSIZ, 1, LUTSIZ, W1, .TRUE., NOUT,
     :                LUTSIZ, W2, STATUS )

*  Loop round each output axis.
      DO I = 1, NOUT

*  Create a LutMap which transform the INMAP input value into the curent
*  output axis value.
         MAP = AST_LUTMAP( LUTSIZ, W2( 1, I ), XLO, DX, OPTS,
     :                     STATUS )

*  If this is the first axis, just use the above LutMap as the returned
*  Mapping.
         IF( I .EQ. 1 ) THEN
            OUTMAP = MAP

*  Otherwise add the LutMap into the returned Mapping, in parallel.
         ELSE
            NEWMAP = AST_CMPMAP( OUTMAP, MAP, .FALSE., ' ', STATUS )
            CALL AST_ANNUL( MAP, STATUS )
            CALL AST_ANNUL( OUTMAP, STATUS )
            OUTMAP = NEWMAP
         END IF

      END DO

*  The above CmpMap will have NOUT inputs. If this is more than 1, we need
*  to preceed the above CmpMap with a PermMap that outputs NOUT copies of
*  its 1 input.
      IF( NOUT .GT. 1 ) THEN
         MAP = AST_PERMMAP( 1, INPRM, NOUT, OUTPRM, 0.0D0, ' ', STATUS )
         NEWMAP = AST_CMPMAP( MAP, OUTMAP, .TRUE., ' ', STATUS )
         CALL AST_ANNUL( MAP, STATUS )
         CALL AST_ANNUL( OUTMAP, STATUS )
         OUTMAP = NEWMAP
      END IF

      END
