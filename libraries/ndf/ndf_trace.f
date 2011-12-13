      SUBROUTINE NDF_TRACE( NEWFLG, OLDFLG )
*+
*  Name:
*     NDF_TRACE

*  Purpose:
*     Set the internal NDF_ system error-tracing flag.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_TRACE( NEWFLG, OLDFLG )

*  Description:
*     The routine sets an internal flag in the NDF_ system which
*     enables or disables error-tracing messages. If this flag is set
*     to .TRUE., then any error occurring within the NDF_ system will
*     be accompanied by error messages indicating which internal
*     routines have exited prematurely as a result. If the flag is set
*     to .FALSE., this internal diagnostic information will not appear
*     and only standard error messages will be produced.

*  Arguments:
*     NEWFLG = LOGICAL (Given)
*        The new value to be set for the error-tracing flag.
*     OLDFLG = LOGICAL (Returned)
*        The previous value of the flag.

*  Notes:
*     -  THIS ROUTINE IS OBSOLETE. The internal error tracing flag
*     (referred to above) corresponds with the TRACE tuning parameter
*     used by NDF_TUNE and NDF_GTUNE, so the same effect can be obtained
*     by substituting these two routines.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-NOV-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     9-MAR-1994 (RFWS):
*        Ensure that the TCB is initialised.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_ETFLG = LOGICAL (Read and Write)
*           Error-tracing flag.

*  Arguments Given:
      LOGICAL NEWFLG

*  Arguments Returned:
      LOGICAL OLDFLG

*  Local Variables:
      INTEGER LSTAT              ! Local status value

*.

*  Initialise the local status value.
      LSTAT = SAI__OK

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( LSTAT )
      IF ( LSTAT .EQ. SAI__OK ) THEN

*  Return the previous value of the error-tracing flag.
         OLDFLG = TCB_ETFLG

*  Set the new value.
         TCB_ETFLG = NEWFLG
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( LSTAT .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_TRACE_ERR',
     :        'NDF_TRACE: Error setting the internal NDF_ system ' //
     :        'error-tracing flag.', LSTAT )
         CALL NDF1_TRACE( 'NDF_TRACE', LSTAT )
      END IF

      END
