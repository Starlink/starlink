      SUBROUTINE PARSECON_PAKNB( LU, N, ARRAY, START, END, STATUS )













*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PAKNB( LU, N, ARRAY, START, END, STATUS )

*  Description:
*     To encode elements of the compiled form of an interface file for
*     a 2-D BYTE array to reduce the file size.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to write to
*     N = INTEGER (Given)
*        The size of the first dimension
*     ARRAY(*) = BYTE (Given)
*        The array of values to be encoded
*     START = INTEGER (Given)
*        The first value of the second dimension to be encoded
*     END = INTEGER (Given)
*        The last value of the second dimension to be encoded
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     Uses BYTE
*     The routine guarantees to handle only values of N up to MAXN,
*       but in fact can handle one or two more without running out of space.

*  Name:
*     PARSECON_PAKNB

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (AJC):
*        Original version.
*     24-MAR-1993 (AJC):
*        Remove include PARSECON_PAR
*        Add DAT_PAR for SUBPAR_CMN
*     01-FEB-2004 (Norman Gray, Starlink/Glasgow):
*        Checked into CVS repository cvs.starlink.ac.uk.  See there for
*        further changes.
*     {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER LU
      INTEGER N
      BYTE ARRAY( N, * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR
      INCLUDE 'PARSECON_ERR'     ! For PARSE__NOMEM

*  Parameters:
*  The routine will certainly handle N <= MAXN, but might handle more.
*  Warn if N is too big, but don't fail unless we actually run out of
*  space below
      INTEGER MAXN
      PARAMETER ( MAXN = 5 )

*  Local Variables:
      BYTE VBUFF( MAXN*SUBPAR__MAXPAR ) ! Values buffer
      INTEGER NBUFF( MAXN*SUBPAR__MAXPAR ) ! Values count buffer
      BYTE LASTV               ! The last value handled
      INTEGER NV                 ! The consecutive value counter
      INTEGER BPT                ! Pointer to next entry in buffers
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise counters
      BPT = 1
      NV = 0
      LASTV = ARRAY( 1, START )

*  Loop through ARRAY from START to END
      DO 20, I = START, END

         DO 10, J = 1, N

            IF( ARRAY( J, I ) .NE. LASTV ) THEN
*           End of consecutive equal values
               IF ( BPT .LE. MAXN*SUBPAR__MAXPAR ) THEN
                  VBUFF( BPT ) = LASTV
                  NBUFF( BPT ) = NV
                  BPT = BPT + 1
                  LASTV = ARRAY( J, I )
                  NV = 1
               ELSE
*               Run off the end of the arrays
                  STATUS = PARSE__NOMEM
                  CALL EMS_SETI ( 'N', N )
                  CALL EMS_SETI ( 'MAXN', MAXN )
                  CALL EMS_REP ( 'PCN_PAK1',
     :               'PARSECON: Exceeded maximum working space'
     :                 // ' (pcn_paknb called with N=^N, MAXN=^MAXN)',
     :               STATUS)
*               JUMP OUT immediately.  Don't bother trying to fix up
*               BPT and write out what we've got -- this is too serious
*               an error.
                  RETURN
               ENDIF
            ELSE
*           Count consecutive values
               NV = NV + 1

            ENDIF

10       CONTINUE

20    CONTINUE

*  End of ARRAY
*  Save the last value and write the record
      VBUFF( BPT ) = LASTV
      NBUFF( BPT ) = NV

      WRITE ( LU ) BPT, (NBUFF(I),I=1,BPT), (VBUFF(I),I=1,BPT)

      END
