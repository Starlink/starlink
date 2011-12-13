      SUBROUTINE CCD1_WRIDI( FD, ID, X, Y, P, NP, N, BUFFER, BLEN,
     :                       STATUS )
*+
*  Name:
*     CCD1_WRIDI

*  Purpose:
*     Write identifiers, X, Y and associated data to a list file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_WRIDI( FD, ID, X, Y, P, NP, N, BUFFER, BLEN, STATUS )

*  Description:
*     This routine writes out a number of data lines to a list file:
*     an ID number, an X and Y coordinate, and zero or more associated
*     data values from an array.
*
*     If the output buffer is potentially too short to hold all the
*     values in each column of the array, then a message to this
*     effect will be written, and every line of output will contain
*     the same, reduced, number of items.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor.
*     ID( N ) = INTEGER (Given)
*        The identifiers for the lines.
*     X( N ) = DOUBLE PRECISION (Given)
*        The X coordinates for the lines.
*     Y( N ) = DOUBLE PRECISION (Given)
*        The Y coordinates for the lines.
*     P( NP, N ) = DOUBLE PRECISION (Given)
*        The associated data.  On each line I, all the values P( 1, I )
*        to P( NP, I ) will be written (if there is space).
*     NP = INTEGER (Given)
*        The number of additional data items to be written on each line.
*     N = INTEGER (Given)
*        The number of lines to write to the output file.
*     BUFFER = CHARACTER * ( BLEN ) (Given and Returned)
*        Buffer to hold an output line.
*     BLEN = INTEGER (Given)
*        Length of BUFFER.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER FD
      INTEGER N
      INTEGER NP
      INTEGER ID( N )
      INTEGER BLEN
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      DOUBLE PRECISION P( NP, N )

*  Arguments Given and Returned:
      CHARACTER * ( * ) BUFFER

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of character string

*  Local Variables:
      INTEGER GAP                ! Gap between columns
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in buffer
      INTEGER IEND               ! Last filled position in string
      INTEGER J                  ! Loop variable
      INTEGER NCHAR              ! Number of characters used to encode value
      INTEGER NV                 ! Number of values which can be safely output
      CHARACTER * ( VAL__SZD ) WORD ! Buffer to contain values as a string

*  Local Data:
      DATA GAP / 2 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Work out how many columns we can be sure of outputting.
      NV = MIN( NP, ( BLEN - VAL__SZI + GAP ) / ( VAL__SZD + GAP ) )

*  If that is fewer than requested, warn that this is the case.
      IF ( NV .LT. NP ) THEN
         CALL MSG_SETI( 'NV', NV )
         CALL MSG_SETI( 'NP', NP )
         CALL CCD1_MSG( ' ', '     Warning: can only output ^NV ' //
     :                  'data columns out of ^NP.', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Loop for all output lines.
      DO I = 1, N

*  Clear the output buffer.
         BUFFER = ' '

*  Set the identifier value.
         CALL CHR_ITOC( ID( I ), WORD, NCHAR )

*  Insert it into the output buffer.
         BUFFER( 1 : NCHAR ) = WORD( 1 : NCHAR )

*  Increment position within output buffer.
         IAT = MAX( NCHAR, 8 ) + 2

*  Do the same with the X coordinate.
         CALL CHR_DTOC( X( I ), WORD, NCHAR )
         IEND = IAT + NCHAR
         BUFFER( IAT : IEND ) = WORD( 1 : NCHAR )
         IAT = IAT + MAX( NCHAR, 8 ) + GAP

*  Do the same with the Y coordinate.
         CALL CHR_DTOC( Y( I ), WORD, NCHAR )
         IEND = IAT + NCHAR
         BUFFER( IAT : IEND ) = WORD( 1 : NCHAR )
         IAT = IAT + MAX( NCHAR, 8 ) + GAP

*  Do the same with the data columns.
         DO J = 1, NV
            CALL CHR_DTOC( P( J, I ), WORD, NCHAR )
            IEND = IAT + NCHAR
            BUFFER( IAT : IEND ) = WORD( 1 : NCHAR )
            IAT = IAT + MAX( NCHAR, 8 ) + GAP
         END DO

*  Now write the buffer to file.
         CALL FIO_WRITE( FD, BUFFER( 1 : CHR_LEN( BUFFER ) ), STATUS )
      END DO

      END
* $Id$
