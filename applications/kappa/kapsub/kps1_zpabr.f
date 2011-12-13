      SUBROUTINE KPS1_ZPABR( REGION, LINE, COLBND, LINBND, BUFFER,
     :                       LEN, STATUS )
*+
*  Name:
*     KPS1_ZPABx

*  Purpose:
*     Creates a string to be output to an ASCII steering file from
*     ZAPLIN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ZPABx( REGION, LINE, COLBND, LINBND, BUFFER, LEN,
*                      STATUS )

*  Description:
*     This routine is a server for ZAPLIN.  It reports the pixel indices
*     of the bounds of the region to be be zapped.  It also validates
*     the bounds for a line or column region by checking that they do
*     not include the whole array.
*
*     If a region has been zapped the format of the string is:
*        lower_column lower_line upper_column upper_line
*
*     If a line has been zapped the format of the string is:
*        lower_line upper_line  L
*
*     If a column has been zapped the format of the string is:
*        lower_column upper_column  C

*  Arguments:
*     REGION = LOGICAL (Given)
*        If true the string contains lower and upper pixel indices of
*        the region that has been zapped is reported.  If false the
*        line or column pixel-index bounds are written to the string
*        depending on the polarity of LINE.
*        returned.
*     LINE = LOGICAL (Given)
*        If true a string giving the line bounds in pixel indices is
*        created.  If false the string contains the column bounds.
*        However, it is ignored if REGION is true.
*     COLBND( 2 ) = ? (Given)
*        The column bounds of the region that has been zapped (lower
*        then upper).  These are only used if REGION is true or line is
*        false.  These can be data co-ordinates or pixel indices.
*     LINBND( 2 ) = ? (Given)
*        The line bounds of the region that has been zapped (lower
*        then upper).  These are only used if REGION is true or line is
*        true.  These can be data co-ordinates or pixel indices.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        The string containing the bounds of the zapped region.
*     LEN = INTEGER (Returned)
*        The length of the string in characters, ignoring trailing
*        blanks.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     bounds supplied to the routine must have the data type specified.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 29 (MJC):
*        Original version.
*     1991 June 17 (MJC):
*        Converted to generic.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL
     :  REGION,
     :  LINE

      REAL
     :  COLBND( 2 ),
     :  LINBND( 2 )

*  Arguments Returned:
      CHARACTER * ( * )
     :  BUFFER

      INTEGER
     :  LEN

*  Status:
      INTEGER STATUS             ! Global status

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the length.

      LEN = 0

*    Record the region zapped.

      IF ( REGION ) THEN

         CALL CHR_PUTR( COLBND( 1 ), BUFFER, LEN )
         CALL CHR_PUTC( '  ', BUFFER, LEN )
         CALL CHR_PUTR( LINBND( 1 ), BUFFER, LEN )
         CALL CHR_PUTC( '  ', BUFFER, LEN )
         CALL CHR_PUTR( COLBND( 2 ), BUFFER, LEN )
         CALL CHR_PUTC( '  ', BUFFER, LEN )
         CALL CHR_PUTR( LINBND( 2 ), BUFFER, LEN )

*    Record the lines zapped.

      ELSE IF ( LINE ) THEN

         CALL CHR_PUTR( LINBND( 1 ), BUFFER, LEN )
         CALL CHR_PUTC( '  ', BUFFER, LEN )
         CALL CHR_PUTR( LINBND( 2 ), BUFFER, LEN )
         CALL CHR_PUTC( '  L', BUFFER, LEN )

*    Record the columns zapped.

      ELSE

         CALL CHR_PUTR( COLBND( 1 ), BUFFER, LEN )
         CALL CHR_PUTC( '  ', BUFFER, LEN )
         CALL CHR_PUTR( COLBND( 2 ), BUFFER, LEN )
         CALL CHR_PUTC( '  C', BUFFER, LEN )
      END IF

      END
