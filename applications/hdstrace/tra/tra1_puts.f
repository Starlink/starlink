      SUBROUTINE TRA1_PUTS( NDIM, DIMS, IVALUE, VALUE, COMMA, INDS,
     :                      LINE, LENG, FULL, STATUS )
*+
*  Name:
*     TRA1_PUTS

*  Purpose:
*     Put the supplied value string in the supplied text line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL TRA1_PUTS( NDIM, DIMS, IVALUE, VALUE, COMMA, INDS, LINE,
*    :                LENG, FULL, STATUS )

*  Description:
*     The supplied value string is planted in the text line followed by
*     an optional comma.  The line length and array indices are updated
*     as appropriate.  The routine determines whether or not there is
*     enough room to plant the value string, and returns a logical flag
*     stating what has happened.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Dimensionality of the object.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Dimensions of the object.
*     IVALUE = INTEGER (Given)
*        Index to the value if the object is a vector.
*     VALUE = CHARACTER * ( * ) (Given)
*        Value string.
*     COMMA = LOGICAL (Given)
*        If true a comma is written after the value.
*     INDS = CHARACTER (Given and Returned)
*        Array indices
*     LINE = CHARACTER*(*) (Given and Returned)
*        Line of text to be appended
*     LENG = INTEGER (Given and Returned)
*        Current length of the characters in LINE excluding any trailing
*        blanks.
*     FULL = LOGICAL (Returned)
*        If true the line given was full.
*     STATUS = INTEGER (Given)
*        Global status

*  Implementation Deficiencies:
*     At present, this ignores the true dimensionality, and produces
*     a list of values seperated by commas.

*  Copyright:
*     Copyright (C) 1983, 1989, 1991 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     JRG: Jack Giddings (UCL)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1983 (JRG):
*        Original version.
*     1989 May 16 (MJC):
*        Tidied and extended the prologue; added COMMA argument.
*     1989 Jun 15 (MJC):
*        Renamed from LSDIR to avoid confusion with the original TRACE
*        version; added STATUS argument and check.
*     1991 January 30 (MJC):
*        Converted to SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing


*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SAI Constants


*  Arguments Given:
      INTEGER NDIM             ! Number of dimensions
      INTEGER DIMS(*)          ! Dimensions
      INTEGER IVALUE           ! Value index as if object is vector
      CHARACTER * ( * ) VALUE  ! Value string
      LOGICAL COMMA            ! True if a comma is to be appended to
                               ! value


*  Arguments Given and Returned:
      INTEGER INDS( * )        ! Array indices
      CHARACTER * ( * ) LINE   ! Line to receive numbers
      INTEGER LENG             ! Current line length

*    Export :                  ! True if:
      LOGICAL FULL             ! Line was found full


*  Status:
      INTEGER STATUS           ! Global status


*  External References:
      INTEGER CHR_LEN          ! String length
      INTEGER CHR_SIZE         ! String size


*  Local Variables:
      INTEGER NCHAR            ! Value string length
      INTEGER NEED             ! Space needed

*.

*    Check global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the number of characters needed allowing for the ellipsis and
*    a prefixing comma if it is not the first element of an array.

      NCHAR = CHR_LEN( VALUE )
      NEED = NCHAR + 3
      IF ( COMMA ) THEN
         NEED = NEED + 1
      END IF

*    See if there is space in the line.

      FULL = ( CHR_SIZE( LINE ) - LENG ) .LE. NEED

      IF ( .NOT. FULL ) THEN

*       There is room to accommodate the value, so append it to the
*       line.  Append the comma if required.

         CALL CHR_PUTC( VALUE( 1:NCHAR ), LINE, LENG )
         IF ( COMMA ) THEN
            CALL CHR_PUTC( ',', LINE, LENG )
         END IF
      END IF

      END
