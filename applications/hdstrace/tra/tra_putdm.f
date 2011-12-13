      SUBROUTINE TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )
*+
*  Name:
*     TRA_PUTDM

*  Purpose:
*     Appends dimensions to a string at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )

*  Description:
*     The given dimensions are added onto the supplied string at the
*     requested position in a concise format:
*     (%DIMS(1),%DIMS(2)....%DIMS(%NDIM)).

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Array containing the dimensions.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The string onto which the dimensions are to be appended.
*     LENG = INTEGER (Given and Returned)
*        The position in the string at which the dimensions are to be
*        appended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If bad status set then return
*     Append an opening bracket for the list of dimensions.
*     For all the dimensions
*        Append this dimension.
*        If its not the last then
*           Append a comma ( , ).
*        Endif
*     Endfor
*     Append the closing bracket for the list of dimensions.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991 Science & Engineering Research
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
*     DB: Dave Baines (ROE)
*     MJC:Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     06/04/1984 (MJC):
*        Original version.
*     1989 Jun 15 (MJC):
*        Renamed from PUTDIM to avoid confusion with the original TRACE
*        version, and added STATUS argument and initial check; tidied.
*     1991 January 30 (MJC):
*        Converted to the SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      INTEGER
     :  NDIM,                  ! Number of dimensions
     :  DIMS( DAT__MXDIM )     ! Array of dimensions

*  Arguments Given and Returned:
      CHARACTER * ( * )
     :  LINE                   ! String onto which dimensions are to be
                               ! appended
      INTEGER
     :  LENG                   ! Current position in the string

*  Status:
      INTEGER
     :  STATUS                 ! Global status

*  Local Variables:
      INTEGER
     :  I                      ! Index into array of dimensions

*.

*    Check global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Append leading parenthesis to the string for the dimensions.

      CALL CHR_PUTC( '(', LINE, LENG )

*    Loop round for all the dimensions.

      DO I = 1, NDIM

*       Append this dimension to the string.

         CALL CHR_PUTI( DIMS( I ), LINE, LENG )

*       Check if it is the last.

         IF ( I .LT. NDIM ) THEN

*          It is not so append a comma.

            CALL CHR_PUTC( ',', LINE, LENG )
         END IF
      END DO

*    Append the closing parenthesis for the dimensions.

      CALL CHR_PUTC( ')', LINE, LENG )

      END
