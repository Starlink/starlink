      SUBROUTINE TRA_ARELM( NELEM, NDIM, DIMS, CDIMS, STATUS )
*+
*  Name:
*     TRA_ARELM

*  Purpose:
*     Calculate dimension indices for a vectorised array element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA_ARELM( NELEM, NDIM, DIMS, CDIMS, STATUS )

*  Description:
*     The indices, %CDIMS, to the %NELEM'th element of the vectorised
*     array with actual dimensionality %NDIM and actual dimensions
*     %DIMS are calculated.

*  Arguments:
*     NELEM = INTEGER (Given)
*        Number of the element for which the dimension indices are to
*        be calculated.
*     NDIM = INTEGER (Given)
*        Dimensionality of the array under consideration.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Dimensions of the array under consideration.
*     CDIMS( DAT__MXDIM ) = INTEGER (Returned)
*        Dimension indices for the NELEM'th element.
*     STATUS = INTEGER (Given)
*        The global status.

*  Algorithm:
*     Adjust the vectorised index by subtracting 1 because it is easier
*       to calculate the dimension indices if the first element of the
*       array is treated as being indexed as 0,0,0...
*     For all the dimensions
*        Index in this dimension to the NELEM'th element of the array is
*          the remainder of the division of VALUE by this dimension of
*          the array
*        1 is added to restore first element to 1,1,1...
*        Modify VALUE to take into account that have dealt with this
*          dimension
*     Endfor

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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15/03/1984 (DB):
*        Original version.
*     07/04/1984 (DB):
*        Documentation revised.
*     1989 May 16 (MJC):
*        Added STATUS argument and tidied.
*     1989 Jun 15 (MJC):
*        Renamed from ARELEM to avoid confusion with the original TRACE
*        version.
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
     :  NELEM,                 ! No. of element for which dim. indices
                               ! are to be calculated.
     :  NDIM,                  ! Dimensionality of array under
                               ! consideration.
     :  DIMS( DAT__MXDIM )     ! Dimensions of the array under
                               ! consideration.

*  Arguments Returned:
      INTEGER
     :  CDIMS( DAT__MXDIM )    ! Dimension indices for the NELEM'th
                               ! element

*  Status:
      INTEGER
     :  STATUS

*  Local Variables:
      INTEGER
     :  VALUE,                 ! Holds running value for index
                               ! calculation
     :  INDEX                  ! Index to dimensions

*.

*    Check global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    It is easier to calculate dimensions if first element is 0,0,0...

      VALUE = NELEM - 1

*    Loop round for all the dimensions.

      DO INDEX = 1, NDIM

*       INDEX'th index to NELEM'th element of the array is the remainder
*       of the division of VALUE by the INDEX'th dimension of the array.
*       1 is added to restore first element to 1,1,1...

         CDIMS( INDEX ) = MOD( VALUE, DIMS( INDEX ) ) + 1

*       Modify VALUE to take into account that have dealt with
*       INDEX'th dim.

         VALUE = VALUE / DIMS( INDEX )
      END DO

      END
