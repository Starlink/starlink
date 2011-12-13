      SUBROUTINE KPS1_TRNVI ( NDIMS, ODIMS, NPTS, CODATA, VADATA,
     :                         PSCALE, COMIN, OUTARR, STATUS )
*+
*  Name:
*     KPS1_TRNVx

*  Purpose:
*     Converts co-ordinate and value arrays of data into an n-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_TRNVx( NDIMS, ODIMS, NPTS, CODATA, VADATA, PSCALE,
*    :                 COMIN, OUTARR, STATUS )

*  Description:
*     This routine transfers the supplied data values whose co-ordinates
*     and values are known to a regularly-spaced n-d output array.  A
*     scaling between pixel separations may be made.  The bin selected
*     is calculated as follows:
*        index = IFIX( ( CODATA - COMIN ) / PSCALE ) + 1

*  Arguments:
*     NDIMS = INTEGER (Given)
*        The number of dimensions of the output array, or in other
*        words the number of co-ordinates.
*     ODIMS( NDIMS ) = INTEGER (Given)
*        The dimensions of the output array when stored as an n-d
*        array.
*     NPTS = INTEGER (Given)
*        Number of input data points.
*     CODATA( NDIMS, NPTS ) = REAL (Given)
*        Co-ordinates of each data point. Each line corresponds to a
*        new pixel and the order matches that of the data values in
*        %VADATA.
*     VADATA( NPTS ) = ? (Given)
*        Input data array containing pixel values to be transferred.
*     PSCALE( NDIMS ) = REAL (Given)
*        Pixel-to-pixel distance in co-ordinate units.
*     COMIN( NDIMS ) = REAL (Given)
*        Minimum co-ordinate values.  The pixel index at a minimum
*        co-ordinate is one.
*     OUTARR( * ) = ? (Returned)
*        Output array containing the input data, stored as a vector.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Notes:
*     -  This is a server subroutine for TRANDAT.  The code would be at
*        the top level (i.e. TRANDAT) but for the requirement to
*        transfer ASCII-file data into mapped work arrays.
*     -  There is a routine for integer and floating-point data types:
*        replace "x" in the routine name by I, D, or R as appropriate.

*  Algorithm:
*     -  Find the product of the number of elements within succeeding
*     dimensions.
*     -  Initialise the output array.
*     -  For all points compute the pixel index (after the offset and
*     scaling) for each dimension, and then convert to a pixel index in
*     the output vector array. Set the value of the pixel.

*  Prior Requirements:
*     -  The output array should be initialised to zero or the magic
*     value when created.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Jun 18 (MJC):
*        Original loosely based on the 2-d version of the
*        routine, then called TRNDSB (RAL::CUR).
*     1991 May 27 (MJC):
*        Corrected the calculation of pixel index by removing a
*        superfluous + 0.5 from the IFIX calculation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! SSE global definitions
      INCLUDE 'DAT_PAR'           ! Data-system constants

*  Arguments Given:
      INTEGER
     :  NDIMS,
     :  ODIMS( NDIMS ),
     :  NPTS

      REAL
     :  PSCALE( NDIMS ),
     :  CODATA( NDIMS, NPTS ),
     :  COMIN( NDIMS )

      INTEGER
     :  VADATA( NPTS )

*  Arguments Returned:
      INTEGER
     :  OUTARR( * )

*  Status:
      INTEGER  STATUS

*  Local Variables:
      INTEGER
     :  COORD,                 ! Co-ordinates of output array pixel
     :  FDIMS( DAT__MXDIM ),   ! Product of the dimensions
     :  J, K,                  ! Counters
     :  NELM,                  ! Number of elements in the output array
     :  VECPOS                 ! Element number of the pixel in the
                               ! vectorised array

      REAL
     :  CURPOS                 ! Current position values

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the number of elements in the output array, and the number
*    in the lower dimensions.

      NELM = 1
      DO  J = 1, NDIMS
         FDIMS( J ) = NELM
         NELM = NELM * ODIMS( J )
      END DO

*    Loop round all the points in INDATA, getting out the co-ordinate
*    and data values for each one.

      DO  K = 1, NPTS
         DO  J = 1, NDIMS

            CURPOS = CODATA( J, K )

*          Calculate the pixel "co-ordinate" of the current data point.

            COORD = IFIX( ( ( CURPOS - COMIN( J ) ) / PSCALE( J ) ) ) +
     :              1

*          Find the position within the vector.

            IF ( J .EQ. 1 ) THEN
               VECPOS = COORD
            ELSE
               VECPOS = VECPOS + ( COORD - 1 ) * FDIMS( J )
            END IF
         END DO

*       Now set the requisite output array pixel to the correct value.

         OUTARR( VECPOS ) = VADATA( K )
      END DO

*    Return and end.

      END
