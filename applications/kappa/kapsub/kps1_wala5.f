      SUBROUTINE KPS1_WALA5( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2,
     :                       JB1, JB2, XAMAP, YAMAP, STATUS )
*+
*  Name:
*     KPS1_WALA5

*  Purpose:
*     Calculate input pixel coordinates for WCSALIGN using an AST Mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA5( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2, JB1,
*                      JB2, XAMAP, YAMAP, STATUS )

*  Description:
*     The section (IB1:IB2,JB1:JB2) of the arrays XAMAP and YAMAP are
*     filled using the full projection mappings.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST Mapping from input pixel co-ordinates to reference (i.e.
*        output) pixel co-ordinates.
*     LBNDX = INTEGER (Given)
*        The lower X bound of the output image.
*     UBNDX = INTEGER (Given)
*        The upper X bound of the output image.
*     LBNDY = INTEGER (Given)
*        The lower Y bound of the output image.
*     UBNDY = INTEGER (Given)
*        The upper Y bound of the output image.
*     IB1 = INTEGER (Given)
*        The lower X bound of the section to be filled.
*     IB2 = INTEGER (Given)
*        The upper X bound of the section to be filled.
*     JB1 = INTEGER (Given)
*        The lower Y bound of the section to be filled.
*     JB2 = INTEGER (Given)
*        The upper Y bound of the section to be filled.
*     XAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Given and Returned)
*        Holds the input X pixel coordinate corresponding to the centre
*        of each output pixel.
*     YAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Given and Returned)
*        Holds the input Y pixel coordinate corresponding to the centre
*        of each output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SALIA5.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER MAP
      INTEGER LBNDX
      INTEGER UBNDX
      INTEGER LBNDY
      INTEGER UBNDY
      INTEGER IB1
      INTEGER IB2
      INTEGER JB1
      INTEGER JB2

*  Arguments Given and Returned:
      DOUBLE PRECISION XAMAP( LBNDX:UBNDX, LBNDY:UBNDY )
      DOUBLE PRECISION YAMAP( LBNDX:UBNDX, LBNDY:UBNDY )

*  Status:
      INTEGER STATUS          ! Global status

*  Local Constants:
      INTEGER SIZE            ! Size of working arrays
      PARAMETER ( SIZE = 5000 )

*  Local Variables:
      DOUBLE PRECISION XX( SIZE ) ! X pixel coordinates
      DOUBLE PRECISION YY( SIZE ) ! Y pixel coordinates
      INTEGER FILL            ! Index of the current fill
      INTEGER I               ! Output X pixel index
      INTEGER I0              ! Output X pixel index
      INTEGER J               ! Output Y pixel index
      INTEGER J0              ! Output Y pixel index
      INTEGER K               ! Loop count
      INTEGER N               ! No. of points stored in the work arrays
      INTEGER NFILL           ! No. of times the work arrays are filled
      INTEGER NTOT            ! Total no. of points to be transformed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the row and column number of the first pixel to be
*  transformed.
      J0 = JB1
      J = JB1
      I0 = IB1
      I = IB1

*  Calculate the total no. of points to be transformed.
      NTOT = ( JB2 - JB1 + 1 )*( IB2 - IB1 + 1 )

*  Calculate the number of times this number of points would fill the
*  double precision working arrays.
      NFILL = 1 + ( NTOT - 1 )/SIZE

*  Loop round this number of times.
      DO FILL = 0, NFILL - 1

*  Calculate the number of points included in this "fill".
         N = MIN( SIZE, NTOT - FILL*SIZE )

*  Loop round this number of points, storing the pixel coordinates in
*  the double precision work arrays.
         DO K = 1, N
            YY( K ) = DBLE( J ) - 0.5D0
            XX( K ) = DBLE( I ) - 0.5D0

*  Increment the pixel indices.
            I = I + 1
            IF( I .GT. IB2 ) THEN
               I = IB1
               J = J + 1
            END IF

         END DO

*  Transform the points from output pixel coordinates to input pixel
*  coordinates.
         CALL AST_TRAN2( MAP, N, XX, YY, .FALSE., XX, YY, STATUS )

*  Abort if an error occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now store the input pixel coordinates in the returned arrays.
         DO K = 1, N

            IF( XX( K ) .NE. AST__BAD ) THEN
               XAMAP( I0, J0 ) = DBLE( XX( K ) )
               YAMAP( I0, J0 ) = DBLE( YY( K ) )
            ELSE
               XAMAP( I0, J0 ) = AST__BAD
               YAMAP( I0, J0 ) = AST__BAD
            END IF

*  Increment the pixel indices.
            I0 = I0 + 1
            IF( I0 .GT. IB2 ) THEN
               I0 = IB1
               J0 = J0 + 1
            END IF

         END DO

      END DO

 999  CONTINUE

      END
