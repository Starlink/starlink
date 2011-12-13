      DOUBLE PRECISION FUNCTION KPS1_POLYV( ORDER, C, X )
*+
*  Name:
*     KPG1_POLYV

*  Purpose:
*     Evaluate a polynomial at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPS1_POLYV( ORDER, C, X )

*  Description:
*     This routine evaluates a polynomial of the given order and
*     coefficients at the selected position.

*  Arguments:
*     ORDER = INTEGER (Given)
*        The order of polynomial.
*     C( ORDER + 1 ) = DOUBLE PRECISION (Given)
*        The polynomial coefficients.
*     X = DOUBLE PRECISION (Given)
*        The position at which to evaluate the polynomial.

*  Result:
*     The value of the polynomial.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-2005 (PWD):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      INTEGER ORDER
      DOUBLE PRECISION C( ORDER + 1 )
      DOUBLE PRECISION X

*  Local variables:
      INTEGER I                 ! Do-loop increment variables

*.

*  Evaluate fit for the given coordinate. Use Horner's Rule for speed
*  by avoiding evaluation of powers.
      KPS1_POLYV = C( ORDER + 1 )
      DO I = ORDER, 1, -1
         KPS1_POLYV = KPS1_POLYV * X + C( I )
      END DO
      END
