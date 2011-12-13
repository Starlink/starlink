      SUBROUTINE KPS1_LFTSV( ORDER, A, INITA, B, WRK1, WRK2, STATUS )
*+
*  Name:
*     KPG1_LFTSV

*  Purpose:
*     Solve least-squares equations for fitting a polynomial.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LFTSV( ORDER, A, INITA, B, WRK1, WRK2, STATUS )

*  Description:
*     This routine solves a system of linear equations in a least-
*     squares sense for the FITLINE application.

*  Arguments:
*     ORDER = INTEGER (Given)
*        The order of polynomial which will be fit using these matrices.
*     A( ORDER + 1, ORDER + 1 ) = DOUBLE PRECISION (Given and Returned)
*        Matrix holding power X sums (A in Ax=B).
*     INITA = LOGICAL (Given)
*        If .TRUE., then the A matrix should factored.  If .FALSE., it
*        has already been factored on a previous call and can be reused
*        to solve another B immediately.  When reusing A you must also
*        return the same WRK2.
*     B( ORDER + 1 ) = DOUBLE PRECISION (Given and Returned)
*        Y times power X sums (B in Ax=B).  On exit it contains the
*        polynomial coefficients.
*     WRK1( * ) = DOUBLE PRECISION (Given and Returned)
*        Workspace of size (ORDER+1).
*     WRK2( * ) = INTEGER (Given and Returned)
*        Workspace of size (ORDER+1).  Re-used when INITA is .FALSE.

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
*     20-SEP-2005 (PWD):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variables.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global status:
      INTEGER STATUS

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard constants

*  Arguments Given:
      INTEGER ORDER
      LOGICAL INITA

*  Arguments Given and Returned:
      DOUBLE PRECISION A( ORDER + 1, ORDER + 1 )
      DOUBLE PRECISION B( ORDER + 1 )
      DOUBLE PRECISION WRK1( ORDER + 1 )
      INTEGER WRK2( ORDER + 1 )

*  Local Variables:
      INTEGER IND
      INTEGER ITASK

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( INITA ) THEN
         ITASK = 1
      ELSE
         ITASK = 2
      END IF

*  Let PDA take the strain.
      CALL PDA_DGEFS( A, ORDER + 1, ORDER + 1, B, ITASK, IND,
     :                WRK1, WRK2, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END
