      SUBROUTINE CCD1_LSFUN1( M, N, XC, FVECC, IFAIL )
*+
*  Name:
*     CCD1_LSFUN1

*  Purpose:
*     Returns the least squares values to E04FDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LSFUN1( M, N, XC, FVECC, IFAIL )

*  Description:
*     This routine calculates the squared difference between the
*     current parameterised position and the reference positions.
*     It is used EXCLUSIVELY by CCD1_GFIT.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals (=CCD1_NREC * 2).
*     N = INTEGER (Given)
*        The number of parameters (variables) =(CCD1_NPAR)
*     XC( N ) = DOUBLE PRECISION (Given)
*        The values of the parameters.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        The squared differeces.
*     IFAIL = INTEGER (Given and Returned)
*        Flag to stop calling routine if required (see PDA_LMDIF1
*        description).

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1992 (PDRAPER):
*        Original version.
*     17-SEP-1996 (PDRAPER):
*        Now called by PDA_LMDIF1, rather than E04FDF.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'CCD1_FITCM'       ! Common block for passing fit
                                 ! information.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CCD1_IPPIN = INTEGER
*           Pointer to array to hold all the parameter values as
*           variables on input to the transformation.
*        CCD1_IPPO = INTEGER
*           Pointer to array to hold the output (transformed)
*           X and Y positions.
*        CCD1_IPX = INTEGER
*           Pointer to X reference positions.
*        CCD1_IPY = INTEGER
*           Pointer to Y reference positions.
*        CCD1_NDXY = INTEGER
*           Size of first dimension of CCD1_IPX and CCD1_IPY arrays
*           when declared.
*        CCD1_NREC = INTEGER
*           Number of X and Y values.
*        CCD1_NPAR = INTEGER
*           Number of parameters in forward transformation.
*        CCD1_IDTR = INTEGER
*           TRANSFORM identifier.

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )
      INTEGER IFAIL

*  Local Variables:
      INTEGER STATUS             ! Local status

*.

*  Initialise STATUS
      STATUS = SAI__OK

*  Add the current parameter estimates to the input array prior to
*  transformation.
      CALL CCD1_ITRA( XC, %VAL( CNF_PVAL( CCD1_IPX ) ),
     :                %VAL( CNF_PVAL( CCD1_IPY ) ),
     :                CCD1_NDXY, CCD1_NREC, CCD1_NPAR + 2, .FALSE.,
     :                %VAL( CNF_PVAL( CCD1_IPPIN ) ), STATUS )

*  Transform the data.
       CALL TRN_TRND( .FALSE., CCD1_NREC, CCD1_NPAR + 2, CCD1_NREC,
     :                %VAL( CNF_PVAL( CCD1_IPPIN ) ),
     :                CCD1_IDTR, CCD1_NREC, 2,
     :                %VAL( CNF_PVAL( CCD1_IPPO ) ), STATUS )

*  Now form the differences.
      CALL CCD1_LSFUNS( %VAL( CNF_PVAL( CCD1_IPX ) ),
     :                  %VAL( CNF_PVAL( CCD1_IPY ) ),
     :                  %VAL( CNF_PVAL( CCD1_IPPO ) ),
     :                  CCD1_NREC, FVECC, STATUS )

      END

* $Id$
