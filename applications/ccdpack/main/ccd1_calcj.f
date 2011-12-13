      SUBROUTINE CCD1_CALCJ( M, N, XC, FJACC, LJAC )
*+
*  Name:
*     CCD1_CALCJ

*  Purpose:
*     Calculates the jacobian matrix for the non-linear least squares
*     problem of CCD1_SZSLV.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CALCJ( M, N, XC, FJACC, LJAC )

*  Description:
*      This routine calculates the jacobian matrix of partial derivates
*      for the non-linear least squares equations solved by the routine
*      CCD1_SZSLV. It uses the information stored in commons blocks about
*      the pair-wise relative scale and zero point corrections that is
*      created for the PDA_DQED solver.

*  Arguments:
*     M = INTEGER (Given)
*        Number of residuals.
*     N = INTEGER (Given)
*        Number of unknowns.
*     XC( N ) = DOUBLE PRECISION (Given)
*        Value of the unknowns for which derivatives are required.  The
*        first N/2 of these will be the logarithmic scale factor
*        corrections for each data array; the second N/2 will be the
*        zero point corrections.
*     FJACC( LJAC, N ) = DOUBLE PRECISION (Returned)
*        Jacobian array of partial derivatives. This
*        gives the partial derivative of each residual with respect to
*        each correction, evaluated with the current set of
*        corrections.
*     LJAC = INTEGER (Given)
*        First dimension of FJACC. Should be at least M.

*  Notes:
*     This routine reads data from global variables held in common.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: P.W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     30-APR-1992 (RFWS):
*        Original version.
*     4-AUG-1992 (RFWS):
*        Added ability to constrain the corrections applied to the
*        "reference data array".
*     14-AUG-1992 (RFWS):
*        Installed scaling factors for the variables being optimised
*        and converted the zero point corrections to refer to the mean
*        false origin values stored in global variables.
*     23-SEP-1996 (PDRAPER):
*        Version for CCDPACK covariance matrix created from the bones
*        of CCD1_SZLSF during NAG removal.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS

*  Global Variables:
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS
*        CCD1_DIFS( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read)
*           Array of observed logarithmic scale factor differences.
*        CCD1_DIFZ( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read)
*           Array of observed zero point differences.
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Read)
*           Array of pairs of indices identifying which two data arrays
*           contributed to which observed scale factor and zero point
*           difference.
*        CCD1_IREF = INTEGER (Read)
*           Index of the "reference data array" to which the optimised
*           scale factor and zero point corrections should be
*           normalised (set to zero if there is no reference data
*           array).
*        CCD1_MORIG( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array giving the "mean false origin" values for each data
*           array, to which the zero point corrections being optimised
*           are referred.
*        CCD1_ORIG( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array giving the false origin values used when obtaining
*           the scale factor and zero point differences.
*        CCD1_RNG1 = DOUBLE PRECISION (Read)
*           Expected range of logarithmic scale factor corrections
*           (used as scaling factor for these corrections).
*        CCD1_RNG2 = DOUBLE PRECISION (Read)
*           Expected range of zero point corrections (used as scaling
*           factor for these corrections).
*        CCD1_WT1( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of weighting factors to be applied to each
*           logarithmic scale factor residual.
*        CCD1_WT2( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of weighting factors to be applied to each zero point
*           residual.

*  Arguments Given:
      INTEGER M
      INTEGER N
      INTEGER LJAC
      DOUBLE PRECISION XC( N )

*  Arguments Returned:
      DOUBLE PRECISION FJACC( LJAC, N )

*  Local Variables:
      DOUBLE PRECISION DS        ! Expected log scale factor difference
      DOUBLE PRECISION DZ        ! Expected zero point difference
      DOUBLE PRECISION S1        ! Log scale factor correction 1
      DOUBLE PRECISION S2        ! Log scale factor corection 2
      DOUBLE PRECISION Z1        ! Zero point correction 1
      DOUBLE PRECISION Z2        ! Zero point correction 2
      INTEGER I                  ! Loop counter
      INTEGER I1                 ! Index of data array 1
      INTEGER I2                 ! Index of data array 2
      INTEGER J                  ! Loop counter
      INTEGER NCMP               ! Number of inter-comparisons
      INTEGER NIN                ! Number of data arrays

*.

*  Determine the number of data arrays and the number of
*  inter-comparisons which were made.
      NCMP = M / 2
      NIN = N / 2

*  Initialise the Jacobian matrix to zero.
      DO 2 J = 1, N
         DO 1 I = 1, M
            FJACC( I, J ) = 0.0D0
 1       CONTINUE
 2    CONTINUE

*  Loop to consider each residual. First identify the pair of data
*  arrays used in each inter-comparison.
      DO 3 I = 1, NCMP
         I1 = CCD1_IPAIR( 1, I )
         I2 = CCD1_IPAIR( 2, I )

*  Extract the current estimate of the logarithm of the scale factor
*  correction for each of these data arrays, correcting for the scaling
*  factor used to normalise the values handled by NAG. Derive the
*  expected logarithm of the relative scale factor when the two
*  un-corrected data arrays are inter-compared.
         S1 = XC( I1 ) * CCD1_RNG1
         S2 = XC( I2 ) * CCD1_RNG1
         DS = S2 - S1

*  Form the associated derivatives if required.
         FJACC( I, I1 ) = - CCD1_WT1( I ) * CCD1_RNG1
         FJACC( I, I2 ) = CCD1_WT1( I ) * CCD1_RNG1

*  Extract the current estimate of the zero point corrections for the
*  two data arrays (allow for scaling factors and for each correction
*  being referred to different false origins as given by CCD1_MORIG).
*  Derive the expected relative zero point when the two un-corrected
*  data arrays are inter-compared
         Z1 = XC( I1 + NIN ) * CCD1_RNG2 - CCD1_MORIG( I1 ) * EXP( S1 )
         Z2 = XC( I2 + NIN ) * CCD1_RNG2 - CCD1_MORIG( I2 ) * EXP( S2 )
         DZ = ( Z2 - Z1 ) * EXP( - S1 )

*  Correct the expected relative zero point for the false origin used
*  when the data arrays were actually inter-compared.
         DZ = DZ + CCD1_ORIG( I ) * EXP( DS )

*  Form the associated derivatives.
         FJACC( I + NCMP, I1 ) = - CCD1_WT2( I ) * CCD1_RNG1 *
     :        ( DZ - CCD1_MORIG( I1 ) )
         FJACC( I + NCMP, I2 ) = CCD1_WT2( I ) * CCD1_RNG1 *
     :        EXP( DS ) * ( CCD1_ORIG( I ) - CCD1_MORIG( I2 ) )
         FJACC( I + NCMP, I1 + NIN ) = - CCD1_WT2( I ) *
     :        EXP( - S1 ) * CCD1_RNG2
         FJACC( I + NCMP, I2 + NIN ) = CCD1_WT2( I ) *
     :        EXP( - S1 ) * CCD1_RNG2
 3    CONTINUE
      END
* $Id$
