      SUBROUTINE CCD1_SZPRD( MODE, N, NIM, X, Y, LIW, LW, IW, W )
*+
*  Name:
*     CCD1_SZPRD

*  Purpose:
*     Form matrix product when finding scale factor or zero point
*     corrections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SZPRD( MODE, N, NIM, X, Y, W, LW, IW, LIW )

*  Description:
*     This is an instance of the APROD service routine required by the
*     PDA library routine PDA_LSQR which solves sparse linear
*     least-squares problems. It is configured for use when solving for
*     a set of consistent logarithmic scale factor or zero point
*     corrections for overlapping data arrays which have been
*     inter-compared in pairs.  Its form is determined by the
*     requirements of the PDA library.

*  Arguments:
*     MODE = INTEGER (Given)
*        Specifies the operation to be performed.
*     N = INTEGER (Given)
*        Number of elements in the "right hand side" vector (one more
*        than the number of data array inter-comparisons made).
*     NIM = INTEGER (Given)
*        Number of data arrays.
*     X( N ) = DOUBLE PRECISION (Given and Returned)
*        The X vector supplied by the routine PDA_LSQR, to be updated.
*        This will contain values of the logarithmic scale factor or
*        zero point corrections for each data array.
*     Y( M ) = DOUBLE PRECISION (Given and Returned)
*        The Y vector supplied by the routine PDA_LSQR, to be updated.
*        This will contain values of the logarithmic scale factor or
*        zero point differences between pairs of data arrays (the data
*        arrays concerned being identified in the CCD1_IPAIR global
*        array).
*     W( LW ) = DOUBLE PRECISION (Given)
*        Not used.
*     LW = INTEGER (Given)
*        Not used.
*     IW( LIW ) = INTEGER (Given)
*        Not used.
*     LIW = INTEGER (Given)
*        Not used.

*  Notes:
*     This routine reads data from global variables stored in common.

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
*     28-FEB-1992 (RFWS):
*        Original version.
*     22-APR-1992 (RFWS):
*        Changed to access global variables held in common.
*     5-AUG-1992 (RFWS):
*        Added constraints on the mean correction or the correction
*        applied to the "reference data array".
*     19-SEP-1996 (PDRAPER):
*        Changed for use by PDA_LSQR, rather than F04QAF.
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
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Read)
*           Array containing pairs of integers which identify which two
*           data arrays were inter-compared to produce each logarithmic
*           scale factor or zero point difference.
*        CCD1_IREF = INTEGER (Read)
*           Index of the "reference data array" to which the
*           corrections should be normalised (set to zero if there is
*           no reference data array).
*        CCD1_WT1( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of weights to apply to each residual. These are
*           multiplied into the logarithmic scale factor or zero point
*           differences before invoking F04QAF and are also multiplied
*           into the results of the sparse matrix multiplication
*           performed by this routine (see NAG documentation).  This
*           has the effect of weighting the residuals as required.

*  Arguments Given:
      INTEGER MODE
      INTEGER N
      INTEGER NIM
      INTEGER LW
      DOUBLE PRECISION W( LW )
      INTEGER LIW
      INTEGER IW( LIW )

*  Arguments Given and Returned:
      DOUBLE PRECISION X( NIM )
      DOUBLE PRECISION Y( N )

*  Local Variables:
      INTEGER NCMP               ! No. of data array inter-comparisons
      INTEGER I                  ! Loop counter
      INTEGER I1                 ! First constraint index
      INTEGER I2                 ! Last constraint index

*.

*  Determine the number of data array inter-comparisons made.
      NCMP = N - 1

*  Set up indices for the data arrays which will contribute to
*  constrain the corrections; either the mean correction is constrained
*  to be zero (all corrections contribute) or the correction for the
*  "reference data array" is constrained to be zero.
      IF ( ( CCD1_IREF .LT. 1 ) .OR. ( CCD1_IREF .GT. NIM ) ) THEN
         I1 = 1
         I2 = NIM
      ELSE
         I1 = CCD1_IREF
         I2 = CCD1_IREF
      END IF

*  Evaluate the matrix product as required by the NAG routine F04QAF.

*  If MODE is 1, modify Y using the difference data.
      IF ( MODE .EQ. 1 ) THEN
         DO 1 I = 1, NCMP
            Y( I ) = Y( I ) + CCD1_WT1( I ) *
     :                        ( X( CCD1_IPAIR( 2, I ) ) -
     :                          X( CCD1_IPAIR( 1, I ) ) )
 1       CONTINUE

*  Add constraints on the mean correction. These constraints can always
*  be met exactly, so weight them so that they are "large" compared
*  with the difference data. This prevents them contributing to the
*  error estimates on the results.
         DO 2 I = I1, I2
            Y( N ) = Y( N ) + DBLE( NCMP ) * CCD1__BIGWT * X( I ) /
     :                        DBLE( I2 - I1 + 1 )
 2       CONTINUE

*  If mode is 2, modify X using the difference data.
      ELSE IF ( MODE .EQ. 2 ) THEN
         DO 3 I = 1, NCMP
            X( CCD1_IPAIR( 2, I ) ) = X( CCD1_IPAIR( 2, I ) ) +
     :                                   CCD1_WT1( I ) * Y( I )
            X( CCD1_IPAIR( 1, I ) ) = X( CCD1_IPAIR( 1, I ) ) -
     :                                   CCD1_WT1( I ) * Y( I )
 3       CONTINUE

*  Add constraints on the mean correction. These constraints can always
*  be met exactly, so weight them so that they are "large" compared
*  with the difference data. This prevents them contributing to the
*  error estimates on the results.
         DO 4 I = I1, I2
            X( I ) = X( I ) + DBLE( NCMP ) * CCD1__BIGWT * Y( N ) /
     :                        DBLE( I2 - I1 + 1 )
 4       CONTINUE
      END IF

      END
* $Id$
