      SUBROUTINE CCG8_ORVAR( EL, NBIG, PP, VEC, MATRIX, STATUS )
*+
*  Name:
*     CCG8_ORVAR

*  Purpose:
*     Returns the variances and covariances of the order statistics
*     from n to 1, assuming an initially normal distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG8_ORVAR( EL, NBIG, PP, VEC, MATRIX, STATUS )

*  Description:
*     The routine returns the variances and covariances of the order
*     statistics, assuming an initial (pre-ordered) normal distribution
*     of mean 0 and standard deviation 1. The routine returns all
*     variance/covariances in an array with the terms vectorised - that
*     is following on after each row. This uses the symmetric nature of
*     the matrix to compress the data storage, but remember to double
*     the covariance components if summing in quadrature. The variances
*     -covariances are returned for all statistics from n to 1. The
*     special case of n = 1 returns the variance of 2/pi (median).

*  Arguments:
*     EL = INTEGER*8 (Given)
*        Number of members in ordered set.
*     NBIG = INTEGER*8 (Given)
*        Maximum number of entries in covariance array row.
*        equal to EL*(EL+1)/2).
*     PP( EL ) = DOUBLE PRECISION (Given)
*        Workspace for storing expected values of order statistics.
*     VEC( NBIG, EL ) = DOUBLE PRECISION (Returned)
*        The upper triangles of the nset by nset variance-covariance
*        matrix packed by columns.  Each triangle is packed into a
*        single row.  For each row element Vij is stored in
*        VEC(i+j*(j-1)/2), for 1 <= i <= j <= nset.
*     MATRIX( EL, EL ) = DOUBLE PRECISION (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Data are returned as above to save on repeated calls (which are
*     too slow).  To obtain the actual variance of the data of order n
*     you need to sum all the variances and twice the covariances and
*     use these to modify the actual variance of the (unordered) data.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1996-1998, 2002 Central Laboratory of the Research
*     Councils. Copyright (C) 2009 Science & Technology Facilities
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-MAR-1991 (PDRAPER):
*        Original version.
*     22-MAY-1992 (PDRAPER):
*        Changed 2/pi to pi/2 - in line with other variances.
*     4-SEP-1996 (PDRAPER):
*        Replaced NAG routine calls with public domain versions.
*        Interface remains the same except limit on EL is now
*        CCG__MXNDF (needed extra workspace and this is the easiest
*        place to create it).
*     24-FEB-1997 (PDRAPER):
*        Removed unnecessary EXP1 and EXP2 estimates
*     28-JAN-1998 (PDRAPER):
*        Added check for normal scores with a population of 2.
*        The second score isn't calculated as it is the negative
*        of the first score.
*     1-NOV-2002 (DSB):
*        Add MATRIX workspace argument in order to remove limit that EL
*        be no bigger than CCG1__MXNDF.
*     2009 July 5 (MJC):
*        Rebadged to be part of the more-public CCG library.
*     15-JAN-2020 (DSB):
*        Add support for huge files.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*     Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'

*  Arguments Given:
      INTEGER*8 EL
      INTEGER*8 NBIG
      DOUBLE PRECISION PP( EL )

*  Arguments Returned:
      DOUBLE PRECISION VEC( NBIG, EL )
      DOUBLE PRECISION MATRIX( EL, EL )

*  Local Constants:
      DOUBLE PRECISION PIBY2
      PARAMETER ( PIBY2 = 1.57079632679  ) ! PI/2

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      DOUBLE PRECISION PDA8_V11
      EXTERNAL PDA8_V11

*  Local Variables:
      INTEGER*8 I                ! Loop variable
      INTEGER*8 J                ! Loop variable
      INTEGER*8 K                ! Loop variable
      INTEGER*8 L                ! Loop variable
      INTEGER IFAIL              ! Local status return
      INTEGER*8 N2               ! Half of matrix size
      DOUBLE PRECISION SUMSQ     ! Sum of squares
      DOUBLE PRECISION V11       ! Value of extreme variance (1,1)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for all possible values of the ordered set size.
      DO 1  I = 1, EL
         IF( I .EQ. 1 ) THEN

*  If just have the one nset then the appropriate value is pi/2.
            VEC( 1, 1 ) = PIBY2
         ELSE

*  Get the expected values of the normal order statistics.
            N2 = I/2
            CALL PDA8_NSCOR( PP, I, N2, IFAIL )
            IF ( N2 .EQ. 1 ) PP( 2 ) = -PP( 1 )

*  Form sum of squares of the expected values of the normal order
*  statistics
            SUMSQ= 0.0D0
            DO 2 J = 1, N2
               SUMSQ = PP( J ) * PP( J ) + SUMSQ
    2       CONTINUE
            SUMSQ = SUMSQ * 2.0D0

*  Get the covariance matrix for I*I.
            V11 = PDA8_V11( I, IFAIL )
            CALL PDA8_COVMAT( MATRIX, I, EL, V11, PP( 1 ),
     :                        PP( 2 ), SUMSQ, IFAIL )

*  Now pack this into the output matrix in the form recognised by other
*  routines.
            K = 1
            DO 3 J = 1, I
               DO 4 L = 1, J
                  VEC( K, I ) = MATRIX( L, J )
                  K = K + 1
    4          CONTINUE
    3       CONTINUE
         END IF
    1 CONTINUE

      END
