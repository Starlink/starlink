      SUBROUTINE KPS1_ALIG0( NX, NY, IN, REF, CORLIM, CR, NREJ, IFAC,
     :                       RFAC, IOFF, ROFF, STATUS )

*+
*  Name:
*     KPS1_ALIG0

*  Purpose:
*     Copy the input values if the correlation is below a given value.
*     value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ALIG0( NX, NY, IN, REF, CORLIM, CR, NREJ, IFAC, RFAC,
*                      IOFF, ROFF, STATUS )

*  Description:
*     This routine creates a copy of the input array in which pixels with
*     a low correlation are set bad. It is used by the ALIGN2D command.
*     The copy is returned in the CR array

*  Arguments:
*     NX = INTEGER
*        Number of pixels in a row.
*     NY = INTEGER
*        Number of pixels in a column.
*     IN( NX, NY ) = DOUBLE PRECISION (Given)
*        The input array.
*     REF( NX, NY ) = DOUBLE PRECISION (Given)
*        The reference array.
*     CORLIM = REAL (Given)
*        The lower limit on usable correlations.
*     CR( NX, NY ) = DOUBLE PRECISION (Given and Returned)
*        On entry, the array of correlation values. On exit, a copy of the
*        input array in which pixels with correlation below CORLIM have
*        been set bad.
*     NREJ = INTEGER (Returned)
*        The number of pixels rejected.
*     IFAC = DOUBLE PRECISION (Returned)
*        Factor by which to multipl the remaining input values to give
*        them a standard deviation of unity. Returned as VAL__BADD if no
*        good pixels remain.
*     RFAC = DOUBLE PRECISION (Returned)
*        Factor by which to multiply the remaining reference values to give
*        them a standard deviation of unity. Returned as VAL__BADD if no
*        good pixels remain.
*     IOFF = DOUBLE PRECISION (Returned)
*        Offset to subtract from the scaled input values to give them a
*        mean of zero. Returned as VAL__BADD if no good pixels remain.
*     ROFF = DOUBLE PRECISION (Returned)
*        Offset to subtract from the scaled reference values to give them a
*        mean of zero. Returned as VAL__BADD if no good pixels remain.
*     STATUS  =  INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     11-MAY-2016 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE parameters
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Status:
      INTEGER STATUS             ! Global status value

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION IN( NX, NY )
      DOUBLE PRECISION REF( NX, NY )
      REAL CORLIM

*  Arguments Given and Returned:
      DOUBLE PRECISION CR( NX, NY )

*  Arguments Returned:
      INTEGER NREJ
      DOUBLE PRECISION IFAC
      DOUBLE PRECISION RFAC
      DOUBLE PRECISION IOFF
      DOUBLE PRECISION ROFF

*  Local Variables:
      INTEGER IX
      INTEGER IY
      INTEGER NGOOD
      DOUBLE PRECISION ISUM
      DOUBLE PRECISION ISUM2
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION RSUM
      DOUBLE PRECISION RSUM2
      DOUBLE PRECISION SIGMA
*.

*  Initialise returned values.
      NREJ = 0
      IFAC = VAL__BADD
      RFAC = VAL__BADD

*  Check the global inhertied status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do it.
      RSUM = 0.0D0
      ISUM = 0.0D0
      RSUM2 = 0.0D0
      ISUM2 = 0.0D0
      NGOOD = 0

      write(*,*) '# ix iy in'

      DO IY = 1, NY
         DO IX = 1, NX

            IF( CR( IX, IY ) .NE. VAL__BADD ) THEN
               IF( CR( IX, IY ) .LT. CORLIM ) THEN
                  CR( IX, IY ) = VAL__BADD
                  NREJ = NREJ + 1
               ELSE
                  CR( IX, IY ) = IN( IX, IY )
               END IF
            END IF

            IF( CR( IX, IY ) .NE. VAL__BADD .AND.
     :          REF( IX, IY ) .NE. VAL__BADD ) THEN
               ISUM = ISUM + CR( IX, IY )
               RSUM = RSUM + REF( IX, IY )
               ISUM2 = ISUM2 + CR( IX, IY )**2
               RSUM2 = RSUM2 + REF( IX, IY )**2
               NGOOD = NGOOD + 1
            END IF

            IF( CR( IX, IY ) .NE. VAL__BADD ) then
         write(*,*) ix, iy,  CR( IX, IY )
            ELSE
         write(*,*) ix, iy,  ' null'
            ENDIF

         END DO
      END DO

      IF( NGOOD .GT. 0 ) THEN
         MEAN = ISUM/NGOOD
         SIGMA = SQRT( ISUM2/NGOOD - MEAN**2 )
         IFAC = 1.0D0/SIGMA
         IOFF = MEAN/SIGMA

         MEAN = RSUM/NGOOD
         SIGMA = SQRT( RSUM2/NGOOD - MEAN**2 )
         RFAC = 1.0D0/SIGMA
         ROFF = MEAN/SIGMA
      END IF

      END
