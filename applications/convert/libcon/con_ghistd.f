      SUBROUTINE CON_GHSTD( BAD, DIM, ARRAY, NUMBIN, VALMAX, VALMIN,
     :                      HIST, STATUS )
*+
*  Name:
*     CON_GHSTD

*  Purpose:
*     Calculates the histogram of an array of data.

*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL CON_GHSTD( BAD, DIM, ARRAY, NUMBIN, VALMAX, VALMIN, HIST,
*     :               STATUS )

*  Description:
*     This routine calculates the truncated histogram of an array of
*     data between defined limits and in a defined number of bins.

*  Arguments:
*     BAD = LOGICAL (Given)
*        If .TRUE., bad pixels will be processed.  This should not be
*        set to false unless the input array contains no bad pixels.
*     DIM = INTEGER (Given)
*        The dimension of the array whose histogram is required.
*     ARRAY( DIM ) = ? (Given)
*        The input data array.
*     NUMBIN = INTEGER (Given)
*        Number of bins used in the histogram.  For integer data types
*        this should result in a bin width of at least one unit.  So for
*        example, byte data should never have more than 256 bins.
*     VALMAX = DOUBLE PRECISION (Given)
*        Maximum data value included in the array.
*     VALMIN = DOUBLE PRECISION  (Given)
*        Minimum data value included in the array.
*     HIST( NUMBIN ) = INTEGER (Returned)
*        Array containing the histogram.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     - Compute the scale factor
*     - For all array elements, check whether the value is within the
*     range of the histogram, and if it is compute its bin number. Check
*     for bad pixels if requested.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 July 19 (MJC):
*        Original version.
*     1991 September 17 (MJC):
*        Fixed overflow bug.
*     1991 November 13 (MJC):
*        Fixed a bug converting NUMBIN for calculations.
*     1996 March 25 (MJC):
*        Better check that the bin width is valid.
*     1996 July 3 (MJC):
*        Made to cope with unsigned integer types.
*     05-FEB-1999 (AJC):
*        Copied and renamed from KAPPA KPG1_GHISTD.  Removed use of
*        NUM_ conversions and IF on binsize calculation (as no longer
*        generic)
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER DIM
      INTEGER NUMBIN

      LOGICAL BAD

      DOUBLE PRECISION ARRAY( DIM )
      DOUBLE PRECISION VALMAX
      DOUBLE PRECISION VALMIN

*  Arguments Returned:
      INTEGER HIST( NUMBIN )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER DUMMY              ! Dummy used in histogram calculation
      DOUBLE PRECISION DVMAX     ! Maximum value
      DOUBLE PRECISION DVMIN     ! Minimum value
      INTEGER I                  ! Counter
      INTEGER K                  ! Counter
      DOUBLE PRECISION SCALE     ! Scale factor used for choosing
                                 ! correct bin
      DOUBLE PRECISION TEMP      ! Work variable
      DOUBLE PRECISION THRESH    ! Threshold for identical-limits test
      DOUBLE PRECISION TNUMB     ! Number of bins

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First set the bins of the histogram to zero.
      DO  K = 1, NUMBIN
         HIST( K ) = 0
      END DO

*  Assign some useful variables.
      DVMIN = VALMIN
      DVMAX = VALMAX
      TNUMB = DBLE( NUMBIN )

*  Set the minimum separation allowed for the data limits such that
*  each of the histogram bins is resolvable.  The halving prevents
*  arithmetic overflow for data with a very large dynamic range.
      THRESH = ( ABS( DVMAX ) / 2.0D0 + ABS( DVMIN ) / 2.0D0 ) *
     :            VAL__EPSD * TNUMB

*  Next set the scaling factor, provided the bins are resolvable.
*  Again the halving prevents arithmetic overflow for data with a very
*  large dynamic range.  Thus both sides of the comparison have been
*  halved.
      IF ( ABS( DVMAX / 2.0D0 - DVMIN / 2.0D0 ) .GE. THRESH ) THEN
         SCALE  =  DVMAX - DVMIN
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_GHSTD_LIMITS',
     :     'Maximum and minimum are identical or almost '/
     :     /'the same.  No histogram formed', STATUS )
         GOTO 999
      END IF

*  Calculate the histogram.
*  ========================

*  For processing efficiency deal with the various cases separately.
*  These are with and without bad pixels, and unsigned integer types
*  versus other types.  The latter difference is needed because Fortran
*  does not support unsigned integer types, and numerical comparisons
*  will use the signed types, and hence give the wrong answers.  Thus
*  the comparisons are performed in floating-point.

*  Bad-pixel testing to be undertaken on Fortran data types.
*  ---------------------------------------------------------
         IF ( BAD ) THEN

*  Loop round all the elements.
            DO I = 1, DIM

*  Test for bad pixel or pixel value outside the histogram range.
               IF ( ARRAY( I ) .NE. VAL__BADD .AND.
     :              ARRAY( I ) .GE. VALMIN .AND.
     :              ARRAY( I ) .LE. VALMAX ) THEN

*  Find bin number for particular point.  MIN is to allow for pixel
*  value to equal maximum.
                  DUMMY = MIN( NUMBIN, 1 + INT( TNUMB *
     :                    ( ARRAY( I ) - DVMIN )
     :                    / SCALE ) )

*  Increment the number of data in the bin by one.
                  HIST( DUMMY )  =  HIST( DUMMY ) + 1
               END IF

*  End of loop round elements.
            END DO

*  No bad-pixel testing... on Fortran data types.
*  ----------------------------------------------
         ELSE

*  Loop round all the elements.
            DO I = 1, DIM

*  Test for pixel value outside the histogram range.
               IF ( ARRAY( I ) .GE. VALMIN .AND.
     :              ARRAY( I ) .LE. VALMAX ) THEN

*  Find bin number for particular point.  MIN is to allow for pixel
*  value to equal maximum.
                  DUMMY = MIN( NUMBIN, 1 + INT( TNUMB *
     :                    ( ARRAY( I ) - DVMIN )
     :                    / SCALE ) )

*  Increment the number of data in the bin by one.
                  HIST( DUMMY )  =  HIST( DUMMY ) + 1
               END IF

*  End of loop round elements.
            END DO

*  End of main bad-pixels-present check
         END IF

  999  CONTINUE

*  End and return.
      END
