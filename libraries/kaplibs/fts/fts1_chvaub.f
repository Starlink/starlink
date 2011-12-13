      SUBROUTINE FTS1_CHVAUB( EL, INARR, OLDVAL, NEWVAL, OUTARR,
     :                         NREP, STATUS )
*+
*  Name:
*     FTS1_CHVAx

*  Purpose:
*     Replaces all occurrences of a value in an array with another
*     value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_CHVAx( EL, INARR, OLDVAL, NEWVAL, OUTARR, NREP, STATUS )

*  Description:
*     This routine copies the input array to the output array, except
*     where a specified value occurs, and this is replaced with a new
*     value throughout the output array.

*  Arguments:
*     EL = INTEGER (Given)
*        The dimension of the input and output arrays.
*     INARR( EL ) = ? (Given)
*        The input array.
*     OLDVAL = ? (Given)
*        Value to be replaced.
*     NEWVAL = ? (Given)
*        New value to be substituted for the old value.
*     OUTARR( EL ) = ? (Returned)
*        The output array containing the modified values.
*     NREP = INTEGER (Returned)
*        The number of replacements made.
*     STATUS  =  INTEGER (Given and Returned)
*        Global status value.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     arrays and values supplied to the routine must have the data type
*     specified.

*  Algorithm:
*     -  To avoid testing two floating-point values for equality, they
*     are tested to be different by less than a small fraction of
*     the value to be replaced.  The machine precision defines the
*     minimum detectable difference.
*     -  For all pixels, if pixel value is the value to be removed then
*     replace old value in data array with the new value, otherwise
*     copy the input value to the output array.

*  Copyright:
*     Copyright (C) 1988, 1990, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     1988 Oct 26 (MJC):
*        Original.
*     1990 Nov 24 (MJC):
*        Used double precision for processing large integer values.
*     1991 November 4 (MJC):
*        Renamed from CHVA.  Used SST prologue. Added NREP argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'PRM_PAR'          ! Machine-precision constant

*  Arguments Given:
      INTEGER EL
      BYTE INARR( EL )
      BYTE NEWVAL
      BYTE OLDVAL

*  Arguments Returned:
      BYTE OUTARR( EL )
      INTEGER NREP

*  Status:
      INTEGER  STATUS

*  Local Variables:
      INTEGER I                  ! Loop counter

      DOUBLE PRECISION DIFF      ! Normalised maximum difference between
                                 ! the data value and the value to
                                 ! change for them to be regarded as
                                 ! identical
      DOUBLE PRECISION FPOLD     ! Floating-point value to be replaced

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'      ! Definitions of conversion routines

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      NREP = 0

*  To avoid testing two floating-point values for equality, they are
*  tested to be different by less than a small fraction of the value to
*  be replaced.  The machine precision defines the minimum detectable
*  difference.
      FPOLD = 0.5 * NUM_UBTOD( OLDVAL )
      DIFF = ABS( FPOLD * VAL__EPSD )
      DO  I = 1, EL, 1

*  The halving is done to prevent overflows.
         IF ( ABS( ( NUM_UBTOD( INARR( I ) ) * 0.5 )
     :        - FPOLD ) .LE. DIFF ) THEN

*  A match has been found so replace the value in the output array and
*  increment the count of the replacements.
            OUTARR( I ) = NEWVAL
            NREP = NREP + 1
         ELSE

*  There is no match so copy the value to the output array.
            OUTARR( I ) = INARR( I )
         END IF
      END DO

*  End and return.
      END
