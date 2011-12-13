      SUBROUTINE KPG1_PL2GE( LOC, MXPAR, VARNT, NXPAR, NYPAR, LIMITS,
     :                       XMIN, XMAX, YMIN, YMAX, COEFF, VARPRE,
     :                       VARIAN, WORK, STATUS )

*+
*  Name:
*     KPG1_PL2GE

*  Purpose:
*     Reads two-dimensional polynomial information from a POLYNOMIAL
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PL2GE( LOC, MXPAR, VARNT, NXPAR, NYPAR, LIMITS, XMIN,
*                      XMAX, YMIN, YMAX, COEFF, VARPRE, VARIAN, WORK,
*                      STATUS )

*  Description:
*     This routine reads information describing a two-dimensional
*     polynomial surface from a standard Starlink POLYNOMIAL structure,
*     as defined in SGP/38.  All floating-point information within the
*     structure is returned as DOUBLE PRECISION.
*
*     It is assumed the calling programme needs a one-dimensional array
*     of coefficients, where COEFF( (IX-1)*NYPAR + IY ) contains the
*     coefficient for the (IX,IY)th term (with NYPAR being the total
*     number of Y terms).  Such a one-dimensional array is used by the
*     NAG routines and defined in the NAG manual (see Chapter E02 on
*     "Curve and Surface fitting").
*
*     This routine will convert read the two-dimensional coefficient
*     array from the POLYNOMIAL structure (in the format described in
*     SGP/38) and load a flipped version of this into the required
*     one-dimensional array.  If there is a variance array present, the
*     VARPRE flag is set .TRUE., and the variances returned in VARIAN.
*
*     The routine will also read the TMIN and TMAX arrays from the
*     structure and return XMIN, XMAX, YMIN and YMAX.  Note that these
*     items are compulsory when VARNT='CHEBYSHEV' but optional when
*     VARNT='SIMPLE'.  In the latter case an attempt will be made to
*     read TMIN and TMAX from the structure, but their absence will not
*     be regarded as an error.  The returned parameter LIMITS will
*     indicate if these items have been read successfully.
*
*

*  Arguments:
*     LOC = CHARACTER*( DAT__SZLOC ) (Given)
*        Locator to the existing POLYNOMIAL structure.
*     MXPAR = INTEGER (Given)
*        The maximum number of parameters in either x or y.  The
*        declared size of the coefficient arrays given to this routine
*        is assumed to be MXPAR * MXPAR (see below).
*     VARNT = CHARACTER*(*) (Returned)
*        Variant of the polynomial ('CHEBYSHEV' or 'SIMPLE').  This
*        item should be at least CHARACTER*9.
*     NXPAR = INTEGER (Returned)
*        Number of x parameters (= order of polynomial in x direction +
*        1).
*     NYPAR = INTEGER (Returned)
*        Number of y parameters (= order of polynomial in Y direction +
*        1).
*     LIMITS = LOGICAL (Returned)
*        When VARNT='SIMPLE' this logical flag indicates whether any
*        TMIN and TMAX limits have been read from the polynomial
*        structure and returned in the next four arguments (in which
*        case LIMITS will be .TRUE.).
*     XMIN = DOUBLE PRECISION (Returned)
*        Minimum value along x axis for which polynomial is valid.
*     XMAX = DOUBLE PRECISION (Returned)
*        Maximum value along x axis for which polynomial is valid.
*     YMIN = DOUBLE PRECISION (Returned)
*        Minimum value along y axis for which polynomial is valid.
*     YMAX = DOUBLE PRECISION (Returned)
*        Maximum value along y axis for which polynomial is valid.
*     COEFF( MXPAR * MXPAR ) = DOUBLE PRECISION (Returned)
*        Array of polynomial coefficients, in the format used by NAG
*        routines (see KPS1_FSPF2).
*     VARPRE = LOGICAL (Returned)
*        Whether or not there are coefficient variances present in the
*        POLYNOMIAL structure.
*     VARIAN( MXPAR * MXPAR ) = DOUBLE PRECISION (Returned)
*        Array of polynomial coefficient variances, in the format used
*        by NAG routines.  The values are only assigned when VARPRE is
*        .TRUE..
*     WORK( MXPAR, MXPAR ) = DOUBLE PRECISION (Returned)
*        Work array containing the two-dimensional array of
*        coefficients as read from the POLYNOMIAL structure.
*     STATUS =  INTEGER (Given and Returned)
*        Global status value.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997 Central Laboratory of the Research
*                   Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     SMB: Steven M Beard (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-Apr-1993 (SMB):
*        Original version.
*     23-Apr-1993 (SMB):
*        DAT_PAR included (commented out) so the routine can work
*        in a UNIX environment.
*     6-May-1993 (SMB):
*        DAT_PAR does not need to be commented out.
*     10-May-1993 (SMB):
*        I had mis-understood the meaning of the arguments of
*        CMP_GETND.  Now corrected.
*     1995 July 30 (MJC):
*        Renamed from PLYPUT2D.  Used modern prologue and style.
*     1997 May 10 (MJC):
*        Added VARIAN and VARPRE arguments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! DAT__ global constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      CHARACTER*( DAT__SZLOC ) LOC

      INTEGER MXPAR

*  Arguments Returned:
      CHARACTER *( * ) VARNT
      INTEGER NXPAR
      INTEGER NYPAR
      LOGICAL LIMITS
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION YMIN
      DOUBLE PRECISION YMAX
      DOUBLE PRECISION COEFF( MXPAR * MXPAR )
      LOGICAL VARPRE
      DOUBLE PRECISION VARIAN( MXPAR * MXPAR )
      DOUBLE PRECISION WORK( MXPAR, MXPAR )

*  Status:
      INTEGER STATUS

*  Local constants:
      INTEGER MAXDIM             ! Only two-dimensional polynomials can
                                 ! be handled
      PARAMETER ( MAXDIM = 2 )

*  Local variables:
      INTEGER DIM( MAXDIM )      ! Actual dimensions of coefficients
                                 ! array
      INTEGER DIMX( MAXDIM )     ! Declared dimensions of coefficients
                                 ! array
      INTEGER IX                 ! Loop counter
      INTEGER IY                 ! Loop counter
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of HDS component
      INTEGER NDIM               ! Number of dimensions
      LOGICAL THERE              ! HDS object is present?
      DOUBLE PRECISION TMIN( MAXDIM ) ! Lower bounds for co-ordinates
      DOUBLE PRECISION TMAX( MAXDIM ) ! Upper bounds for co-ordinates

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the VARIANT element from the polynomial structure.
      CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )

*  Find out the size of the DATA_ARRAY of coefficients, and ensure it
*  is smaller than the maximum permitted.
      CALL CMP_SHAPE( LOC, 'DATA_ARRAY', MAXDIM, DIM, NDIM, STATUS )

*  Is there a variance array?
      VARPRE = .FALSE.
      CALL DAT_THERE( LOC, 'VARIANCE', VARPRE, STATUS )

      NXPAR = DIM( 1 )
      NYPAR = DIM( 2 )

      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( NXPAR .LE. MXPAR ) .AND. ( NYPAR .LE. MXPAR ) ) THEN

*  Read the coefficients into the work array.
         DIMX( 1 ) = MXPAR
         DIMX( 2 ) = MXPAR

         CALL CMP_GETND( LOC, 'DATA_ARRAY', MAXDIM, DIMX, WORK, DIM,
     :                   STATUS )

*  Flip the work array to restore the original one-dimensional
*  coefficients array.
         DO IY = 1, NYPAR
            DO IX = 1, NXPAR
               COEFF( ( IX - 1 ) * NYPAR + IY ) = WORK( IX, IY )
            END DO
         END DO

*  Read the variances of thr coefficients into the work array.
         CALL CMP_GETND( LOC, 'VARIANCE', MAXDIM, DIMX, WORK, DIM,
     :                   STATUS )

*  Flip the work array to restore the original one-dimensional
*  coefficients' variances array.
         DO IY = 1, NYPAR
            DO IX = 1, NXPAR
               VARIAN( ( IX - 1 ) * NYPAR + IY ) = WORK( IX, IY )
            END DO
         END DO

*  If VARNT='CHEBYSHEV' the presence of the TMIN and TMAX arrays is
*  compulsory, so their absence should be regarded as an error.  If
*  VARNT='SIMPLE' the arrays are optional.  Attempt to read them and set
*  the LIMITS flag if they are read successfully.
         IF ( VARNT .EQ. 'CHEBYSHEV' ) THEN
            CALL CMP_GET1D( LOC, 'TMIN', MAXDIM, TMIN, NDIM, STATUS )
            XMIN = TMIN( 1 )
            YMIN = TMIN( 2 )

            CALL CMP_GET1D( LOC, 'TMAX', MAXDIM, TMAX, NDIM, STATUS )
            XMAX = TMAX( 1 )
            YMAX = TMAX( 2 )

            LIMITS = .TRUE.

         ELSE
            LIMITS = .TRUE.

            CALL DAT_THERE( LOC, 'TMIN', THERE, STATUS )

            IF ( ( STATUS .EQ. SAI__OK ) .AND. THERE ) THEN

               CALL CMP_GET1D( LOC, 'TMIN', MAXDIM, TMIN, NDIM,
     :                         STATUS )
               XMIN = TMIN( 1 )
               YMIN = TMIN( 2 )

            ELSE
               LIMITS = .FALSE.
            END IF

            CALL DAT_THERE( LOC, 'TMAX', THERE, STATUS )

            IF ( (STATUS .EQ. SAI__OK) .AND. THERE ) THEN

               CALL CMP_GET1D( LOC, 'TMAX', MAXDIM, TMAX, NDIM,
     :                         STATUS )
               XMAX = TMAX( 1 )
               YMAX = TMAX( 2 )

            ELSE
               LIMITS = .FALSE.
            END IF
         END IF

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  There are too many coefficients in the POLYNOMIAL structure to store
*  in the arrays provided.  Report an error.
         CALL DAT_NAME( LOC, NAME, STATUS )

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( ' ', 'Error reading coefficients '/
     :     /'from POLYNOMIAL structure ^NAME.', STATUS )

         CALL MSG_SETI( 'NXPAR', NXPAR )
         CALL MSG_SETI( 'NYPAR', NYPAR )
         CALL MSG_SETI( 'MXPAR', MXPAR )
         CALL ERR_REP( ' ', ' - There are ^NXPAR x ^NYPAR '/
     :     /'coefficients, which exceeds the maximum of ^MXPAR '/
     :     /'x ^MXPAR.', STATUS )
      END IF

      END
