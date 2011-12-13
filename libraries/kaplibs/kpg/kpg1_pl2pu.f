      SUBROUTINE KPG1_PL2PU( LOC, VARNT, NXPAR, NYPAR, LIMITS, XMIN,
     :                       XMAX, YMIN, YMAX, COEFF, VARIAN, WORK,
     :                       STATUS )
*+
*  Name:
*     KPG1_PL2PU

*  Purpose:
*     Writes two-dimensional polynomial information to a POLYNOMIAL
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PL2PU( LOC, VARNT, NXPAR, NYPAR, LIMITS, XMIN, XMAX,
*                      YMIN, YMAX, COEFF, VARIAN, WORK, STATUS )

*  Description:
*     This routine writes information describing a two-dimensional
*     polynomial surface to a standard Starlink POLYNOMIAL structure,
*     as defined in SGP/38.  An empty POLYNOMIAL structure should
*     already have been created.  All floating point components within
*     the structure are written as DOUBLE PRECISION.
*
*     It is assumed the calling programme has a one-dimensional array of
*     coefficients, where COEFF( ( IX - 1 ) * NYPAR + IY ) contains the
*     coefficient for the (IX,IY)th term (with NYPAR being the total
*     number of Y terms).  Such a one-dimensional array is used by the
*     NAG routines and defined in the NAG manual (see Chapter E02 on
*     "Curve and Surface fitting").
*
*     This routine will convert the coefficient array to
*     two-dimensional, flip it around and store it in the POLYNOMIAL
*     structure so that DATA_ARRAY(IX,IY) contains the coefficient for
*     the (IX,IY)th term (see SGP/38).
*
*     The routine will also load the TMIN and TMAX arrays with XMIN,
*     XMAX, YMIN and YMAX.  Note that these are compulsory when
*     VARNT='CHEBYSHEV' but optional when VARNT='SIMPLE'.  In the latter
*     case the logical parameter LIMITS will be used to decide whether
*     to write the limits.  All the components have type _DOUBLE.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to existing but empty POLYNOMIAL structure.
*     VARNT = CHARACTER * ( * ) (Given)
*        Variant of the polynomial ('CHEBYSHEV' or 'SIMPLE').  (This is
*        written but not checked).
*     NXPAR = INTEGER (Given)
*        Number of x parameters (= order of polynomial in x direction +
*        1)
*     NYPAR  = INTEGER (Given)
*        Number of y parameters (= order of polynomial in y direction +
*        1)
*     LIMITS = LOGICAL (Given)
*        When VARNT='SIMPLE' this logical flag may be used to control
*        whether the TMIN and TMAX limits are written to the polynomial
*        structure (based on the next 4 arguments).  Setting
*        LIMITS=.TRUE. will cause the limits to be written.  This
*        parameter is ignored when VARNT='CHEBYSHEV', as the limits
*        then are compulsory.
*     XMIN = DOUBLE PRECISION (Given)
*        Minimum value along x axis for which polynomial is valid.
*     XMAX = DOUBLE PRECISION (Given)
*        Maximum value along x axis for which polynomial is valid.
*     YMIN = DOUBLE PRECISION (Given)
*        Minimum value along y axis for which polynomial is valid.
*     YMAX = DOUBLE PRECISION (Given)
*        Maximum value along y axis for which polynomial is valid.
*     COEFF( NXPAR * NYPAR ) = DOUBLE PRECISION (Given)
*        Array of polynomial coefficients, in the format used by NAG
*        routines.
*     VARIAN( NXPAR * NYPAR ) = DOUBLE PRECISION (Given)
*        Array of variances of polynomial coefficients, in the format
*        used by NAG routines.
*     WORK( NXPAR, NYPAR ) = DOUBLE PRECISION (Returned)
*        Work array used to flip the polynomial coefficients.  On exit
*        it will contain the two-dimensional array of coefficients
*        written to the POLYNOMIAL structure.
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
*       in a UNIX environment.
*     6-May-1993 (SMB):
*        DAT_PAR does not need to be commented out.
*     10-May-1993 (SMB):
*        I had mis-understood the meaning of the arguments of
*        CMP_PUTND. Now corrected.
*     1995 July 30 (MJC):
*        Renamed from PLYGET2D.  Used modern prologue and style.
*     1997 May 10 (MJC):
*        Added VARIAN argument, and improved the documentation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! DAT__ global constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      CHARACTER*( DAT__SZLOC ) LOC
      CHARACTER *( * ) VARNT
      INTEGER NXPAR
      INTEGER NYPAR
      LOGICAL LIMITS

      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION YMIN
      DOUBLE PRECISION YMAX
      DOUBLE PRECISION COEFF( NXPAR * NYPAR )
      DOUBLE PRECISION VARIAN( NXPAR * NYPAR )

*  Arguments Returned:
      DOUBLE PRECISION WORK( NXPAR, NYPAR )

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Character length determining function

*  Local Constants:
      INTEGER MAXDIM             ! Only two-dimensional polynomials can
                                 ! be handled
      PARAMETER ( MAXDIM = 2 )

*  Local Variables:
      INTEGER CLEN               ! Character length
      INTEGER DIM( MAXDIM )      ! Dimensions of coefficients array
      INTEGER IX                 ! Loop counter
      INTEGER IY                 ! Loop counter
      DOUBLE PRECISION TMAX( MAXDIM ) ! Upper bounds for co-ordinates
      DOUBLE PRECISION TMIN( MAXDIM ) ! Lower bounds for co-ordinates

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the VARIANT element to the polynomial structure, ignoring
*  trailing blanks.
      CLEN = MAX( 1, CHR_LEN( VARNT ) )
      CALL DAT_NEW0C( LOC, 'VARIANT', CLEN, STATUS )
      CALL CMP_PUT0C( LOC, 'VARIANT', VARNT( 1:CLEN ), STATUS )

*  Flip the coefficients array and load up the work array.
      DO IY = 1, NYPAR
         DO IX = 1, NXPAR
            WORK( IX, IY ) = COEFF( ( IX - 1 ) * NYPAR + IY )
         END DO
      END DO

*  Write the coeffients array to the DATA_ARRAY of the polynomial
*  structure.
      DIM( 1 ) = NXPAR
      DIM( 2 ) = NYPAR
      CALL DAT_NEW( LOC, 'DATA_ARRAY', '_DOUBLE', MAXDIM, DIM, STATUS )
      CALL CMP_PUTND( LOC, 'DATA_ARRAY', MAXDIM, DIM, WORK, DIM,
     :                STATUS )

*  Flip the coefficient variances array and load up the work array.
      DO IY = 1, NYPAR
         DO IX = 1, NXPAR
            WORK( IX, IY ) = VARIAN( ( IX - 1 ) * NYPAR + IY )
         END DO
      END DO

*  Write the coeffients array to the DATA_ARRAY of the polynomial
*  structure.
      DIM( 1 ) = NXPAR
      DIM( 2 ) = NYPAR
      CALL DAT_NEW( LOC, 'VARIANCE', '_DOUBLE', MAXDIM, DIM, STATUS )
      CALL CMP_PUTND( LOC, 'VARIANCE', MAXDIM, DIM, WORK, DIM, STATUS )

*  Copy the x and y limits to the TMIN and TMAX arrays within the
*  polynomial structure.  This is only compulsory if VARNT='CHEBYSHEV'.
*  If VARNT='SIMPLE' nothing will be written unless LIMITS=.TRUE..
      IF ( ( VARNT .EQ. 'CHEBYSHEV' ) .OR. LIMITS ) THEN
         TMIN( 1 ) = XMIN
         TMIN( 2 ) = YMIN
         CALL DAT_NEW1D( LOC, 'TMIN', MAXDIM, STATUS )
         CALL CMP_PUT1D( LOC, 'TMIN', MAXDIM, TMIN, STATUS )

         TMAX( 1 ) = XMAX
         TMAX( 2 ) = YMAX
         CALL DAT_NEW1D( LOC, 'TMAX', MAXDIM, STATUS )
         CALL CMP_PUT1D( LOC, 'TMAX', MAXDIM, TMAX, STATUS )
      END IF

      END
