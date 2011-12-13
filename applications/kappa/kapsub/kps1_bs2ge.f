      SUBROUTINE KPS1_BS2GE( LOC, MXPAR, NXKNOT, NYKNOT, XKNOT, YKNOT,
     :                       SCALE, COEFF, WORK, STATUS )

*+
*  Name:
*     KPS1_BS2GE

*  Purpose:
*     Reads two-dimensional B-spline information from a POLYNOMIAL
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BS2GE( LOC, MXPAR, NXKNOT, NYKNOT, XKNOT, YKNOT, SCALE,
*                      COEFF, WORK, STATUS )

*  Description:
*     This routine reads information describing a two-dimensional
*     bi-cubic B-spline polynomial surface from a standard Starlink
*     POLYNOMIAL structure, as defined in SGP/38 and extended to
*     B-splines.  All floating-point information within the structure is
*     returned as single precision.
*
*     It is assumed the calling programme needs a one-dimensional array
*     of coefficients, where COEFF( ( IX - 1 ) * ( NYKNOT + 4 ) + IY )
*     contains the coefficient for the (IX,IY)th term (with NYKNOT + 4
*     being the total number of Y terms).  Such a one-dimensional array
*     is used by the PDA_SURFIT and PDA_BISPEV routines and defined in
*     the NAG manual (see Chapter E02 on "Curve and Surface fitting").
*
*     This routine will convert the two-dimensional coefficient array
*     from the POLYNOMIAL structure (in the format described in SGP/38
*     and extended to B-splines in KPS1_PL2PU), and load a flipped
*     version of this into the required one-dimensional array.
*
*     The routine will also read the knot positions in _INTEGER type
*     array KNOTS (or XKNOTS and YKNOTS for older data), and a
*     data-scaling factor in SCALE of type _REAL.

*  Arguments:
*     LOC = CHARACTER*( DAT__SZLOC ) (Given)
*        Locator to the existing POLYNOMIAL structure.
*     MXPAR = INTEGER (Given)
*        The maximum number of coefficients in either x or y.  The
*        declared size of the coefficient arrays given to this routine
*        is assumed to be MXPAR * MXPAR (see below).  It should be set
*        to the maximum number of interior knots per axis plus four.
*     NXKNOT = INTEGER (Returned)
*        The number of x interior knots.
*     NYKNOT = INTEGER (Returned)
*        The number of y interior knots.
*     XKNOT( MXPAR + 4 ) = REAL (Returned)
*        The x positions of complete set of knots associated with x.
*     YKNOT( MXPAR + 4 ) = REAL (Returned)
*        The y positions of complete set of knots associated with y.
*     SCALE = REAL (Returned)
*        The scale factor applied to the data values before calculating
*        the spline.
*     COEFF( MXPAR * MXPAR ) = DOUBLE PRECISION (Returned)
*        Array of B-spline polynomial coefficients, in the format used
*        by NAG and PDA routines (see KPS1_SUSF).
*     WORK( MXPAR, MXPAR ) = REAL (Returned)
*        Work array containing the two-dimensional array of
*        coefficients as read from the POLYNOMIAL structure.
*     STATUS =  INTEGER (Given and Returned)
*        Global status value.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Copyright:
*     Copyright (C) 2007, 2009 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 4 (MJC):
*        Original version derived from KPG1_PL2GE.
*     2009 December 17 (MJC):
*        Use a single generic concatenated KNOTS vector as now
*        documented in SGP/38, but for compatibility with older data
*        test for XKNOTS and YKNOTS components if KNOTS is absent.
*     2009 December 19 (MJC):
*        Use known values to declare XKNOT and YKNOT dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! DAT__ global constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

*  Arguments Given:
      CHARACTER*( DAT__SZLOC ) LOC
      INTEGER MXPAR

*  Arguments Returned:
      INTEGER NXKNOT
      INTEGER NYKNOT
      REAL XKNOT( MXPAR + 4 )
      REAL YKNOT( MXPAR + 4 )
      REAL COEFF( MXPAR * MXPAR )
      REAL SCALE
      REAL WORK( MXPAR, MXPAR )

*  Status:
      INTEGER STATUS             ! Global inherited status

*  Local Constants:
      INTEGER MAXDIM             ! Only two-dimensional polynomials can
                                 ! be handled
      PARAMETER ( MAXDIM = 2 )

*  Local Variables:
      INTEGER DIM( MAXDIM )      ! Actual dimensions of coefficients
                                 ! array
      INTEGER DIMX( MAXDIM )     ! Declared dimensions of coefficients
                                 ! array
      INTEGER ELX                ! Number of X knots read from structure
      INTEGER ELY                ! Number of Y knots read from structure
      CHARACTER*256 FILE         ! File name
      INTEGER IX                 ! Loop counter
      INTEGER IY                 ! Loop counter
      INTEGER KPNTR              ! Pointer to knots' vector workspace
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of HDS component
      INTEGER NDIM               ! Number of dimensions
      INTEGER NKNOTS             ! Total number of knots
      INTEGER NLEV               ! Number of levels in the HDS path
      CHARACTER*256 PATH         ! HDS path name
      LOGICAL VALID              ! KNOTS locator is valid?
      CHARACTER*10 VARNT         ! Variant of the POLYNOMIAL structure

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the VARIANT element from the polynomial structure.
      CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )

      IF ( VARNT .NE. 'BSPLINE' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'V',  VARNT )
         CALL ERR_REP( 'KPS1_BS2GE_VARIANT', 'The POLYNOMIAL '/
     :                 /'structure has type ^V, expecting BSPLINE.',
     :                 STATUS )
       ELSE

*  Find out the size of the DATA_ARRAY of coefficients, and ensure it
*  is smaller than the maximum permitted.
         CALL CMP_SHAPE( LOC, 'DATA_ARRAY', MAXDIM, DIM, NDIM, STATUS )

         NXKNOT = DIM( 1 ) - 4
         NYKNOT = DIM( 2 ) - 4

         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( DIM( 1 ) .LE. MXPAR ) .AND.
     :        ( DIM( 2 ) .LE. MXPAR ) ) THEN

*  Read the coefficients into the work array.
            DIMX( 1 ) = MXPAR
            DIMX( 2 ) = MXPAR

            CALL CMP_GETNR( LOC, 'DATA_ARRAY', MAXDIM, DIMX, WORK, DIM,
     :                      STATUS )

*  Flip the work array to restore the original one-dimensional
*  coefficients array.
            DO IY = 1, DIM( 2 )
               DO IX = 1, DIM( 1 )
                  COEFF( ( IX - 1 ) * DIM( 2 ) + IY ) = WORK( IX, IY )
               END DO
            END DO

*  Test whether there is a KNOTS component.
            CALL DAT_THERE( LOC, 'KNOTS', VALID, STATUS )

            IF ( VALID ) THEN

*  Obtain workspace of the length of the KNOTS array.
               NKNOTS = NXKNOT + NYKNOT + 16
               CALL PSX_CALLOC( NKNOTS, '_REAL', KPNTR, STATUS )

               CALL CMP_GET1R( LOC, 'KNOTS', NKNOTS,
     :                         %VAL( CNF_PVAL( KPNTR ) ), ELX, STATUS )

               IF ( ELX .NE. NKNOTS ) THEN
                  CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'X', ELX )
                  CALL MSG_SETI( 'NX', NKNOTS )
                  CALL MSG_SETC( 'P', PATH )
                  CALL MSG_SETC( 'F', FILE )
                  CALL ERR_REP( 'KPS1_BS2GE_INVPOLY2', 'The '/
     :                          /'POLYNOMIAL structure ^P in ^F has '/
     :                          /'fewer (^X) knots than expected '/
     :                          /'(^NX).', STATUS )
                  GO TO 999
               END IF

*  Copy the X-axis set of knots from the work vector into returned
*  array.
               CALL KPG1_CPNDR( 1, 1, NKNOTS,
     :                          %VAL( CNF_PVAL( KPNTR ) ), 1,
     :                          NXKNOT + 8, XKNOT, ELX, STATUS )

*  Copy the Y-axis set of knots from the work vector into returned
*  array, offset by the number XKNOTS.
               CALL KPG1_CPNDR( 1, 1, NKNOTS,
     :                          %VAL( CNF_PVAL( KPNTR ) ), NXKNOT + 9,
     :                          NKNOTS, YKNOT, ELX, STATUS )

               CALL PSX_FREE( KPNTR, STATUS )

            ELSE

*  Copy the knots to the XKNOTS and YKNOTS arrays within the
*  POLYNOMIAL structure.
               CALL CMP_GET1R( LOC, 'XKNOTS', NXKNOT + 8, XKNOT, ELX,
     :                         STATUS )
               CALL CMP_GET1R( LOC, 'YKNOTS', NYKNOT + 8, YKNOT, ELY,
     :                         STATUS )

               IF ( ELX .NE. NXKNOT + 8 .OR. ELY .NE. NYKNOT + 8 ) THEN
                  CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'X', ELX )
                  CALL MSG_SETI( 'Y', ELY )
                  CALL MSG_SETI( 'NX', NXKNOT + 8 )
                  CALL MSG_SETI( 'NY', NYKNOT + 8 )
                  CALL MSG_SETC( 'P', PATH )
                  CALL MSG_SETC( 'F', FILE )
                  CALL ERR_REP( 'KPS1_BS2GE_INVPOLY', 'The POLYNOMIAL '/
     :                          /'structure ^P in ^F has fewer '/
     :                          /'(^X,^Y) knots than expected '/
     :                          /'(^NX,^NY).', STATUS )
                  GO TO 999
               END IF
            END IF

*  Read the data scaling factor.
            CALL CMP_GET0R( LOC, 'SCALE', SCALE, STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  There are too many coefficients in the POLYNOMIAL structure to store
*  in the arrays provided.  Report an error.
            CALL DAT_NAME( LOC, NAME, STATUS )

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( ' ', 'Error reading coefficients '/
     :        /'from POLYNOMIAL structure ^NAME.', STATUS )

            CALL MSG_SETI( 'NXPAR', DIM( 1 ) )
            CALL MSG_SETI( 'NYPAR', DIM( 2 ) )
            CALL MSG_SETI( 'MXPAR', MXPAR )
            CALL ERR_REP( ' ', ' - There are ^NXPAR x ^NYPAR '/
     :        /'coefficients, which exceeds the maximum of ^MXPAR '/
     :        /'x ^MXPAR.', STATUS )
         END IF
      END IF

  999 CONTINUE
      END
