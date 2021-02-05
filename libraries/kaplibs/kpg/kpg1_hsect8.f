      SUBROUTINE KPG1_HSECT8( LOC1, NDIM, LBND, UBND, LOC2, NAME,
     :                       STATUS )
*+
*  Name:
*     KPG1_HSECT8

*  Purpose:
*     Copy a section from an HDS array to a new component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_HSECT8( LOC1, NDIM, LBND, UBND, LOC2, NAME, STATUS )

*  Description:
*     This routine creates a new HDS object holding a copy of a section
*     of a supplied primitive HDS array. The section may extend outside
*     the bounds of the supplied array, in which case the new areas will
*     be filled with bad values (or spaces if the array holds character
*     data).

*  Arguments:
*     LOC1 = CHARACTER*(DAT__SZLOC) (Given)
*        An HDS locator for a primitive array object. This array has
*        implicit lower bounds of (1,1,1,...) and upper bounds equal to
*        its dimension sizes.
*     NDIM = INTEGER (Given)
*        The dimensionality of the required section. An error will be
*        reported if this is not the same as the dimensionality of the
*        supplied HDS array.
*     LBND( NDIM ) = INTEGER*8 (Given)
*        The lower bounds of the required array section.
*     UBND( NDIM ) = INTEGER*8 (Given)
*        The upper bounds of the required array section.
*     LOC2 = CHARACTER*(DAT__SZLOC) (Given)
*        An HDS locator for a structure in which to create the new
*        component holding a copy of the required array section.
*     NAME = CHARACTER*(DAT__SZNAM) (Given)
*        The name of the new component to create within the "LOC2"
*        structure. Any existing component with this name will be
*        erased first.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-DEC-2012 (DSB):
*        Original version.
*     6-OCT-2020 (DSB):
*        Renamed from kpg1_hsect to kpg1_hsect8 and added support for
*        huge arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'CNF_PAR'          ! CNF constants

*  Arguments Given:
      CHARACTER LOC1*(*)
      INTEGER NDIM
      INTEGER*8 LBND( NDIM )
      INTEGER*8 UBND( NDIM )
      CHARACTER LOC2*(*)
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      CHARACTER TYPE*(DAT__SZTYP)
      CHARACTER LOC3*(DAT__SZLOC)
      INTEGER CLEN
      INTEGER*8 DIMIN( DAT__MXDIM )
      INTEGER*8 DIMOUT( DAT__MXDIM )
      INTEGER*8 EL
      INTEGER I
      INTEGER IPIN
      INTEGER IPOUT
      INTEGER*8 LBNDI( DAT__MXDIM )
      INTEGER NDIMIN
      INTEGER*8 UBNDI( DAT__MXDIM )
      LOGICAL PRIM
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied locator is for a primitive array.
      CALL DAT_PRIM( LOC1, PRIM, STATUS )
      IF( .NOT. PRIM .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_HSECT8: Supplied object is not a '//
     :                 'primitive array', STATUS )
      END IF

*  Check the supplied array has the expected number of axes.
      CALL DAT_SHAPE8( LOC1, DAT__MXDIM, DIMIN, NDIMIN, STATUS )
      IF( NDIMIN .NE. NDIM .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'I', NDIMIN )
         CALL MSG_SETI( 'J', NDIM )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_HSECT8: Supplied object has ^I axes'//
     :                 ' but ^J bounds were supplied', STATUS )
      END IF

*  Store the bounds asssociated with the input array, check the
*  requested bounds are now swapped, and get the dimensions of the output
*  array.
      DO I = 1, NDIM
         LBNDI( I ) = 1
         UBNDI( I ) = DIMIN( I )
         DIMOUT( I ) = UBND( I ) - LBND( I ) + 1
         IF( DIMOUT( I ) .LT. 1 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'A', I )
            CALL MSG_SETK( 'L', LBND( I ) )
            CALL MSG_SETK( 'U', UBND( I ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'KPG1_HSECT8: Supplied lower bound on '//
     :                    'axis ^A (^L) is higher than the upper '//
     :                    'bounds (^U).', STATUS )
         END IF
      END DO

*  Get the data type of the supplied array.
      CALL DAT_TYPE( LOC1, TYPE, STATUS )

*  Map it as a vector.
      CALL DAT_MAPV8( LOC1, TYPE, 'READ', IPIN, EL, STATUS )

*  Create the required new component, and get a locator to it.
      CALL DAT_NEW8( LOC2, NAME, TYPE, NDIM, DIMOUT, STATUS )
      CALL DAT_FIND( LOC2, NAME, LOC3, STATUS )

*  Map it as a vector.
      CALL DAT_MAPV8( LOC3, TYPE, 'WRITE', IPOUT, EL, STATUS )

*  Copy the required section of the old array into the new array. A case
*  for each data type.
      IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CPND8I( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CPND8R( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CPND8D( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CPND8W( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CPND8UW( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL(IPIN) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CPND8B( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CPND8UB( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL(IPIN) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_CPND8K( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS )

      ELSE IF( TYPE(:5) .EQ. '_CHAR' ) THEN
         CALL DAT_CLEN( LOC1, CLEN, STATUS )
         CALL KPG1_CPND8C( NDIM, LBNDI, UBNDI, %VAL( CNF_PVAL( IPIN ) ),
     :                    LBND, UBND, %VAL( CNF_PVAL( IPOUT ) ), EL,
     :                    STATUS, %VAL( CNF_CVAL( CLEN ) ),
     :                    %VAL( CNF_CVAL( CLEN ) ) )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'T', TYPE )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_HSECT8: Unsupported data type ^T',
     :                 STATUS )
      END IF

*  Free resources
      CALL DAT_ANNUL( LOC3, STATUS )

      END
