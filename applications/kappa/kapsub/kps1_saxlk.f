      SUBROUTINE KPS1_SAXLK( INDF1, INDF2, STATUS )
*+
*  Name:
*     KPS1_SAXLK

*  Purpose:
*     Copy an AXIS system from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SAXLK( INDF1, INDF2, STATUS )

*  Description:
*     This routine copies the entire array of AXIS structures from INDF2
*     to INDF1.  These axis arrays are extrapolated as necessary by the
*     NDF library.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the destination NDF.
*     INDF2 = INTEGER (Given)
*        Identifier for the source NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16 MAY 2000 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, including CNF_PVAL, and wrapped
*        long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NCCOMP             ! Number of character components
      PARAMETER ( NCCOMP = 2 )
      INTEGER NACOMP             ! Number of array components
      PARAMETER ( NACOMP = 3 )

*  Local Variables:
      CHARACTER ACOMPS( NACOMP )*( 8 ) ! AXIS array components
      CHARACTER CCOMPS( NCCOMP )*( 5 ) ! AXIS character components
      CHARACTER COMP*8           ! AXIS component name
      CHARACTER TYPE*( NDF__SZTYP )! Array numeric type
      CHARACTER VALUE*( 256 )    ! Character-component value
      INTEGER EL                 ! No. of elements in an axis array
      INTEGER IAX                ! Axis index
      INTEGER ICOMP              ! Component index
      INTEGER INDF2S             ! NDF identifier for source section
      INTEGER IP1                ! Pointer to destination array
      INTEGER IP2                ! Pointer to source array
      INTEGER LBND( NDF__MXDIM ) ! Lower axis bounds of destination
      INTEGER NDIM               ! Number of axes in destination
      INTEGER UBND( NDF__MXDIM ) ! Upper axis bounds of destination
      INTEGER VLEN               ! Length of character component
      LOGICAL NORM               ! Axis normalization error
      LOGICAL THERE              ! Is the component there?

*  Local Data:
      DATA CCOMPS / 'LABEL', 'UNITS' /
      DATA ACOMPS / 'CENTRE', 'WIDTH', 'VARIANCE' /

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Reset the destination AXIS structures.
      CALL NDF_RESET( INDF1, 'AXIS', STATUS )

*  Find the pixel bounds of the destination NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Take a section from the source NDF which matches the bounds of the
*  destination NDF.
      CALL NDF_SECT( INDF2, NDIM, LBND, UBND, INDF2S, STATUS )

*  Check each axis.
      DO IAX = 1, NDIM

*  Loop round each array component.
         DO ICOMP = 1, NACOMP
            COMP = ACOMPS( ICOMP )

*  Pass on if this axis does not have the a defined AXIS component in
*  the source section.
            CALL NDF_ASTAT( INDF2S, COMP, IAX, THERE, STATUS )
            IF( THERE ) THEN

*  Get the numeric type of the array.
               CALL NDF_ATYPE( INDF2S, COMP, IAX, TYPE, STATUS )

*  Map the source array.
               CALL NDF_AMAP( INDF2S, COMP, IAX, TYPE, 'READ', IP2, EL,
     :                        STATUS )

*  Map the destination array.
               CALL NDF_AMAP( INDF1, COMP, IAX, TYPE, 'WRITE', IP1, EL,
     :                        STATUS )

*  Copy the source array to the destination array.
               CALL KPG1_COPY( TYPE, EL, IP2, IP1, STATUS )

            END IF
         END DO

*  Loop round each character component.
         DO ICOMP = 1, NCCOMP
            COMP = CCOMPS( ICOMP )

*  Pass on if this axis does not have the a defined AXIS component in
*  the source section.
            CALL NDF_ASTAT( INDF2S, COMP, IAX, THERE, STATUS )
            IF( THERE ) THEN

*  Get the value from the source NDF, and its length.
               CALL NDF_ACGET( INDF2S, COMP, IAX, VALUE, STATUS )
               CALL NDF_ACLEN( INDF2S, COMP, IAX, VLEN, STATUS )

*  Put it into destination NDF.
               CALL NDF_ACPUT( VALUE( : VLEN ), INDF1, COMP, IAX,
     :                         STATUS )

            END IF
         END DO

* Get the normalization flag for this axis from the source and put it
*  into the destination.
         CALL NDF_ANORM( INDF2S, IAX, NORM, STATUS )
         CALL NDF_ASNRM( NORM, INDF1, IAX, STATUS )

      END DO

      END
