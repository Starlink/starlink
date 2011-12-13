      SUBROUTINE CON_RAXES( RLOC, CUBID, STATUS )
*+
*  Name:
*     CON_RAXES

*  Purpose:
*     Constructs the NDF axis components for an Asterix data cube.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_CAXES( RLOC, CUBID; STATUS )

*  Description:
*     Construct the NDF axis components for an Asterix data cube.

*  Arguments:
*     RLOC  =  CHARACTER*(*) (Given)
*        Locator to the input cube.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Generate each of the three axis components in turn.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
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
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     14/7/97 (ACD):
*       Original version.
*     3/9/97  (ACD):
*       First stable version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

*  Arguments Given:
      CHARACTER*(*) RLOC
      INTEGER CUBID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string less trailing blanks

*  Local Variables:
      INTEGER AXPTR( 3 )         ! Axis pointers
      CHARACTER*(DAT__SZLOC) ARLOC ! Locator to the axis data array
      CHARACTER*(DAT__SZLOC) AXLOC ! Locator to the axis component
      CHARACTER*(DAT__SZLOC) CELLOC ! Locator to the axis cell (array
                                 ! element)
      REAL ESCALE                ! Energy axis scale factor
      CHARACTER*(DAT__SZLOC) ESCLOC ! Locator to the energy-axis scale
                                 ! factor
      REAL EZEROP                ! Energy axis zero point
      CHARACTER*(DAT__SZLOC) EZPLOC ! Locator to the energy-axis zero point
      CHARACTER*80 LABEL         ! Energy-axis label
      CHARACTER*(DAT__SZLOC) LABLOC ! Locator to the energy-axis label
      INTEGER NAXIS( 3 )         ! Number of elements in each axis
      INTEGER NC                 ! Used length of string
      REAL SCALE                 ! Axis scale factor (arcmin)
      CHARACTER*(DAT__SZLOC) SCLLOC ! Locator to the axis scale factor
      CHARACTER*80 UNITS         ! Energy-axis units
      CHARACTER*(DAT__SZLOC) UNTLOC ! Locator to the energy-axis units

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  First axis; energy.
         CALL NDF_AMAP( CUBID, 'CENTRE', 1, '_REAL', 'WRITE',
     :               AXPTR( 1 ), NAXIS( 1 ), STATUS )

         CALL DAT_FIND( RLOC, 'AXIS', AXLOC, STATUS )
         CALL DAT_CELL( AXLOC, 1, 3, CELLOC, STATUS )
         CALL DAT_FIND( CELLOC, 'DATA_ARRAY', ARLOC, STATUS )

         CALL DAT_FIND( ARLOC, 'SCALE', ESCLOC, STATUS )
         CALL DAT_GET0R( ESCLOC, ESCALE, STATUS )

         CALL DAT_FIND( ARLOC, 'BASE', EZPLOC, STATUS )
         CALL DAT_GET0R( EZPLOC, EZEROP, STATUS )

         CALL CON_LAXIS( EZEROP, ESCALE, NAXIS( 1 ),
     :                   %VAL( CNF_PVAL( AXPTR( 1 ) ) ), STATUS )
         CALL NDF_AUNMP( CUBID, 'CENTRE', 1, STATUS )

         CALL DAT_FIND( CELLOC, 'LABEL', LABLOC, STATUS )
         CALL DAT_GET0C( LABLOC, LABEL, STATUS )
         NC = CHR_LEN( LABEL )
         CALL NDF_ACPUT( LABEL( :NC ), CUBID, 'LABEL', 1, STATUS )

         CALL DAT_FIND( CELLOC, 'UNITS', UNTLOC, STATUS )
         CALL DAT_GET0C( UNTLOC, UNITS, STATUS )
         NC = CHR_LEN( UNITS)
         CALL NDF_ACPUT( UNITS( :NC ), CUBID, 'UNITS', 1, STATUS )

*  Second axis; X offset.
         CALL NDF_AMAP( CUBID, 'CENTRE', 2, '_REAL', 'WRITE',
     :                  AXPTR( 2 ), NAXIS( 2 ), STATUS )

         CALL DAT_FIND( RLOC, 'AXIS', AXLOC, STATUS )
         CALL DAT_CELL( AXLOC, 1, 1, CELLOC, STATUS )
         CALL DAT_FIND( CELLOC, 'DATA_ARRAY', ARLOC, STATUS )
         CALL DAT_FIND( ARLOC, 'SCALE', SCLLOC, STATUS )

         CALL DAT_GET0R( SCLLOC, SCALE, STATUS )

*  Convert the scale from degrees to arcmintes.
         SCALE = SCALE * 6.0E1

         CALL CON_SAXIS( SCALE, NAXIS( 2 ), %VAL(CNF_PVAL(AXPTR( 2 ))),
     :                   STATUS )

         CALL NDF_AUNMP( CUBID, 'CENTRE', 2, STATUS )
         CALL NDF_ACPUT( 'X offset', CUBID, 'LABEL', 2,
     :     STATUS )
         CALL NDF_ACPUT( 'Arcmin', CUBID, 'UNITS', 2, STATUS )

*  Third axis; Declination offset.
         CALL NDF_AMAP( CUBID, 'CENTRE', 3, '_REAL', 'WRITE',
     :                  AXPTR( 3 ), NAXIS( 3 ), STATUS )

         CALL DAT_FIND( RLOC, 'AXIS', AXLOC, STATUS )
         CALL DAT_CELL( AXLOC, 1, 2, CELLOC, STATUS )
         CALL DAT_FIND( CELLOC, 'DATA_ARRAY', ARLOC, STATUS )
         CALL DAT_FIND( ARLOC, 'SCALE', SCLLOC, STATUS )

         CALL DAT_GET0R( SCLLOC, SCALE, STATUS )

*  Convert the scale from degrees to arcmin.
         SCALE = SCALE * 6.0E1

         CALL CON_SAXIS( SCALE, NAXIS( 3 ),
     :                   %VAL( CNF_PVAL( AXPTR( 3 ) ) ), STATUS )

         CALL NDF_AUNMP( CUBID, 'CENTRE', 3, STATUS )
         CALL NDF_ACPUT( 'Y offset', CUBID, 'LABEL', 3, STATUS )
         CALL NDF_ACPUT( 'Arcmin', CUBID, 'UNITS', 3, STATUS )

*  Report any error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CON_RAXES_ERR', 'CON_RAXES: Failure '/
     :        /'setting the axis components for the output cube.',
     :        STATUS )
         END IF

      END IF

      END
