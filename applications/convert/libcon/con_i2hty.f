      SUBROUTINE CON_I2HTY( INTTYP, HDSTYP, STATUS )
*+
*  Name:
*     CON_I2HTY

*  Purpose:
*     Converts Interim character format code to its HDS equivalent.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CON_I2HTY( INTTYP, HDSTYP, STATUS )

*  Arguments:
*     INTTYP = CHARACTER * 2 (Given)
*        The Interim data type: SB, SW, SL, DP, R, UB, UW.  If it is
*        not one of these, an error report is made and SAI__ERROR status
*        will be set.
*     NBYTES = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS data type.  If the input data type is invalid, this is
*        a null string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Converts an Interim character data type, e.g. SW, to its HDS
*     form, e.g. _WORD.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     1992 February 4 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * )
     :  INTTYP

*  Arguments Returned:
      CHARACTER * ( * )
     :  HDSTYP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTYP
      PARAMETER( MAXTYP=7 )

*  Local Variables:
      INTEGER
     :  I                        ! Loop counter

      CHARACTER * 2
     :  HTYPES( MAXTYP ) * ( DAT__SZTYP ), ! The table of HDS numeric
                                 ! data types
     :  ITYPES( MAXTYP ) * ( 2 ) ! The table of Interim numeric data
                                 ! types

*  Local Data:
      DATA ITYPES/'SB', 'SW', 'SL',  'R', 'DP', 'UB', 'UW' /
      DATA HTYPES/'_BYTE','_WORD','_INTEGER','_REAL','_DOUBLE',
     :            '_UBYTE','_UWORD'/

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a match is found.  Copy the type string to the output
*  argument. If none is found an error report will be made, otherwise
*  the routine exits.
      HDSTYP = ' '
      DO I = 1, MAXTYP
         IF ( INTTYP .EQ. ITYPES( I ) ) THEN
            HDSTYP = HTYPES( I )
            GOTO 999
         END IF
      END DO

*  If an error occurred, then report a contextual message.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'TYPE', INTTYP )
      CALL ERR_REP( 'CON_I2HTY_ERR',
     :  'Invalid Interim data type ^TYPE.  Unable to convert it to '/
     :  /'an HDS equivalent.', STATUS )

  999 CONTINUE
      END
