      SUBROUTINE PARSECON_UPKC( LU, ARRAY, START, END, STATUS )











*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_UPKC( LU, ARRAY, START, END, STATUS )

*  Description:
*     To decode elements of the new-style (packed) compiled form of an
*     interface file for a 1-D CHARACTER array.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to read from
*     ARRAY(*) = <TYPE> (Given)
*        The array of values
*     START = INTEGER (Given)
*        The first element to be inserted
*     END = INTEGER (Given)
*        The last element to be inserted
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Name:
*     PARSECON_UPKC

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (AJC):
*        Original version.
*     21-JAN-1992 (AJC):
*           Remove unused declarations
*        24-MAR-1993 (AJC):
*           Add DAT_PAR for SUBPAR_CMN
*        {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'     ! PARSECON status values

*  Arguments Given:
      INTEGER LU
      CHARACTER*(*) ARRAY( * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR

*  Local Variables:
      INTEGER IOSTAT             ! IO status
      INTEGER NBUFF( SUBPAR__MAXPAR ) ! String length buffer
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the encoded record
      READ ( LU, IOSTAT=IOSTAT ) (NBUFF(I),I=START,END),
     : (ARRAY(I)(1:NBUFF(I)),I=START,END)

      IF ( IOSTAT .NE. 0 ) THEN
*     Read failed
         STATUS = PARSE__READERR
         CALL EMS_FIOER( 'IOSTAT', IOSTAT )
         CALL EMS_REP( 'PCN_UPKC1', 'Read error: ^IOSTAT', STATUS )

      ELSE
*     Space fill the elements
         DO 10, I = START, END
            ARRAY(I)(NBUFF(I)+1:) = ' '
10       CONTINUE

      ENDIF

100   CONTINUE

      END
