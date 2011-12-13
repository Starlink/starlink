      SUBROUTINE PARSECON_UPKL( LU, ARRAY, START, END, STATUS )











*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_UPKL( LU, ARRAY, START, END, STATUS )

*  Description:
*     To deccode elements of the new-style (packed) compiled form of an
*     interface file.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to read from
*     ARRAY(*) = LOGICAL (Given)
*        The array of values
*     START = INTEGER (Given)
*        The first element to be inserted
*     END = INTEGER (Given)
*        The last element to be inserted
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Name:
*     PARSECON_UPKL

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     24-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     {enter_changes_here}
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
      LOGICAL ARRAY( * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR

*  Local Variables:
      INTEGER IOSTAT             ! IO status
      LOGICAL VBUFF( SUBPAR__MAXPAR ) ! Values buffer
      INTEGER NBUFF( SUBPAR__MAXPAR ) ! Values count buffer
      LOGICAL LASTV               ! The last value handled
      INTEGER NV                 ! The consecutive value counter
      INTEGER BPT                ! Pointer to element in encoded array
      INTEGER APT                ! Pointer to element in ARRAY
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the encoded record
      READ ( LU, IOSTAT=IOSTAT ) BPT, (NBUFF(I),I=1,BPT),
     : (VBUFF(I),I=1,BPT)

      IF ( IOSTAT .NE. 0 ) THEN
*     Read failed
         STATUS = PARSE__READERR
         CALL EMS_FIOER( 'IOSTAT', IOSTAT )
         CALL EMS_REP( 'PCN_UPK1', 'Read error: ^IOSTAT', STATUS )

      ELSEIF ( BPT .EQ. 0 ) THEN
*     Illegal record
         STATUS = PARSE__READERR
         CALL EMS_REP( 'PCN_UPK2',
     :   'Invalid compiled interface file record', STATUS )

      ELSE
*     Read OK - unpack the record
*     Initialise array pointer
         APT = START

*     For each element in the encoded arrays
         DO 20, I = 1, BPT

*        Get the value count
            NV = NBUFF( I )
*        and the value
            LASTV = VBUFF( I )

*        Fill in the required number of elements of ARRAY
            DO 10, J = 1, NV

               IF ( APT .GT. END ) THEN
*              Too many elements provided
                  STATUS = PARSE__READERR
                  CALL EMS_REP( 'PCN_UPK3',
     :            'Too many elements in compiled interface file',
     :            STATUS )

*              Exit from loops
                  GO TO 100

               ELSE
*              Still within ARRAY - store the value
                  ARRAY( APT ) = LASTV
*              and increment the pointer
                  APT = APT + 1

               ENDIF

10          CONTINUE

20       CONTINUE

      ENDIF

*  Check that ARRAY is filled
      IF ( APT-1 .NE. END ) THEN
         STATUS = PARSE__READERR
         CALL EMS_REP( 'PCN_UPK4',
     :   'Insufficient elements in compiled interface file', STATUS )
      ENDIF

100   CONTINUE

      END
