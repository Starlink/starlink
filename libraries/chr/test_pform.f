      SUBROUTINE TEST_PFORM(STATUS)
*+
*  Name:
*     TEST_PFORM

*  Purpose:
*     Test CHR_PFORM.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_PFORM(STATUS)

*  Description:
*     Test CHR_PFORM.
*     If any failure occurs, return STATUS = SAI__ERROR.
*     Otherwise, STATUS is unchanged.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The status of the tests.

*  Copyright:
*     Copyright (C) 1989, 1993, 1994 Science & Engineering Research Council.
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
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     14-SEP-1993 (ACC)
*        Modularised version: broken into one routine for each of 5 main
*        categories of tests.
*     02-MAR-1994 (ACC)
*        Second modularised version: broken further into one routine for
*        each of subroutine tested.  This subroutine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     CHR_PFORM

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
*     None

*  Arguments Returned:
      INTEGER STATUS

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  Local Constants:
      INTEGER MXPAR             ! Size of PARRAY; used in CHR_PFORM
      PARAMETER (MXPAR = 4)
      LOGICAL JUST
      PARAMETER (JUST = .TRUE.)
      LOGICAL NOJUST
      PARAMETER (NOJUST = .FALSE.)

*  Local Variables:
      INTEGER I,ISTAT,IPOSN      ! INTEGER values
      CHARACTER*32 PARRAY(MXPAR)
      CHARACTER*35 JUSPAR(MXPAR)
      CHARACTER*35 RAGPAR(MXPAR)
      CHARACTER*35 LINE(MXPAR)

      DATA PARRAY(1)/'Mary had a little lamb.  '/
      DATA PARRAY(2)/'It''s fleece was white as snow.  '/
      DATA PARRAY(3)/'And everywhere that Mary went '/
      DATA PARRAY(4)/'the lamb was sure to go.'/
      DATA JUSPAR(1)/'Mary had a little lamb. It''s fleece'/
      DATA JUSPAR(2)/'was white as snow.  And  everywhere'/
      DATA JUSPAR(3)/'that Mary went the lamb was sure to'/
      DATA JUSPAR(4)/'go.                                '/
      DATA RAGPAR(1)/'Mary had a little lamb. It''s fleece'/
      DATA RAGPAR(2)/'was white as snow. And everywhere  '/
      DATA RAGPAR(3)/'that Mary went the lamb was sure to'/
      DATA RAGPAR(4)/'go.                                '/

*.

*    Test CHR_PFORM

      ISTAT = SAI__OK

*    Test ragged right

      IPOSN = 0
      I = 1
*    DO WHILE loop
30    CONTINUE
         IF ( I .GT. MXPAR ) THEN
            PRINT *, 'CHR_PFORM ragged FAILS index too large, ',I
            ISTAT = SAI__ERROR
         ELSE
            CALL CHR_PFORM( MXPAR, PARRAY, NOJUST, IPOSN, LINE(I) )
            I = I + 1
            IF ( IPOSN .NE. 0 ) GO TO 30
         ENDIF

      DO 40 I = 1, MXPAR
         IF ( LINE(I) .NE. RAGPAR(I) ) THEN
            PRINT *, 'CHR_PFORM ragged FAILS'
            PRINT *, 'Output string =',LINE(I)
            PRINT *, 'Should be     =',RAGPAR(I)
            ISTAT = SAI__ERROR
         END IF
40    CONTINUE

*    Test right justify

      IPOSN = 0
      I = 1
*    DO WHILE loop
50    CONTINUE
         IF ( I .GT. MXPAR ) THEN
            PRINT *, 'CHR_PFORM ragged FAILS index too large, ',I
            ISTAT = SAI__ERROR
         ELSE
         CALL CHR_PFORM( MXPAR, PARRAY, JUST, IPOSN, LINE(I) )
            I = I + 1
            IF ( IPOSN .NE. 0 ) GO TO 50
         ENDIF

      DO 60 I = 1,MXPAR
         IF ( LINE(I) .NE. JUSPAR(I) ) THEN
            PRINT *, 'CHR_PFORM justify FAILS'
            PRINT *, 'Output string =',LINE(I)
            PRINT *, 'Should be     =',JUSPAR(I)
            ISTAT = SAI__ERROR
         END IF
60    CONTINUE

      IF (ISTAT .EQ. SAI__ERROR) THEN
         STATUS = SAI__ERROR
      ELSE
         PRINT *, 'CHR_PFORM OK'
      END IF

      END
