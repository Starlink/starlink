      SUBROUTINE TEST_SORT(STATUS)
*+
*  Name:
*     TEST_SORT

*  Purpose:
*     Test CHR_SORT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TEST_SORT(STATUS)

*  Description:
*     Test CHR_SORT.
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
*     CHR_SORT

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

*  External References:
      EXTERNAL CHR_SCOMP
      LOGICAL CHR_SCOMP

*  Local Constants:
      INTEGER MXSARY
      PARAMETER (MXSARY = 10)

*  Local Variables:
      INTEGER I,ISTAT,           ! INTEGER values
     :        NSORT
      CHARACTER*5 SARRAY(MXSARY)
      CHARACTER*5 STARTA(MXSARY)
      CHARACTER*5 LIST(MXSARY)

      DATA STARTA/'Mary','had','a','little','lamb.','Its','fleec','was',
     :          'white','a'/
      DATA LIST/'Its','Mary','a','fleec','had','lamb.','little','was',
     :          'white',' '/

*.

*    Test CHR_SORT

      ISTAT = SAI__OK
      DO 110 I = 1, MXSARY
         SARRAY(I) = STARTA(I)
110   CONTINUE

      CALL CHR_SORT( CHR_SCOMP, MXSARY, SARRAY, NSORT )
      IF ( NSORT .NE. 9 ) THEN
         PRINT *, 'CHR_SORT FAILS - NSORT is:', NSORT, ' should be: 9'
      ELSE
         DO 120 I = 1, NSORT
            IF ( SARRAY(I) .NE. LIST(I) ) ISTAT = SAI__ERROR
120      CONTINUE
         IF ( ISTAT .EQ. SAI__OK ) THEN
            PRINT *, 'CHR_SORT OK'
         ELSE
            PRINT *, 'CHR_SORT FAILS - returned array, correct array'
            DO 130 I = 1, MXSARY
               PRINT *, SARRAY(I),LIST(I)
130         CONTINUE
         ENDIF
      ENDIF

      END
