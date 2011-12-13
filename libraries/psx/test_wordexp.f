      PROGRAM TEST_WORDEXP
*+
*  Name:
*     TEST_WORDEXP

*  Purpose:
*     Test the function PSX_WORDEXP

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     17-APR-2006 (TIMJ):
*         Add prologue.

*-

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      INTEGER CONTEXT
      CHARACTER *(64) EXPAN
      LOGICAL FIRST

*.

      PRINT *, ' '
      PRINT *, ' --  Program TEST_WORDEXP, function PSX_WORDEXP  -- '
      PRINT *, ' '


* Initialize STATUS
      STATUS = SAI__OK
      CALL EMS_BEGIN( STATUS )

      CONTEXT = 0
      CALL PSX_WORDEXP( '$STARLINK_DIR/bin', CONTEXT, EXPAN, STATUS )

      PRINT *, '  Context should be zero: ',CONTEXT
      PRINT *, '  Expand $STARLINK_DIR/bin as ',EXPAN

      PRINT *, ' '
      PRINT *, '  Expand a wildcard ./*.h'
      FIRST = .TRUE.
      DO WHILE ( FIRST .OR. CONTEXT .NE. 0 )
         FIRST = .FALSE.
         CALL PSX_WORDEXP( './*.h', CONTEXT, EXPAN, STATUS )
         PRINT *, '   Found: ',EXPAN
      END DO
      PRINT *, ' '

      PRINT *, ' Expand wildcard when no file matches'
      CONTEXT = 0
      CALL PSX_WORDEXP( './*.boo', CONTEXT, EXPAN, STATUS )
      PRINT *,' Result = ',EXPAN

      CALL EMS_END( STATUS )

      END
