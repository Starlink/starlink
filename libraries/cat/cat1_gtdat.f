      SUBROUTINE CAT1_GTDAT (DATE, STATUS)
*+
*  Name:
*     CAT1_GTDAT
*  Purpose:
*     Get the current date in the StarBase format.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_GTDAT (DATE; STATUS)
*  Description:
*     Get the current date in the StarBase format.
*
*     This is a dummy implementation which returns 0.0D0.  A proper
*     implementation would obtain the current date and time from the
*     host operating system and convert it into the StarBase format.
*  Arguments:
*     DATE  =  DOUBLE PRECISION (Returned)
*        Current date in the StarBase format.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*
*  Implementation Deficiencies:
*     This is a dummy implementation which returns 0.0D0.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/7/93 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Returned:
      DOUBLE PRECISION
     :  DATE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DATE = 0.0D0

      END IF

      END
