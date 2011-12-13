      PROGRAM TEST_PAGER
*+
*  Name:
*     TEST_PAGER

*  Purpose:
*     Simple test of pager interface

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     04-OCT-2004 (TIMJ):
*         Original.
*     17-APR-2006 (TIMJ):
*         Add Prolog.

*-
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER I

      STATUS = SAI__OK

      CALL SHL_PAGRST( STATUS )


      DO I = 1, 50
         CALL SHL_PAGTXT( 'This is a test of the pager', STATUS )
      END DO

      END
