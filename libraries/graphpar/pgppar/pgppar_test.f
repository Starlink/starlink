      PROGRAM PGPPAR_TEST
*+
*  Name:
*     pgp_test

*  Purpose:
*     Simple test to see that the PGPPAR routine are available for linking

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

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     {enter_new_authors_here}

*-

      INTEGER STATUS
*.

      STATUS = 0

      PRINT *,'This test program only links against the PGPPAR routines'
      PRINT *,'It does not actually call them.'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL PGP_ANNUL
      CALL PGP_ASSOC
      CALL PGP_CANCL
      CALL PGP_DEACT

  999 CONTINUE
      print *,'ending'
      END
