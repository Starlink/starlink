      PROGRAM GKSPAR_TEST
*+
*  Name:
*    gkspar_test

*  Purpose:
*    Simple test to see that the GKS routine are available to be linked
*    against.

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
*    {enter_authors_here}

*-
      INTEGER STATUS
      STATUS = 0

      PRINT *,'This test program only links against the GKSPAR routines'
      PRINT *,'It does not actually call them.'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL GKS_ANNUL
      CALL GKS_ASSOC
      CALL GKS_DEACT
      CALL GKS_CANCL
      CALL GKS_GSTAT
      CALL GKS_RESET
  999 CONTINUE
      print *,'ending'
      END
