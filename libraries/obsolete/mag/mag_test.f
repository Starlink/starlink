      PROGRAM MAG_TEST
*+
*  Name:
*     mag_test

*  Purpose:
*     Simple test to see that the MAG routine are available to be linked against.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     KFH: K F Hartley (RAL)

*  History:
*     12-DEC-1991 (KFH):
*        Original version

*-
      INTEGER STATUS
      STATUS = 0

      PRINT *,'This test program only links against the MAG routines.'
      PRINT *,'It does not actually call them, so a magnetic tape is'
     ://' not needed.'
      IF( STATUS .NE. -12345 ) GOTO 999
      CALL MAG_ALOC
      CALL MAG_ANNUL
      CALL MAG_ASSOC
      CALL MAG_CANCL
      CALL MAG_CLOSE
      CALL MAG_DEACT
      CALL MAG_DEAL
      CALL MAG_DISM
      CALL MAG_JEOV
      CALL MAG_JUMP
      CALL MAG_MOUNT
      CALL MAG_MOVE
      CALL MAG_POS
      CALL MAG_READ
      CALL MAG_REW
      CALL MAG_SET
      CALL MAG_SKIP
      CALL MAG_WRITE
      CALL MAG_WTM
  999 CONTINUE
      print *,'ending'
      END
