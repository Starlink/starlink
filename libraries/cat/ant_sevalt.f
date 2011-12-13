
*+
*  Name:
*    ant_sevalt

*  Copyright:
*    Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*    All Rights Reserved.

*  Licence:
*    This program is free software; you can redistribute it and/or
*    modify it under the terms of the GNU General Public License as
*    published by the Free Software Foundation; either version 2 of
*    the License, or (at your option) any later version.
*
*    This program is distributed in the hope that it will be
*    useful,but WITHOUT ANY WARRANTY; without even the implied
*    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*    PURPOSE. See the GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*    02110-1301, USA

*-
*
*    Byte version.

      SUBROUTINE ANT_SEVALB (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      BYTE VALUE
      INTEGER STATUS

      BYTE      VALUEV, BUFFV
      BYTE      VALUEB, BUFFB
      INTEGER*2 VALUEU, BUFFU
      INTEGER*2 VALUEW, BUFFW
      INTEGER   VALUEI, BUFFI
      REAL      VALUER, BUFFR
      DOUBLE PRECISION VALUED, BUFFD
      LOGICAL   VALUEL, BUFFL
      CHARACTER VALUEC*100, BUFFC*100
      LOGICAL CONVOK

      INCLUDE 'CAT_PAR'

      CALL ANT_XEVALD (XID, NROW, NULFLG, BUFFD, STATUS)

      CALL CAT1_TCNVT (CAT__TYPED, BUFFV, BUFFB, BUFFU, BUFFW,
     :  BUFFI, BUFFR, BUFFD, BUFFL, BUFFC, CAT__TYPEB,
     :  VALUEV, VALUEB, VALUEU, VALUEW, VALUEI, VALUER, VALUED,
     :  VALUEL, VALUEC, CONVOK, STATUS)

      VALUE = VALUEB

      END

*
*    Word version.

      SUBROUTINE ANT_SEVALW (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      INTEGER*2 VALUE
      INTEGER STATUS

      BYTE      VALUEV, BUFFV
      BYTE      VALUEB, BUFFB
      INTEGER*2 VALUEU, BUFFU
      INTEGER*2 VALUEW, BUFFW
      INTEGER   VALUEI, BUFFI
      REAL      VALUER, BUFFR
      DOUBLE PRECISION VALUED, BUFFD
      LOGICAL   VALUEL, BUFFL
      CHARACTER VALUEC*100, BUFFC*100
      LOGICAL CONVOK

      INCLUDE 'CAT_PAR'

      CALL ANT_XEVALD (XID, NROW, NULFLG, BUFFD, STATUS)

      CALL CAT1_TCNVT (CAT__TYPED, BUFFV, BUFFB, BUFFU, BUFFW,
     :  BUFFI, BUFFR, BUFFD, BUFFL, BUFFC, CAT__TYPEW,
     :  VALUEV, VALUEB, VALUEU, VALUEW, VALUEI, VALUER, VALUED,
     :  VALUEL, VALUEC, CONVOK, STATUS)

      VALUE = VALUEW

      END

*
*    Integer version.

      SUBROUTINE ANT_SEVALI (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      INTEGER VALUE
      INTEGER STATUS

      BYTE      VALUEV, BUFFV
      BYTE      VALUEB, BUFFB
      INTEGER*2 VALUEU, BUFFU
      INTEGER*2 VALUEW, BUFFW
      INTEGER   VALUEI, BUFFI
      REAL      VALUER, BUFFR
      DOUBLE PRECISION VALUED, BUFFD
      LOGICAL   VALUEL, BUFFL
      CHARACTER VALUEC*100, BUFFC*100
      LOGICAL CONVOK

      INCLUDE 'CAT_PAR'

      CALL ANT_XEVALD (XID, NROW, NULFLG, BUFFD, STATUS)

      CALL CAT1_TCNVT (CAT__TYPED, BUFFV, BUFFB, BUFFU, BUFFW,
     :  BUFFI, BUFFR, BUFFD, BUFFL, BUFFC, CAT__TYPEI,
     :  VALUEV, VALUEB, VALUEU, VALUEW, VALUEI, VALUER, VALUED,
     :  VALUEL, VALUEC, CONVOK, STATUS)

      VALUE = VALUEI

      END

*
*    Real version.

      SUBROUTINE ANT_SEVALR (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      REAL VALUE
      INTEGER STATUS

      BYTE      VALUEV, BUFFV
      BYTE      VALUEB, BUFFB
      INTEGER*2 VALUEU, BUFFU
      INTEGER*2 VALUEW, BUFFW
      INTEGER   VALUEI, BUFFI
      REAL      VALUER, BUFFR
      DOUBLE PRECISION VALUED, BUFFD
      LOGICAL   VALUEL, BUFFL
      CHARACTER VALUEC*100, BUFFC*100
      LOGICAL CONVOK

      INCLUDE 'CAT_PAR'

C     write(17, 1000) status
C1000 format(1x, 'SEVALR - status on entry:', I10)

      CALL ANT_XEVALD (XID, NROW, NULFLG, BUFFD, STATUS)

C     write(17, 1001) status
C1001 format(1x, 'SEVALR - status after ANT_XVALD:', I10)

      CALL CAT1_TCNVT (CAT__TYPED, BUFFV, BUFFB, BUFFU, BUFFW,
     :  BUFFI, BUFFR, BUFFD, BUFFL, BUFFC, CAT__TYPER,
     :  VALUEV, VALUEB, VALUEU, VALUEW, VALUEI, VALUER, VALUED,
     :  VALUEL, VALUEC, CONVOK, STATUS)

C     write(17, 1002) status
C1002 format(1x, 'SEVALR - status after CAT1_TCNVT:', I10)

      VALUE = VALUER
C      value = buffd

C     write(17, 1003) status
C1003 format(1x, 'SEVALR - status on exit:', I10)

      END

*
*    Double precision version.

      SUBROUTINE ANT_SEVALD (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      DOUBLE PRECISION VALUE
      INTEGER STATUS

      CALL ANT_XEVALD (XID, NROW, NULFLG, VALUE, STATUS)

      END

*
*    Logical version.

      SUBROUTINE ANT_SEVALL (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      LOGICAL VALUE
      INTEGER STATUS

      CALL ANT_XEVALL (XID, NROW, NULFLG, VALUE, STATUS)

      END

*
*    Character version.

      SUBROUTINE ANT_SEVALC (XID, NROW, NULFLG, VALUE, STATUS)
      IMPLICIT NONE

      INTEGER XID, NROW
      LOGICAL NULFLG
      CHARACTER VALUE*(*)
      INTEGER STATUS

      CALL ANT_XEVALC (XID, NROW, NULFLG, VALUE, STATUS)

      END
