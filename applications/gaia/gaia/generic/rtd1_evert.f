      SUBROUTINE RTD1_EVERT ( DX, IX, SVECT )
C----------------------------------------------------------------------
C   Compute subscript offset and weights for interpolating at a
C   specified position in a vector.  Interpolation is done using a
C   group of pixels centered on the specified position. The order of
C   interpolation is specified by integer MORD in subroutine SETCOF.
C   We do linear interpolation for MORD=0, cubic for 1, and quintic
C   for 2. If MORD=1 (i.e., cubic interpolation) we will be using
C   four pixels in the interpolation.  From the Everett interpolation
C   package originally coded by Larry Goad at KPNO. This version has been
C   lifted almost unchanged from the AIPS package - the only change has
C   been that the include files are now explicitly included in the
C   program body.                              KS / CIT  22nd March 1984
C
C   Inputs:  DX     DBLE     Position in vector
C   Outputs: IX     I*2     Offset to start
C            SVECT  DBLE(*)  Weights

C  Licence:
C     This program is free software; you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation; either version 2 of the
C     License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be
C     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
C     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
C     02110-1301, USA

C----------------------------------------------------------------------
      INTEGER*2 IX
      DOUBLE PRECISION SVECT(1), DX
      INTEGER*2 NR, IR0, IV0, MC, IR
      DOUBLE PRECISION U, W, CW, CU, W2, U2, R0
C                             Everett Interpolation internal variables:
      DOUBLE PRECISION BCOEF(49), SV(14)
      INTEGER*2 MORD2, IS0, NVALS
      COMMON /RTD1_CEVI/ BCOEF, SV, MORD2, IS0, NVALS
      SAVE /RTD1_CEVI/
      DATA R0 /0.0/
C-----------------------------------------------------------------------
      IX = DX
      U  = DX - IX
      IX = IX - IS0
      DO 5 IR = 1,NVALS
         SVECT(IR) = R0
 5    CONTINUE
C                                       distance of point from cells
      W  = 1.0 - U
      CW = W
      CU = U
      W2 = W * W
      U2 = U * U
      SVECT(IS0)   = W
      SVECT(IS0+1) = U
C                                       leave if on cell or linear
      IF ( (U.EQ.0.0D0) .OR. (MORD2.EQ.0) ) GO TO 999
      NR  = 1
      IR0 = 0
      IV0 = IS0 - 1
      DO 20 MC = 1,MORD2
C                                       (IR0=MC*MC):
         IR0 = IR0 + NR
C                                       (NR=2*MC+1):
         NR  = NR  + 2
C                                       (IV0=IS0-MC-1):
         IV0 = IV0 - 1
         CU  = CU * (U2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         CW  = CW * (W2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         DO 10 IR = 1,NR
            SVECT(IV0+IR)   = SVECT(IV0+IR)   + CW * BCOEF(IR0+IR)
            SVECT(IV0+IR+1) = SVECT(IV0+IR+1) + CU * BCOEF(IR0+IR)
 10      CONTINUE
 20   CONTINUE
C
 999  RETURN
      END
