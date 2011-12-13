      SUBROUTINE PGP_TEST(STATUS)
*+
*  Name:
*     pgp_test

*  Purpose:
*     Demonstration program for PGPLOT: draw a histogram.

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

*  Notes:
*     PGRAND and PGRNRM are taken from the PGPLOT demo programs
*     and are copyright (C) 1995 California Institute of Technology.

*  Authors:
*     {enter_new_authors_here}

*-

      INCLUDE 'SAE_PAR'

      INTEGER  I, ISEED, PGBEG, STATUS, LUN
      REAL     DATA(1000), X(620), Y(620)
      REAL     PGRNRM
*.

C
C Call PGP_ASSOC to initiate PGPLOT and open the output device; PGP_ASSOC
C will prompt the user to supply the device name and type.
C
      CALL PGP_ASSOC('DEVICE','WRITE',1,1,LUN,STATUS)
      IF (STATUS.NE.SAI__OK) RETURN
C
C Call RNGAUS to obtain 1000 samples from a normal distribution.
C
      ISEED = 5778731
      DO 10 I=1,1000
          DATA(I) = PGRNRM(ISEED)
   10 CONTINUE
C
C Draw a histogram of these values.
C
      CALL PGHIST(1000,DATA,-3.1,3.1,31,0)
      CALL PGLAB('Variate', ' ',
     $             'PGPLOT Example 2 - Gaussian distribution')
C
C Superimpose the theoretical distribution.
C
      DO 20 I=1,620
          X(I) = -3.1 + 0.01*(I-1)
          Y(I) = 0.2*1000./SQRT(2.*3.14159265)*EXP(-0.5*X(I)*X(I))
   20 CONTINUE
      CALL PGLINE(620,X,Y)
C
C Don't forget to call PGP_ANNULL and PGP_DEACT!
C
      CALL PGP_ANNUL(LUN, STATUS)
      CALL PGP_DEACT(STATUS)
C
999   CONTINUE
      END

      REAL FUNCTION PGRNRM (ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a normally distributed deviate with zero mean and unit
C variance. The routine uses the Box-Muller transformation of uniform
C deviates. For a more efficient implementation of this algorithm,
C see Press et al., Numerical Recipes, Sec. 7.2.
C
C Arguments:
C  ISEED  (in/out) : seed used for PGRAND random-number generator.
C
C Subroutines required:
C  PGRAND -- return a uniform random deviate between 0 and 1.
C
C History:
C  1995 Dec 12 - TJP.
C-----------------------------------------------------------------------
      REAL R, X, Y, PGRAND
C
 10   X = 2.0*PGRAND(ISEED) - 1.0
      Y = 2.0*PGRAND(ISEED) - 1.0
      R = X**2 + Y**2
      IF (R.GE.1.0) GOTO 10
      PGRNRM = X*SQRT(-2.0*LOG(R)/R)
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGRAND(ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a uniform random deviate between 0.0 and 1.0.
C
C NOTE: this is not a good random-number generator; it is only
C intended for exercising the PGPLOT routines.
C
C Based on: Park and Miller's "Minimal Standard" random number
C   generator (Comm. ACM, 31, 1192, 1988)
C
C Arguments:
C  ISEED  (in/out) : seed.
C-----------------------------------------------------------------------
      INTEGER   IM, IA, IQ, IR
      PARAMETER (IM=2147483647)
      PARAMETER (IA=16807, IQ=127773, IR= 2836)
      REAL      AM
      PARAMETER (AM=128.0/IM)
      INTEGER   K
C-
      K = ISEED/IQ
      ISEED = IA*(ISEED-K*IQ) - IR*K
      IF (ISEED.LT.0) ISEED = ISEED+IM
      PGRAND = AM*(ISEED/128)
      RETURN
      END
