      SUBROUTINE NORMAL ( INVAL, SIGMA, SEED, OUTVAL, STATUS )
*+
*  Name:
*     NORMAL

*  Purpose:
*     Takes a value and returns a normally distributed noisy.

*  Language:
*     Starlink

*  Invocation:
*     CALL NORMAL( INVAL, SIGMA, SEED, OUTVAL, STATUS )

*  Description:
*     This routine takes as input a number and returns a number
*     which is the input number plus or minus a random amount
*     of normally distributed noise. Uses a Box-Mueller algorithm
*     to generate a fairly good normal distribution.

*  Arguments:
*     INVAL  =  REAL( READ )
*        Input value to which noise is to be added
*     SIGMA  =  REAL ( READ )
*        Standard deviation of  normal distribution
*     SEED  =  REAL ( READ, WRITE )
*        Seed for random number generator.
*     OUTVAL  =  REAL ( WRITE )
*        Output value which has random noise added
*     STATUS = INTEGER ( READ )
*        Global status value

*  Algorithm:
*     If error on entry
*        OUTVAL = INVAL
*     Else
*        Initialise finished flag to false
*        Do while not finished
*          Get random numbers from the seed
*          If valid numbers generated
*             Modify the input value according to the random
*              numbers and the Box-Mueller transform
*             Return this value in OUTVAL
*             Set finished flag to true
*          Endif
*        Enddo
*     Endif
*     Return

*  Copyright:
*     Copyright (C) 1986, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*     {enter_new_authors_here}

*  History:
*     14-02-1986 : First implementation (REVA::MJM)
*     1986 Aug 12: Completed prologue and nearly conformed to
*        Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 : Renamed parameter section arguments and tidied
*        (RL.STAR::CUR).
*     1992 Mar 17: Used portable random-number generation (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE            ! no implicit typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'        ! SSE global definitions


*  Arguments Given:
      REAL
     :     INVAL,
     :     SIGMA


*  Arguments Given and Returned:
      REAL
     :     SEED


*  Arguments Returned:
      REAL
     :     OUTVAL


*  Status:
      INTEGER  STATUS

*    External References:
      REAL SLA_RANDOM           ! Random-number generator


*  Local Variables:
      REAL
     :     RA, RB, RR, R        ! random numbers used by algorithm

      LOGICAL
     :     FINSHD               ! flag variable used to check that a
                                ! valid random number has been generated


*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         OUTVAL  =  INVAL
      ELSE

*       initialise logical flag

         FINSHD  =  .FALSE.

*       loop until a valid random number has been generated

         DO WHILE ( .NOT. FINSHD )

*          generate two uncorrelated random numbers between -1 and 1.
*          SLA_RANDOM returns values in the range 0 to 1.

            RA  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RA  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RB  =  -1.0 + 2.0 * SLA_RANDOM( SEED )
            RB  =  -1.0 + 2.0 * SLA_RANDOM( SEED )

*          get another from these that will lie between 0 and 2

            RR  =  RA*RA + RB*RB

*          accept only those that lie between 0 and 1 - otherwise start
*          again

            IF ( RR .LT. 1.0 ) THEN

*             generate the Box-Mueller transform

               R  =  SQRT( -2.0 * ALOG( RR ) / RR )
               OUTVAL  =  ( ABS( SIGMA ) * RA * R ) + INVAL

*             set the finished flag to true - exit now

               FINSHD  =  .TRUE.

            END IF

         END DO

      END IF

*    return and end

      END

