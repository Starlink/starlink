*+  NTHMIN - returns the n smallest values in a array of data

      SUBROUTINE NTHMIN ( ARRAY, DIMS, N, STACK, STATUS )
*
*    Description :
*
*     This routine takes a set of data values and returns a stack
*     containing the N smallest values in that array.
*
*    Invocation :
*
*     CALL NTHMIN( ARRAY, DIMS, N, STACK, STATUS )
*
*    Arguments :
*
*     ARRAY ( DIMS )  =  REAL ( READ )
*         Input array of data values
*     DIMS  =  INTEGER ( READ )
*         Dimension of input array
*     N  =  INTEGER ( READ )
*         The number of values to be stored in stack, i.e. specifies
*         n in "n-th smallest value."
*     STACK ( N )  =  REAL ( WRITE )
*         Ordered n smallest values in the input array
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise stack entries to very high value
*     For all valid array values
*        If current array value is less than top stack entry then
*           For all stack entries below first one
*              If current array value less than current stack entry then
*                 Set current-1 stack entry = current stack entry
*              Elseif at bottom of stack
*                 Set bottom stack entry to current array value
*              Else
*                 Set current-1 stack entry = current array value
*              Endif
*           Endfor
*        Endif
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     14-01-1986 :  First implementation - more or less a straight
*                   copy of routine of same name written by Rodney
*                   Warren-Smith for Starlink package EDRS. (REVA::MJM)
*     1986 Aug 12:  Completed prologue and nearly conformed to
*                   Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 :  Renamed parameters -> arguments section in prologue
*                   and added bad-pixel handling (RL.CUR::STAR).
*     1992 Jan 7 :  Replaced 'BAD_PAR' with 'PRM_PAR' (DUVAD::NE).
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'          ! SSE global definitions

      INCLUDE 'PRM_PAR'


*    Import :

      INTEGER
     :    DIMS,
     :    N

      REAL
     :    ARRAY( DIMS )

*    Export :

      REAL
     :    STACK( N )

*    Status :

      INTEGER  STATUS

*    Local Constants :

      REAL
     :    MAXMUM              ! Maximum value used to initialise stack
      PARAMETER( MAXMUM  =  1.0E20 )

*    Local variables :

      INTEGER
     :    LOCAT,              ! locator used in sort
     :    I, J                ! array counters

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    start by initialising the stack entries to a high value

      DO  I  =  1, N
         STACK( I )  =  MAXMUM
      END DO

*    now loop through all members of the input array

      DO  J  =  1, DIMS

*       test if pixel is valid

         IF ( ARRAY( J ) .NE. VAL__BADR ) THEN

*          check if current array value is less than the top stack
*          value - continue with sort if it is

            IF( ARRAY( J ) .LT. STACK( 1 ) ) THEN

*             current array value is less than the top of stack value,
*             and thus belongs in the stack. Run down the stack checking
*             where the value belongs

               DO  LOCAT  =  2, N

*                check array value against current stack entry

                  IF( ARRAY( J ) .LT. STACK( LOCAT ) ) THEN

*                   array value belongs somewhere below the current
*                   stack entry - move the current stack entry up one
*                   to make space

                     STACK( LOCAT - 1 )  =  STACK( LOCAT )

                  ELSE

*                   the array value is greater than the current stack
*                   entry - insert the array value one place above the
*                   current stack position

                     STACK( LOCAT - 1 )  =  ARRAY( J )

                  END IF

*                check to see if we have reached the stack bottom

                  IF( LOCAT .EQ. N ) THEN

*                   we are at the bottom stack position - insert array
*                   value here

                     STACK( N )  =  ARRAY( J )

                  END IF

*             end of scan down stack

               END DO

*          end of if-current-array-value-less-than-top-stack-entry
*          statement

            END IF

*       end of bad pixel check

         END IF

*    end of loop through all array values

      END DO

 999  CONTINUE

*    that's it - return

      END

* $Id$
