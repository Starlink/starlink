*+  NTHMIN - returns the n smallest values in a array of data

      SUBROUTINE FPNTHMIN ( ARRAY, NUMBER, N, STACK)

*    Description :
*
*     This routine takes a set of data values and returns a stack
*     containing the n smallest values in that array.
*
*    Invocation :
*
*     CALL NTHMIN( ARRAY, NUMBER, N, STACK, STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise stack entries to very high value
*     For all array values
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
*
*    History :
*
*     14-01-1986 :  First implementation - more or less a straight
*                :  copy of routine of same name written by Rodney
*                :  Warren-Smith for Starlink package EDRS.
*                :  (REVA::MJM)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    NUMBER,             ! number of values in input data array
     :    N                   ! specifies n in "n-th smallest value"

      REAL
     :    ARRAY( NUMBER )     ! input data values

*    Export :

      REAL
     :    STACK( N )          ! returned stack containing the n
                              ! smallest values in ARRAY

*    Local Constants :

      REAL
     :    MAXIMUM             ! maximum value used to initialise stack
      PARAMETER( MAXIMUM  =  1.0E20 )

*    Local variables :

      INTEGER
     :    LOCAT,              ! locator used in sort
     :    I, J                ! array counters

*-

*    start by initialising the stack entries to a high value
      DO  I  =  1, N
         STACK( I )  =  MAXIMUM
      END DO

*    now loop through all members of the input array
      DO  J  =  1, NUMBER

*       check if current array value is less than the top stack
*       value - continue with sort if it is
         IF( ARRAY( J ) .LT. STACK( 1 ) ) THEN

*          current array value is less than the top of stack value,
*          and thus belongs in the stack. Run down the stack checking
*          where the value belongs
            DO  LOCAT  =  2, N

*             check array value against current stack entry
               IF( ARRAY( J ) .LT. STACK( LOCAT ) ) THEN

*                array value belongs somewhere below the current
*                stack entry - move the current stack entry up one
*                to make space
                  STACK( LOCAT - 1 )  =  STACK( LOCAT )

               ELSE

*                the array value is greater than the current stack
*                entry - insert the array value one place above the
*                current stack position
                  STACK( LOCAT - 1 )  =  ARRAY( J )

               END IF

*             check to see if we have reached the stack bottom
               IF( LOCAT .EQ. N ) THEN

*                we are at the bottom stack position - insert array
*                value here
                  STACK( N )  =  ARRAY( J )

               END IF

*          end of scan down stack
            END DO

*       end of IF CURRENT ARRAY VALUE LESS THAN TOP STACK ENTRY statement
         END IF

*    end of loop through all array values
      END DO


*    that's it - return

      END

