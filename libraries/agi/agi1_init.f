************************************************************************
*+  AGI_1INIT - Initialise the dynamic cache

      SUBROUTINE AGI_1INIT

*    Description :
*     Initialise the dynamic cache.
*
*    Invocation :
*     CALL AGI_1INIT
*
*    Method :
*     For every record in the cache
*        Indicate the element of the FIFO array to be empty = -1.
*        Make the forward and inverse transformation numbers = 0.
*     Enddo
*     Initialise the FIFO pointer to be FIFLEN - 1.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*     June 1989  Allowed for the increased number of FIFO's
*     August 1990  Initialise number of pictures
*     January 1993  Initialise CHEAD
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Global variables :
      INCLUDE 'agi_cache'

*    Local variables :
      INTEGER I, J
*-

*   Initialise the number of pictures and the workstation flag
      CNUMPS = 0
      CNUMPW = ' '

*   Initialise the header control block
      CHEAD = -1

*   Initialise each fifo in turn
      DO J = 0, NFIFO - 1

*   Initialise the FIFO array to -1
         DO I = 0, FIFLEN - 1
            FIFO( I, J ) = -1

*   Make sure that the transformation numbers are zero
            CTRFOR( I, J ) = 0
            CTRINV( I, J ) = 0

         ENDDO

*   Initialise the pointer to FIFLEN - 1,
*   so it gets changed to 0 on first pass
         PFIFO( J ) = FIFLEN - 1

      ENDDO

      END

