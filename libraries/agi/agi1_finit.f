************************************************************************
*+  AGI_1FINIT - Initialise a free list.

      SUBROUTINE AGI_1FINIT ( FRELEN, FRELIS, NEXFRE )

*    Description :
*     Initialise a free list so that NEXFRE and FRELIS indicate the
*     sequence of elements from 1 to FRELEN. The free list is terminated
*     with a -1 to indicate no more free elements. The initialisation is
*     as follows
*              --------------          -----------------
*              | NEXFRE | 1 |          | FRELIS(1) | 2 |
*              --------------          -----------------
*                                      | FRELIS(2) | 3 |
*                                      -----------------
*                                      .               .
*                                      ------------------
*                                      | FRELIS(N) | -1 |
*                                      ------------------
*
*    Invocation :
*     CALL AGI_1FINIT( FRELEN, FRELIS, NEXFRE )
*
*    Method :
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Aug 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Import :

*     Length of the array containing the free list.
      INTEGER FRELEN

*    Export :

*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE

*    Local Constants :
      INTEGER I
*-

*   Make NEXFRE point to first member of free list
      NEXFRE = 1

*   Make sequence in free list array. Terminate with -1.
      DO I = 1, FRELEN - 1
         FRELIS( I ) = I + 1
      ENDDO
      FRELIS( FRELEN ) = -1

      END

