************************************************************************
*+  AGI_1FNEXT - Get next member from free list

      SUBROUTINE AGI_1FNEXT ( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )

*    Description :
*     Get the next member from the free list. If there are no more
*     free then return an error in STATUS. The next free element in
*     the free list is copied from NEXFRE into FREEID and NEXFRE is
*     updated by copying the contents of the element of FRELIS pointed
*     to by NEXFRE. For example if the contents were
*                --------------          -----------------
*                | NEXFRE | 6 |          | FRELIS(3) | 9 |
*                --------------          -----------------
*                                        .               .
*                                        -----------------
*                                        | FRELIS(6) | 3 |
*                                        -----------------
*     Then the result will be
*     --------------          --------------          -----------------
*     | FREEID | 6 |          | NEXFRE | 3 |          | FRELIS(3) | 9 |
*     --------------          --------------          -----------------
*                                                     .               .
*                                                     ------------------
*                                                     | FRELIS(6) | -2 |
*                                                     ------------------
*
*    Invocation :
*     CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, FREEID, STATUS )
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

*    Import-Export :

*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE

*    Export :

*     Indicator to free member
      INTEGER FREEID

*    Status :
      INTEGER STATUS
*-

*   If there are no more free members then return an error status
      IF ( NEXFRE .LT. 0 ) THEN
         STATUS = 1

*   Else get the next free member
      ELSE
         FREEID = NEXFRE
         NEXFRE = FRELIS( NEXFRE )

*   Put a -2 into the old place. This is not strictly neccessary for
*   correct operation, but it allows the release routine to check that
*   the item has been previously given away.
         FRELIS( FREEID ) = -2

      ENDIF

      END

