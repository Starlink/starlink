************************************************************************
*+  AGI_1FRETN - Return a member to the free list

      SUBROUTINE AGI_1FRETN ( FRELEN, FREEID, FRELIS, NEXFRE, STATUS )

*    Description :
*     Return a member to the free list. The FRELIS and NEXFRE are
*     updated. If the element is already free then an error status is
*     returned. The FRELIS element pointed to by FREEID is filled with
*     NEXFRE, and then FREEID is copied into NEXFRE. For example if the
*     contents were
*     --------------          --------------          -----------------
*     | FREEID | 6 |          | NEXFRE | 3 |          | FRELIS(3) | 9 |
*     --------------          --------------          -----------------
*                                                     .               .
*                                                     ------------------
*                                                     | FRELIS(6) | -2 |
*                                                     ------------------
*     Then the result will be
*                --------------          -----------------
*                | NEXFRE | 6 |          | FRELIS(3) | 9 |
*                --------------          -----------------
*                                        .               .
*                                        -----------------
*                                        | FRELIS(6) | 3 |
*                                        -----------------
*
*    Invocation :
*     CALL AGI_1FRETN ( FRELEN, FREEID, FRELIS, NEXFRE )
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

*     Indicator to free member
      INTEGER FREEID

*    Import-Export :

*     Array containing the free list
      INTEGER FRELIS( FRELEN )

*     Pointer indicating next free in free list array
      INTEGER NEXFRE

*    Status :
      INTEGER STATUS
*-

*   Check that the given member is already free
      IF ( FRELIS( FREEID ) .LT. -1 ) THEN

*   Put NEXFRE into the location pointed to by FREEID
         FRELIS( FREEID ) = NEXFRE

*   Update NEXFRE
         NEXFRE = FREEID

      ELSE

*   The element is already free
         STATUS = 2

      ENDIF

      END

