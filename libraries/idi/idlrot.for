*-----------------------------------------------------------------------
*+  IDLROT - Rotate look-up table

      SUBROUTINE IDLROT ( VLUT, START, NENT, SHIFT, STATUS )

*    Description :
*     This rotates the elements of the array VLUT by the amount given
*     by the SHIFT argument. The VLUT array is dimensioned ( 3, NENT )
*     and the data is shifted in sets of three, so the data accessed
*     by the first index is unchanged. The shift only occurs for those
*     elements between SHIFT and NENT of the array. Elements with an
*     second index less than SHIFT are unchenged.
*
*    Invocation :
*     CALL IDLROT( VLUT, START, NENT, SHIFT, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Performing the rotation of the array 'in place' is a non-trivial
*     task. This method uses the NAG routine M01ZCF to decompose the
*     permutation ( given by the rank array ) into cycles. These cycles
*     are then used to reorder the data. The method is described in
*     more detail in the NAG manual on M01ZCF.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     May 1989
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Look-up table
      REAL VLUT( * )

*     Start position
      INTEGER START

*     Number of entries
      INTEGER NENT

*     Amount of shift of LUT
      INTEGER SHIFT

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER I, ICYCL( MAXCOL ), IFAIL, IRANK( MAXCOL ), J, K, L, NUM

      REAL TEMP( 3 )
*-

*   Check the value of start is valid
      IF ( ( START .LT. 0 ) .OR. ( START .GE. NENT ) ) THEN
         START = 0
      ENDIF

*   Ensure the shift is less than the number of entries
      NUM = NENT - START
      SHIFT = MOD( SHIFT, NUM )

*   Rotate the look-up table
*   Create a ranking list for the new order
      IF ( SHIFT .GE. 0 ) THEN
         I = SHIFT + 1
      ELSE
         I = NUM + SHIFT + 1
      ENDIF
      DO J = 1, START
         IRANK( J ) = J
      ENDDO
      DO J = 1 + START, NENT
         IF ( I .EQ. 0 ) THEN
            I = NUM
         ENDIF
         IRANK( J ) = I + START
         I = MOD( I + 1, NUM )
      ENDDO

*   Use the NAG routine M01ZCF to decompose the permutation into cycles
      IFAIL = 1
      CALL M01ZCF( IRANK, 1, NENT, ICYCL, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         STATUS = IDI__ERROR
         GOTO 99
      ENDIF

*   Reorder the data array according to these cycles
      DO K = 1, NENT
         I = ICYCL( K )
         IF ( I .LT. 0 ) THEN
            J = -I
         ELSE

*   Swap rows I and J
            DO L = 1, 3
               TEMP( L ) = VLUT( ( I - 1 ) * 3 + L )
               VLUT( ( I - 1 ) * 3 + L ) = VLUT( ( J - 1 ) * 3 + L )
               VLUT( ( J - 1 ) * 3 + L ) = TEMP( L )
            ENDDO
         ENDIF
      ENDDO

  99  CONTINUE

      END

