*-----------------------------------------------------------------------
*+  IDBAND - Perform a bitwise logical AND on two bytes

      BYTE FUNCTION IDBAND( BYTE1, BYTE2 )

*    Description :
*     This function is intended to introduce a level of abstraction for
*     this very non-standard piece of Fortran. The function is
*     equivalent to the VAX intrinsic function IAND, which itself is
*     non-standard Fortran.
*
*    Invocation :
*     <byte_variable> = IDBAND( BYTE1, BYTE2 )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     No comment !!
*
*    Deficiencies :
*     Very non-standard Fortran - uses byte variables.
*                                 uses AND as a non-logical operator
*
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

*    Import :
      BYTE BYTE1, BYTE2
*-

*   Perform the bitwise logical AND
      IDBAND = BYTE1 .AND. BYTE2

      END

