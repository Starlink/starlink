*-----------------------------------------------------------------------
*+  IDIPIL - Pack in Longwords

      SUBROUTINE IDIPIL ( BUFIN, NBYTE, PACK, DEPTH, OFFSET, BUFOUT,
     :                    NLWORD, STATUS )

*    Description :
*     This converts the input buffer, given in bytes into an output
*     buffer containing integer longwords. The bytes are inserted
*     into the integers according to the pack argument ( see below ).
*     The depth argument allows for bits to be masked out, to prevent
*     the possibility of unwanted bits being propogated.
*
*    Invocation :
*     CALL IDIPIL( BUFIN, NBYTE, PACK, DEPTH, OFFSET, BUFOUT,
*    :             NLWORD, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     PACK = 1 - Fill first byte of longword
*
*       BYTE 4   BYTE 3   BYTE 2   BYTE 1
*     -------------------------------------
*     |\\\\\\\\|\\\\\\\\|\\\\\\\\|        |
*     -------------------------------------
*
*     PACK = 2 - Fill first and third bytes of longword
*
*       BYTE 4   BYTE 3   BYTE 2   BYTE 1
*     -------------------------------------
*     |\\\\\\\\|        |\\\\\\\\|        |
*     -------------------------------------
*
*     PACK = 4 - Fill all 4 bytes of longword
*
*       BYTE 4   BYTE 3   BYTE 2   BYTE 1
*     -------------------------------------
*     |        |        |        |        |
*     -------------------------------------
*
*    Deficiencies :
*     Very non-standard Fortran.
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Array of bytes
      BYTE BUFIN( * )

*     Number of bytes
      INTEGER NBYTE

*     Packing factor
      INTEGER PACK

*     Data depth. Bits per pixel.
      INTEGER DEPTH

*    Import-Export
*     Number of excess pixels in a longword
      INTEGER OFFSET

*    Export :
*     Array of integers
      INTEGER BUFOUT( * )

*     Number of long words in array
      INTEGER NLWORD

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER MASK( 7 )

*    Local variables :
      INTEGER COUNT, IMASK, J, J1, J2, NLEFT, STEPS

      INTEGER B32
      BYTE B8( 4 )
      EQUIVALENCE( B32, B8( 1 ) )

*    Local data :
      DATA MASK / '01'X, '03'X, '07'X, '0F'X, '1F'X, '3F'X, '7F'X /
      SAVE MASK
*-

*   If the packing factor is 1 then just copy the input to the output
      IF ( PACK .EQ. 1 ) THEN
         STEPS = NBYTE
         DO J = 1, STEPS
            B32 = 0
            B8( 1 ) = BUFIN( J )
            BUFOUT( J ) = B32
         ENDDO
         NLWORD = STEPS
         OFFSET = 0

*   If the packing factor is 2 then
      ELSEIF ( PACK .EQ. 2 ) THEN

*   If OFFSET is 1 then there is an odd pixel to put in the first word
         IF ( OFFSET .EQ. 1 ) THEN

*   Allow for an existing byte in the first longword
            B32 = BUFOUT( 1 )
            B8( 3 ) = BUFIN( 1 )
            B8( 4 ) = 0
            BUFOUT( 1 ) = B32
            COUNT = 1

*   Calculate the number of complete words to send
            STEPS = ( NBYTE - OFFSET ) / 2
            J1 = 2
            J2 = STEPS + 1

         ELSE
            COUNT = 0
            STEPS = NBYTE / 2
            J1 = 1
            J2 = STEPS
         ENDIF

*   Extract the main bulk of the pixels
         COUNT = 0
         DO J = J1, J2
            B32 = 0
            B8( 1 ) = BUFIN( COUNT + 1 )
            B8( 3 ) = BUFIN( COUNT + 2 )
            COUNT = COUNT + 2
            BUFOUT( J ) = B32
         ENDDO
         NLWORD = J2

*   See if there is an odd pixel to send
         NLEFT = NBYTE - COUNT
         IF ( NLEFT .EQ. 1 ) THEN
            COUNT = COUNT + 1
            B32 = 0
            B8( 1 ) = BUFIN( COUNT )
            NLWORD = NLWORD + 1
            BUFOUT( NLWORD ) = B32

*   Return the fact that an odd pixel has been extracted
            OFFSET = 1
         ELSE
            OFFSET = 0
         ENDIF

*   If the packing factor is 4 then
      ELSEIF ( PACK .EQ. 4 ) THEN
         COUNT = 0

*   If OFFSET is > 1 then there are odd pixels to put in the first word
         IF ( ( OFFSET .GT. 0 ) .AND. ( OFFSET .LT. 4 ) ) THEN

*   Allow for existing bytes in the first longword
            B32 = BUFOUT( 1 )
            DO J = 5 - OFFSET, 4
               COUNT = COUNT + 1
               B8( J ) = BUFIN( COUNT )
            ENDDO
            BUFOUT( 1 ) = B32

*   Calculate the number of complete words to send
            STEPS = ( NBYTE - OFFSET ) / 4
            J1 = 2
            J2 = STEPS + 1

         ELSE
            STEPS = NBYTE / 4
            J1 = 1
            J2 = STEPS
         ENDIF

*   Extract the main bulk of the pixels
         DO J = J1, J2
            B8( 1 ) = BUFIN( COUNT + 1 )
            B8( 2 ) = BUFIN( COUNT + 2 )
            B8( 3 ) = BUFIN( COUNT + 3 )
            B8( 4 ) = BUFIN( COUNT + 4 )
            COUNT = COUNT + 4
            BUFOUT( J ) = B32
         ENDDO
         NLWORD = J2

*   See if there are any odd pixels to send
         NLEFT = NBYTE - COUNT
         IF ( ( NLEFT .GT. 0 ) .AND. ( NLEFT .LT. 4 ) ) THEN
            B32 = 0
            DO J = 1, NLEFT
               B8( J ) = BUFIN( COUNT + J )
            ENDDO
            COUNT = COUNT + NLEFT
            NLWORD = NLWORD + 1
            BUFOUT( NLWORD ) = B32

*   Return the fact that an odd pixel has been extracted
            OFFSET = 4 - NLEFT
         ELSE
            OFFSET = 0
         ENDIF

*   If the packing factor is anything else then return an error
      ELSE
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   If DEPTH > 8 then the data will be extended with zeroes anyway
*   If DEPTH < 8 then have to replace the more significant bits with zero
      IF ( ( DEPTH .LT. 8 ) .AND. ( DEPTH .GT. 0 ) ) THEN
         IF ( PACK .EQ. 1 ) THEN
            IMASK = MASK( DEPTH )
         ELSEIF ( PACK .EQ. 2 ) THEN
            IMASK = MASK( DEPTH ) + MASK( DEPTH ) * 256 * 256
         ELSEIF ( PACK .EQ. 4 ) THEN
            IMASK = MASK( DEPTH ) + MASK( DEPTH ) * 256 +
     :              MASK( DEPTH ) * 256 * 256 +
     :              MASK( DEPTH ) * 256 * 256 * 256
         ENDIF

*   Do a bitwise AND on the data with the depth mask
         DO J = 1, NLWORD
            BUFOUT( J ) = IAND( BUFOUT( J ), IMASK )
         ENDDO
      ENDIF

  99  CONTINUE

      END

