*-----------------------------------------------------------------------
*+  IDIPIB - Pack in Bytes

      SUBROUTINE IDIPIB ( BUFIN, NPIX, PACK, DEPTH, OFFSET, BUFOUT,
     :                    NBYTE, STATUS )

*    Description :
*     This converts the input buffer, given in integer longwords into
*     an output buffer containing only bytes. The bytes are extracted
*     from the integers according to the pack argument ( see below ).
*     The depth argument allows for bits to be masked out, to prevent
*     the possibility of unwanted being propogated.
*
*    Invocation :
*     CALL IDIPIB( BUFIN, NPIX, PACK, DEPTH, OFFSET, BUFOUT,
*    :             NBYTE, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     PACK = 1 - Send first byte of longword
*
*       BYTE 4   BYTE 3   BYTE 2   BYTE 1
*     -------------------------------------
*     |\\\\\\\\|\\\\\\\\|\\\\\\\\|        |
*     -------------------------------------
*
*     PACK = 2 - Send first and third bytes of longword
*
*       BYTE 4   BYTE 3   BYTE 2   BYTE 1
*     -------------------------------------
*     |\\\\\\\\|        |\\\\\\\\|        |
*     -------------------------------------
*
*     PACK = 4 - Send all 4 bytes of longword
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
*     Array of integers
      INTEGER BUFIN( * )

*     Number of data pixels
      INTEGER NPIX

*     Number of pixels per longword
      INTEGER PACK

*     Data depth. Bits per pixel.
      INTEGER DEPTH

*    Import-Export :
*     Number of excess pixels in a longword
*     On INPUT the start address of BUFIN has to account for any odd pixels
      INTEGER OFFSET

*    Export :
*     Array of bytes
      BYTE BUFOUT( * )

*     Number of bytes in array
      INTEGER NBYTE

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER J, J1, J2, NLEFT, STEPS

      INTEGER B32
      BYTE B8( 4 ), IDBAND, LMASK, MASK( 7 )
      EQUIVALENCE ( B32, B8( 1 ) )

*    Local data :
      DATA MASK / '01'X, '03'X, '07'X, '0F'X, '1F'X, '3F'X, '7F'X /
      SAVE MASK
*-

*   If the packing factor is 1 then just copy the input to the output
      IF ( PACK .EQ. 1 ) THEN
         STEPS = NPIX
         DO J = 1, STEPS
            B32 = BUFIN( J )
            BUFOUT( J ) = B8( 1 )
         ENDDO
         NBYTE = NPIX
         OFFSET = 0

*   If the packing factor is 2 then
      ELSEIF ( PACK .EQ. 2 ) THEN

*   If OFFSET is 1 then there is an odd pixel to send in the first word
         IF ( OFFSET .EQ. 1 ) THEN

*   Extract the odd pixel
            B32 = BUFIN( 1 )
            BUFOUT( 1 ) = B8( 3 )
            NBYTE = 1

*   Calculate the number of complete words to send
            STEPS = ( NPIX - OFFSET ) / 2
            J1 = 2
            J2 = STEPS + 1

         ELSE
            NBYTE = 0
            STEPS = NPIX / 2
            J1 = 1
            J2 = STEPS
         ENDIF

*   Extract the main bulk of the pixels
         DO J = J1, J2
            B32 = BUFIN( J )
            BUFOUT( NBYTE + 1 ) = B8( 1 )
            BUFOUT( NBYTE + 2 ) = B8( 3 )
            NBYTE = NBYTE + 2
         ENDDO

*   See if there is an odd pixel to send
         NLEFT = NPIX - NBYTE
         IF ( NLEFT .EQ. 1 ) THEN
            B32 = BUFIN( J2 + 1 )
            BUFOUT( NBYTE + 1 ) = B8( 1 )
            NBYTE = NBYTE + 1

*   Return the fact that an odd pixel has been extracted
            OFFSET = 1
         ELSE
            OFFSET = 0
         ENDIF

*   If the packing factor is 4 then
      ELSEIF ( PACK .EQ. 4 ) THEN

*   If OFFSET is > 1 then there are odd pixels to send in the first word
         IF ( ( OFFSET .GT. 0 ) .AND. ( OFFSET .LT. 4 ) ) THEN

*   EXtract the odd pixels
            B32 = BUFIN( 1 )
            DO J = 1, OFFSET
               BUFOUT( J ) = B8( 4 - OFFSET + J )
            ENDDO
            NBYTE = OFFSET

*   Calculate the number of complete words to send
            STEPS = ( NPIX - OFFSET ) / 4
            J1 = 2
            J2 = STEPS + 1

         ELSE
            NBYTE = 0
            STEPS = NPIX / 4
            J1 = 1
            J2 = STEPS
         ENDIF

*   Extract the main bulk of the pixels
         DO J = J1, J2
            B32 = BUFIN( J )
            BUFOUT( NBYTE + 1 ) = B8( 1 )
            BUFOUT( NBYTE + 2 ) = B8( 2 )
            BUFOUT( NBYTE + 3 ) = B8( 3 )
            BUFOUT( NBYTE + 4 ) = B8( 4 )
            NBYTE = NBYTE + 4
         ENDDO

*   See if there are any odd pixels to send
         NLEFT = NPIX - NBYTE
         IF ( ( NLEFT .GT. 0 ) .AND. ( NLEFT .LT. 4 ) ) THEN
            B32 = BUFIN( J2 + 1 )
            DO J = 1, NLEFT
               BUFOUT( NBYTE + J ) = B8( J )
            ENDDO
            NBYTE = NBYTE + NLEFT

*   Return the fact that some odd pixels have been extracted
            OFFSET = 4 - NLEFT
         ELSE
            OFFSET = 0
         ENDIF

*   If the packing factor is anything else then return an error
      ELSE
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   If DEPTH > 8 then the data will have been truncated already.
*   If DEPTH < 8 then have to replace the more significant bits with zero
      IF ( ( DEPTH .LT. 8 ) .AND. ( DEPTH .GT. 0 ) ) THEN
         LMASK = MASK( DEPTH )

*   Do a bitwise AND on the byte data with the depth mask
         DO J = 1, NBYTE
            BUFOUT( J ) = IDBAND( BUFOUT( J ), LMASK )
         ENDDO
      ENDIF

  99  CONTINUE

      END

