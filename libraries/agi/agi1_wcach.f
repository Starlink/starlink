************************************************************************
*+  AGI_1WCACH - Write the contents of a picture into the cache

      SUBROUTINE AGI_1WCACH( WKNAME, PICNUM, PNAME, COMENT, DEVICE, NDC,
     :                       WORLD, MEMID, POINT, STATUS )

*    Description :
*     Write the contents of a picture into the dynamic cache
*
*    Invocation :
*     CALL AGI_1WCACH( WKNAME, PICNUM, PNAME, COMENT, DEVICE, NDC,
*    :                 WORLD, MEMID, POINT, STATUS )
*
*    Method :
*     Check status on entry.
*     Update the cache pointer.
*     Delete any compiled transformations at the new pointer.
*     Write the picture contents into the cache.
*     Indicate that the transformations have not been compiled.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*     June 1989  Amended to allow for the extra FIFO's
*     Sept 1989  Annul compiled transforms in the cache
*     June 1990  Added MEMID parameter
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Import :

*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Picture number
      INTEGER PICNUM

*     Picture name
      CHARACTER * ( * ) PNAME

*     Picture description
      CHARACTER * ( * ) COMENT

*     Device coordinates of picture
      REAL DEVICE( 4 )

*     Normalised device coordinates of picture
      REAL NDC( 4 )

*     World coordinates of picture
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID

*    Export :

*     Cache pointer to current fifo entry
      INTEGER POINT

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_cache'

*    Local variables :
      INTEGER I, J, K

      CHARACTER * ( AGI__SZNAM ) TPNAME
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Obtain the FIFO number by hashing the picture number
         K = MOD( PICNUM, NFIFO )

*   Update the cache pointer
         J = MOD( PFIFO( K ) + 1, FIFLEN )
         PFIFO( K ) = J

*   Delete any compiled transformations
         IF ( CTRFOR( J, K ) .NE. 0 ) THEN
            CALL TRN_ANNUL( CTRFOR( J, K ), STATUS )
         ENDIF
         IF ( CTRINV( J, K ) .NE. 0 ) THEN
            CALL TRN_ANNUL( CTRINV( J, K ), STATUS )
         ENDIF

*   Convert the name to uppercase
         TPNAME = PNAME
         CALL CHR_UCASE( TPNAME )

*   Write the new picture into the cache
         FIFO( J, K ) = PICNUM
         CWKNAM( J, K ) = WKNAME
         CPNAME( J, K ) = TPNAME
         CCOM( J, K ) = COMENT
         CMEMID( J, K ) = MEMID

         DO I = 1, 4
            CDEV( I, J, K ) = DEVICE( I )
            CNDC( I, J, K ) = NDC( I )
            CWORLD( I, J, K ) = WORLD( I )
         ENDDO

*   Indicate that the transformations have not been compiled
         CTRFOR( J, K ) = 0
         CTRINV( J, K ) = 0

*   Return the current cache pointer
         POINT = J

      ENDIF

      END

