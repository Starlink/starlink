*+  P4_RESET - Resets the P4 task to a known state
      SUBROUTINE P4_RESET( STATUS )
*    Invocation :
*     CALL P4_RESET( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      4-Aug-1994: Original version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                        ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'                   ! P4 common block
*    Local variables :
      INTEGER ICOUNT                        ! A counter
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop to close and reset devices
      DO ICOUNT = 0, MAXPORT, 1
        CALL PGEND
        DEVICE_NAME( ICOUNT ) = ' '
        LAST_TYPE( ICOUNT ) = 'UNKNOWN'
        PLOT_OK( ICOUNT ) = .FALSE.
        PORT_OK( ICOUNT ) = .FALSE.
      ENDDO

*    Reset the common block for all ports
      CALL P4_INIT_CB( -1, STATUS )

*    Update the NBS system and annul status to SAI__OK
      CALL P4_WRITE_NB( -1, STATUS )
      CALL ERR_ANNUL( STATUS )

*    Clear port 0
      CALL PGASK( .FALSE. )
      CALL PGPAGE( )

      END
