      SUBROUTINE RAW( TP, APHOT, AQUAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE RAW
*
*   Description:
*      Read RAW IUE image from VICAR tape file into memory.
*
*   History:
*      Jack Giddings     01-AUG-82     AT4 version
*      Paul Rees         03-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton    29-SEP-94     IUEDR Vn. 3.1-6
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Constants:
      INTEGER MAXL                  ! maximum number of IUE lines
      INTEGER MAXS                  ! maximum number of IUE scans
      PARAMETER (MAXL = 768, MAXS = 768)

*   Import:
      INTEGER TP                    ! tape descriptor

*   Export:
      INTEGER*2 APHOT(MAXS, MAXL)   ! photometric image array

      BYTE AQUAL(MAXS, MAXL)        ! data quality array

      INTEGER STATUS                ! status return

*   Local variables:
      INTEGER  RAW_VM               ! address of RAW data

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get some VM for RAW image
      CALL ALADR('byte\\', MAXS*MAXL, RAW_VM, STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: getting VM for RAW image\\',
     :                STATUS )
         RETURN
      END IF

*   Read VICAR image
      CALL VIC_RDAT( TP, 1, MAXS, MAXL, %VAL(RAW_VM), STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL RAWP( MAXS, MAXL, %VAL(RAW_VM), APHOT )
         CALL DQ_ZERO( MAXS * MAXL, AQUAL )
      END IF

*   Free VM
      CALL DLADR( RAW_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: freeing VM for RAW image\\',
     :                STATUS )
         RETURN
      END IF
      END
