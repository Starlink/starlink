*+  TIM_PUTOUT - Produces output file
      SUBROUTINE TIM_PUTOUT( PARAM, TYPE, NFREQ, POWER, BASE, SCALE,
     :                                                OFID, STATUS )
*    Description :
*      Produces an output file, usually either a periodogram or phase file
*    Environment parameters :
*      PARAM     UNIV       Name of output file
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*) PARAM                  ! Output file name
      CHARACTER*(*) TYPE                   ! Output file type
      INTEGER NFREQ                        ! Number of output bins
      REAL POWER(*)                        ! Power from time series application
      REAL BASE,SCALE                      ! Start and width values for
*                                          ! output axis
*    Export :
*                                          ! output axis
      INTEGER			OFID			! Output dataset id
*                                          ! output axis
*    Status :
*                                          ! output axis
      INTEGER STATUS

*  Local Variables:
      INTEGER			ODPTR			! Output data ptr
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open output file
      CALL USI_TASSOCO(PARAM, TYPE, OFID, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Create standard components in output file
      CALL BDA_CREBDS(OFID, 1, NFREQ, .TRUE., .FALSE., .FALSE., STATUS)

*    Write data array into output file
      CALL BDI_CREDATA( OFID, 1, NFREQ, STATUS )
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )
      CALL ARR_COP1R( NFREQ, POWER, %VAL(ODPTR), STATUS )
      CALL BDI_UNMAPDATA( OFID, STATUS )

*    Create axis in output file
      CALL BDI_PUTAXVAL(OFID, 1, BASE, SCALE, NFREQ, STATUS)

*    Add history record

*    Abort point
  99  IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'TIM_PUTOUT', STATUS )
      END IF

      END
