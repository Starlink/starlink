*-----------------------------------------------------------------------
*+  IKNQDV - Query Device Characteristics

      SUBROUTINE IKNQDV ( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
     :                    NITT, NCURS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDQDV.
*     The arguments are identical to those in IIDQDV.
*
*    Invocation :
*     CALL IKNQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
*    :             NITT, NCURS, STATUS )
*
*    Method :
*     Verify the input arguments and obtain the required information
*     from the common blocks.
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
*     November 1988
*     December 1990  Changed name from IIDQDV
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Export :
*     Number of available configurations
      INTEGER NCONF

*     Display size in x ( pixels )
      INTEGER XSIZE

*     Display size in y ( pixels )
      INTEGER YSIZE

*     Display depth ( number of bits in DAC's )
      INTEGER DEPTH

*     Number of VLUT's in device
      INTEGER NVLUT

*     Number of ITT's per image memory
      INTEGER NITT

*     Number of cursors
      INTEGER NCURS

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
*-

*   If the given display is not the current one then do a recover
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Number of available configurations
      NCONF = CONNUM

*   Display size in x ( pixels )
      XSIZE = CNPIX( 0 )

*   Display size in y ( pixels )
      YSIZE = CNPIX( 1 )

*   Display depth ( number of bits in DAC's )
      DEPTH = CDISDE

*   Number of VLUT's in device
      NVLUT = CNLUT

*   Number of ITT's per image memory
      NITT = CNITT

*   Number of cursors
      NCURS = CURN

  99  CONTINUE

      END

