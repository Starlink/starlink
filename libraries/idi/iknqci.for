*-----------------------------------------------------------------------
*+  IKNQCI - Query Capabilities Integer

      SUBROUTINE IKNQCI ( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDQCI.
*     The arguments are identical to those in IIDQCI.
*
*    Invocation :
*     CALL IKNQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*    Method :
*     Verify the input arguments and obtain the requested capability
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
*     December 1990  Changed name from IIDQCI
*     October  1991  Added device type as a capability (#7)
*     June     1992  Added number of available LUT colours (#18)
*     October  1992  Trap any undefined memory visibilities (#20)
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

*     Capability
      INTEGER CAPID

*     Size of output array
      INTEGER NARR

*    Export :
*     Output array
      INTEGER OUTARR( NARR )

*     Number of values returned
      INTEGER NOUT

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'

*    Local variables :
      INTEGER J
*-

*   Recover the characterisitics if the given device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 9999
         ENDIF
      ENDIF

*   Check that CAPID is within allowed limits
      IF ( ( CAPID .LT. 1 ) .OR. ( CAPID .GT. 103 ) ) THEN
         GOTO 9998
      ENDIF

*   Branch depending on the value of CAPID
      GOTO (      1001,9998,9998,9998,9998,9998,1007,9998,9998,
     :       1010,1011,1012,1013,1014,1015,1016,1017,1018,9998,
     :       1020,1021,1022,1023,1024,1025,9998,9998,9998,9998,
     :       1030,1031,1032,1033,1034,1035,1036,9998,9998,9998,
     :       1040,1041,1042,1043,1044,1045,1046,9998,9998,9998,
     :       1050,1051,1052,1053,1054,1055,9998,9998,9998,9998,
     :       1060,1061,1062,1063,1064,9998,9998,9998,9998,9998,
     :       1070,9998,9998,9998,9998,9998,9998,1077,1078,1079,
     :       1080,1081,1082,1083,1084,9998,9998,9998,9998,9998,
     :       1090,1091,9998,9998,9998,9998,9998,9998,9998,9998,
     :       1100,1101,1102,1103 ) CAPID

*   Implementation level
 1001 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CIMPLE
      GOTO 9999

*   Device type ( Ikon = 3200 )
 1007 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = 3200
      GOTO 9999

*   Number of available configurations
 1010 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CONNUM
      GOTO 9999

*   Number of available configurations
 1011 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CONFIG
      GOTO 9999

*   Physical size of display
 1012 CONTINUE
      NOUT = MIN( 2, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CNPIX( J )
      ENDDO
      GOTO 9999

*   Display depth ( number of bits in DAC )
 1013 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CDISDE
      GOTO 9999

*   Maximum depth of VLUT's
 1014 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CLUTDE
      GOTO 9999

*   Number of VLUT's
 1015 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNLUT
      GOTO 9999

*   Number of ITT's per image memory
 1016 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNITT
      GOTO 9999

*   Zoom range
 1017 CONTINUE
      NOUT = MIN( 2, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CZOOMR( J )
      ENDDO
      GOTO 9999

*   Number of available VLUT colours
 1018 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = 2 ** CLUTDE
      GOTO 9999

*   Memory visible
 1020 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         IF ( CMEMVI( J ) .LT. 1 ) THEN
            OUTARR( J + 1 ) = 0
         ELSE
            OUTARR( J + 1 ) = 1
         ENDIF
      ENDDO
      GOTO 9999

*   List of memories in currently selected configuration
 1021 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CONMEM( J )
      ENDDO
      GOTO 9999

*   Depth of memories ( in bits )
 1022 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CMEMDE( J )
      ENDDO
      GOTO 9999

*   List of current VLUT bindings
 1023 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CLUTBI( J )
      ENDDO
      GOTO 9999

*   List of current display path bindings
 1024 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CDISBI( J )
      ENDDO
      GOTO 9999

*   List of current ITT bindings
 1025 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CITTBI( J )
      ENDDO
      GOTO 9999

*   Maximum dimensions of transfer window in x
 1030 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWDIX( J )
      ENDDO
      GOTO 9999

*   Maximum dimensions of transfer window in y
 1031 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWDIY( J )
      ENDDO
      GOTO 9999

*   Transfer window x sizes
 1032 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWSIX( J )
      ENDDO
      GOTO 9999

*   Transfer window y sizes
 1033 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWSIY( J )
      ENDDO
      GOTO 9999

*   Transfer window x offsets
 1034 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWOFX( J )
      ENDDO
      GOTO 9999

*   Transfer window y offsets
 1035 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWOFY( J )
      ENDDO
      GOTO 9999

*   Depth of transfer window in bits
 1036 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CTWDE( J )
      ENDDO
      GOTO 9999

*   Number of available device cursors
 1040 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CURN
      GOTO 9999

*   Array of cursor shapes
 1041 CONTINUE
      NOUT = MIN( CURN, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CURASH( J )
      ENDDO
      GOTO 9999

*   Number of cursor shapes
 1042 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CURNSH
      GOTO 9999

*   List of current cursor bindings
 1043 CONTINUE
      NOUT = MIN( CURN, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CURBI( J )
      ENDDO
      GOTO 9999

*   List of current cursor shapes
 1044 CONTINUE
      NOUT = MIN( CURN, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CURSHA( J )
      ENDDO
      GOTO 9999

*   List of current cursor colours
 1045 CONTINUE
      NOUT = MIN( CURN, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CURCOL( J )
      ENDDO
      GOTO 9999

*   List of current cursor visibilities
 1046 CONTINUE
      NOUT = MIN( CURN, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CURVIS( J )
      ENDDO
      GOTO 9999

*   Number of locators
 1050 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNLOC
      GOTO 9999

*   Number of real evaluators
 1051 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNREVA
      GOTO 9999

*   Number of integer evaluators
 1052 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNIEVA
      GOTO 9999

*   Number of logical evaluators ( switches )
 1053 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNLEVA
      GOTO 9999

*   Number of character evaluators
 1054 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNCEVA
      GOTO 9999

*   Number of triggers
 1055 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNTRIG
      GOTO 9999

*   ROI implemented ( logical )
 1060 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANROI
      GOTO 9999

*   Number of device ROI's
 1061 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CNROI
      GOTO 9999

*   List of current ROI bindings
 1062 CONTINUE
      NOUT = MIN( CNROI, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CROIBI( J )
      ENDDO
      GOTO 9999

*   List of current ROI marker colours
 1063 CONTINUE
      NOUT = MIN( CNROI, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CROICO( J )
      ENDDO
      GOTO 9999

*   List of current ROI visibilities
 1064 CONTINUE
      NOUT = MIN( CNROI, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CROIVI( J )
      ENDDO
      GOTO 9999

*   Blink implemented
 1070 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANBLI
      GOTO 9999

*   Trigger number to increase blink speed
 1077 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CBLINS
      GOTO 9999

*   Trigger number to decrease blink speed
 1078 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CBLDES
      GOTO 9999

*   Trigger number to stop blink
 1079 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CBLSTP
      GOTO 9999

*   Split screen implemented ( logical )
 1080 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANSPL
      GOTO 9999

*   Split screen x memory offsets
 1081 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CSPXOF( J )
      ENDDO
      GOTO 9999

*   Split screen y memory offsets
 1082 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CSPYOF( J )
      ENDDO
      GOTO 9999

*   Split screen x, y address
 1083 CONTINUE
      NOUT = MIN( 2, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CSPLXY( J )
      ENDDO
      GOTO 9999

*   Split screen enabled ( logical )
 1084 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CSPLON
      GOTO 9999

*   Intensity bar implemented ( logical )
 1090 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANINT
      GOTO 9999

*   Intensity bar visible
 1091 CONTINUE
      NOUT = MIN( CNMEM, NARR )
      DO J = 0, NOUT - 1
         OUTARR( J + 1 ) = CINTVI( J )
      ENDDO
      GOTO 9999

*   Snapshot implemented ( logical )
 1100 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANSNA
      GOTO 9999

*   Escape function implemented ( logical )
 1101 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANESC
      GOTO 9999

*   Diagnostic routine implemented ( logical )
 1102 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANDIA
      GOTO 9999

*   Dynamic configuration implemented ( logical )
 1103 CONTINUE
      NOUT = 1
      OUTARR( 1 ) = CANDYN
      GOTO 9999

*   No capability of that number
 9998 CONTINUE
      STATUS = IDI__NOCAP

*   Normal exit
 9999 CONTINUE

      END

