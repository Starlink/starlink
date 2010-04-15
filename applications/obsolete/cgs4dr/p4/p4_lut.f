*+  P4_LUT - Read lookup table from external file
       SUBROUTINE P4_LUT( STATUS )
*    Description :
*     Subroutine to read a lookup table from an external file.
*    Invocation :
*     CALL P4_LUT( STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     P. N. Daly  (JACH::PND)
*     K. Krisciunas (JACH::KEVIN)
*    History :
*          1989 ?: Original version.                              (JFL)
*     24-Oct-1989: History added. Extra status checks.            (SMB)
*     25-Oct-1989: Setting of status to ACT_END removed, so that
*                  P4_ENDACTION may be used.                      (SMB)
*      7-May-1990: DYN dynamic memory functions replaced by %val,
*                  so the code can be compiled with array bounds
*                  checking. MAXDIM parameter added.              (SMB)
*      1-Jun-1990: Colour tables are now assumed to be in the
*                  directory CGS4_COLOUR_TABLES.                  (SMB)
*     28-Aug-1990: Code spaced out, and more comments and error
*                  checks added.                                  (SMB)
*     18-Feb-1993: Tidy code                                      (PND)
*     07-Apr-1993: version for P4 task using PGPIXL for image plots  (KLK)
*     21-Apr-1993: do the PGSCR calls here   (KLK)
*     20-Aug-1993: reset the fg/bg colours (PND)
*      3-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'                         ! P4 common block
      INCLUDE 'COLOURS.INC'
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER CLEN, CPOS1, CPOS2
      INTEGER PORT
      INTEGER NDIM
      INTEGER DIMS( 2 )
      INTEGER NELM
      INTEGER CT_PTR
      INTEGER CT_SLT
      INTEGER I, J
      REAL RATIO, AMT, TEMP, DIFF
*-

*    Abort if there is a bad status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get port number and read noticeboard
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )

*    Check that the port has an open device
      CALL P4_CHECK_PORT( PORT, STATUS )

*    Check that the colour table has been specified correctly
      CPOS1 = INDEX( DEVICE_LUT(PORT), 'P4_CT' )
      CPOS2 = INDEX( DEVICE_LUT(PORT), SEPARATOR )
      IF ( CPOS1.EQ.0 .AND. CPOS2.EQ.0 ) THEN
        CLEN = CHR_LEN( DEVICE_LUT(PORT) )
        DEVICE_LUT(PORT) = PREFIX // 'P4_CT' /
     :     / SEPARATOR // DEVICE_LUT(PORT)(1:CLEN)
      ENDIF

*    Open the specified colour table file
      CALL DSA_OPEN( STATUS )
      CALL P4_CHECK_INPUT( DEVICE_LUT( PORT ),  STATUS )
      CALL DSA_NAMED_INPUT ('TABLE', DEVICE_LUT( PORT ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 500

*    Determine the size of the data array in the colour table
      CALL DSA_DATA_SIZE( 'TABLE', 2, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 500
      IF ( DIMS( 1 ) .NE. NGUNS ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'NGUNS', NGUNS )
        CALL MSG_SETI( 'DIMS1', DIMS( 1 ) )
        CALL ERR_REP( ' ', 'P4_LUT: '/
     :    /'Data has ^DIMS1 colour guns instead of ^NGUNS', STATUS )
      ELSE IF ( DIMS( 2 ) .NE. NLEVELS ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'NLEVS', NLEVELS )
        CALL MSG_SETI( 'DIMS2', DIMS( 2 ) )
        CALL ERR_REP( ' ', 'P4_LUT: '/
     :    /'Data has ^DIMS2 colour levels instead of ^NLEVS', STATUS )
      ENDIF

*    Map the data array from the colour table.
      CALL DSA_MAP_DATA( 'TABLE', 'READ', 'FLOAT', CT_PTR, CT_SLT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 500

*    Extract the colour table values
      CALL P4_EXTRACT_CT( %val( CT_PTR ), DIMS( 1 ), DIMS( 2 ),
     :  RED, GREEN, BLUE, STATUS )

*    First inquire how many colours there are and set RGB values
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Map the 256 possible colors onto the number available
        RATIO = FLOAT( CI2( PORT) ) / 256.0
        AMT = RATIO / 2.0
        DO I = 1,256

          TEMP = RATIO * I
          J = NINT(TEMP)
          DIFF = ABS(TEMP-J)

          IF ( DIFF .LT. AMT ) THEN
            IF ( J .EQ. 1 ) THEN

              RED( 1 )   = FG_RD
              GREEN( 1 ) = FG_GR
              BLUE( 1 )  = FG_BL
            ENDIF

            CALL PGSCR( J, RED( I ), GREEN( I ), BLUE( I ) )
          ENDIF
        ENDDO
      ENDIF

*    Close DSA and tidy up.
 500  CONTINUE
      CALL DSA_CLOSE( STATUS )

      END
