*-----------------------------------------------------------------------
*+  IDSACO - Save common blocks

      SUBROUTINE IDSACO ( STATUS )

*    Description :
*     Save the contents of the common blocks for the current device in
*     a FORTRAN direct access file IDI_USER:IDI_COMMS.DAT.
*
*    Invocation :
*     CALL IDSACO( STATUS )
*
*    Method :
*     To save the effort of saving each of the common block entries
*     in turn the whole common block is equated to a local array with
*     EQUIVALENCE and this array is then written to the file in one go.
*     The length local array given by the parameter should exactly
*     match the length of the common block.
*     Each device uses 8 records each 256 words long.
*     Record Contents
*     1      Device name
*     2      Characteristcs, Configuration, Interactions, Positions
*     3 - 8  LUTs
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
*     November 1989 Changed from HDS to FORTRAN direct access.
*     April    1990 Check file record lengths
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMINT)'
      INCLUDE 'IDIINC(IKN_COMLUT)'
      INCLUDE 'IDIINC(IKN_COMPOS)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local constants :
*     HDS container file name
      CHARACTER * ( * ) FNAME
      PARAMETER ( FNAME = 'IDI_USER:IDI_COMMS.DAT' )

*     File record length
      INTEGER FRECL
      PARAMETER ( FRECL = 256 )

*     Number of longwords in the common block storage
      INTEGER CHWORD, COWORD, IWORD, LWORD, PWORD
      PARAMETER ( CHWORD = ( 29 * 1 ) + ( 3 * 2 ) + ( 18 * MAXMEM ) +
     :                     ( 5 * MAXCUR ) + ( 3 * MAXROI ) )
      PARAMETER ( COWORD = ( 2 * 1 ) + ( 5 * MAXMEM ) )
      PARAMETER ( IWORD = ( 1 * 1 ) + ( 7 * MAXINT ) )
      PARAMETER ( LWORD = MAXCOL )
      PARAMETER ( PWORD = ( 5 * MAXMEM ) + ( 2 * MAXCUR ) +
     :                    ( 4 * MAXROI ) + 4 )

*    Local variables :
      LOGICAL FOUND

      INTEGER I, ISTAT, NELEMS, SLEN

      CHARACTER * ( DAT__SZNAM ) SCURRN, SNAME

*     Equivalence the common blocks to an array of integers
      INTEGER CHSAVE( CHWORD ), COSAVE( COWORD ), ISAVE( IWORD ),
     :        PSAVE( PWORD )
      REAL LSAVE1( LWORD ), LSAVE2( LWORD ), LSAVE3( LWORD ),
     :     LSAVE4( LWORD ), LSAVE5( LWORD ), LSAVE6( LWORD )
      EQUIVALENCE ( CURRID, CHSAVE )
      EQUIVALENCE ( CONMOD, COSAVE )
      EQUIVALENCE ( CINTN, ISAVE )
      EQUIVALENCE ( CMEMX( 0 ), PSAVE )
      EQUIVALENCE ( CLUT0( 1, 1 ), LSAVE1 )
      EQUIVALENCE ( CLUT0( 2, 86 ), LSAVE2 )
      EQUIVALENCE ( CLUT0( 3, 171 ), LSAVE3 )
      EQUIVALENCE ( CLUT1( 1, 1 ), LSAVE4 )
      EQUIVALENCE ( CLUT1( 2, 86 ), LSAVE5 )
      EQUIVALENCE ( CLUT1( 3, 171 ), LSAVE6 )
*-

*   Check that the number of elements does not exceed the record length
      NELEMS = CHWORD + COWORD + IWORD + PWORD
      IF ( ( NELEMS .GT. FRECL ) .OR. ( LWORD .GT. FRECL ) ) THEN
         STATUS = IDI__ERROR
         GOTO 99
      ENDIF

*   Open the characteristics file.
*   Use a local status value for opening and closing the file
      OPEN( UNIT = 1, FILE = FNAME, ACCESS = 'DIRECT', RECL = FRECL,
     :      STATUS = 'UNKNOWN', IOSTAT = ISTAT )

*   Check the status
      IF ( ISTAT .NE. 0 ) THEN
         STATUS = IDI__ERROR
         GOTO 99
      ENDIF

*   Check that the current device is open
      IF ( ONOFF( CURRID ) .NE. 0 ) THEN

*   Make up a name for the current device
         CALL IDNAME( CURRID, SCURRN, SLEN, STATUS )

*   See if there is a structure for the current device
*   The device names are held every 8th record
         FOUND = .FALSE.
         I = -7
         DO WHILE ( .NOT. FOUND )
            I = I + 8
            READ( 1, REC = I, ERR = 10, IOSTAT = ISTAT ) SNAME
            IF ( SNAME .EQ. SCURRN ) THEN
               FOUND = .TRUE.
            ENDIF
         ENDDO
  10     CONTINUE

*   Create a new structure if it doesn't already exist
         IF ( .NOT. FOUND ) THEN
            WRITE( 1, REC = I, IOSTAT = ISTAT ) SCURRN
         ENDIF

*   In the next record write the charactersitics, configuration,
*   interactions and positions. Overwrite any existing data.
         I = I + 1
         WRITE( 1, REC = I, IOSTAT = ISTAT ) CHSAVE, COSAVE, ISAVE,
     :                                       PSAVE

*   In the next six records write the LUT
         WRITE( 1, REC = I + 1, IOSTAT = ISTAT ) LSAVE1
         WRITE( 1, REC = I + 2, IOSTAT = ISTAT ) LSAVE2
         WRITE( 1, REC = I + 3, IOSTAT = ISTAT ) LSAVE3
         WRITE( 1, REC = I + 4, IOSTAT = ISTAT ) LSAVE4
         WRITE( 1, REC = I + 5, IOSTAT = ISTAT ) LSAVE5
         WRITE( 1, REC = I + 6, IOSTAT = ISTAT ) LSAVE6

      ENDIF

*   Close the characterisitics file using the local status
      CLOSE( 1, IOSTAT = ISTAT )

  99  CONTINUE

      END

