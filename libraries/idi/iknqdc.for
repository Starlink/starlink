*-----------------------------------------------------------------------
*+  IKNQDC - Query Defined Configuration

      SUBROUTINE IKNQDC ( DISPID, NCONF, MEMTYP, NMEMAX, MODCON,
     :                    MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP,
     :                    NMEM, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDQDC.
*     The arguments are identical to those in IIDQDC.
*
*    Invocation :
*     CALL IKNQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON, MEMID,
*    :             MEMSIX, MEMSIY, MEMDEP, ITTDEP, NMEM, STATUS )
*
*    Method :
*     Verify the input arguments and obtain the configuration from
*     the common blocks if it the current one, otherwise read the
*     workstation description file for the data.
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
*     December 1990  Changed name from IIDQDC
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

*     Configuration number
      INTEGER NCONF

*     Memory type
      INTEGER MEMTYP

*     Maximum number of memories
      INTEGER NMEMAX

*    Export :
*     Configuration mode
      INTEGER MODCON

*     List of memory identifiers
      INTEGER MEMID( * )

*     Memory sizes in x
      INTEGER MEMSIX( * )

*     Memory sizes in y
      INTEGER MEMSIY( * )

*     Memory depths
      INTEGER MEMDEP( * )

*     Memory ITT depths
      INTEGER ITTDEP( * )

*     Number of memories
      INTEGER NMEM

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMCH)'

*    Local variables :
      LOGICAL GOTCON

      CHARACTER FNAME * 72, STRING * 64

      INTEGER ISTAT, J, JNUM, K, NREAD
      INTEGER JCNMEM, JITTDE( MAXMEM ), JMEMDE( MAXMEM ),
     :        JMEMID( MAXMEM ), JMEMSX( MAXMEM ), JMEMSY( MAXMEM ),
     :        JMEMTY( MAXMEM )

      REAL RNUM
*-

*   Obtain the correct description file name for the device
      FNAME = 'IDI_DIR:IKNWDT.DAT'

*   Verify the given number of memories
      IF ( NMEMAX .LE. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Verify the configuration number
      IF ( ( NCONF .LT. -1 ) .OR. ( NCONF .GE. CONNUM ) ) THEN
         STATUS = IDI__INCON
         GOTO 99
      ENDIF

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Find out if the requested configuration is the current one
      IF ( ( NCONF .EQ. -1 ) .OR. ( NCONF .EQ. CONFIG ) ) THEN

*   Read the items from the common blocks

*   Configuration mode
         MODCON = CONMOD

*   Loop through all the memories in this configuration until either
*   the input limit has been reached or the memories have run out
         NMEM = 0
         DO J = 0, MIN( NMEMAX - 1, CNMEM - 1 )

*   Check the memory types against the requested one.
*   Do a bitwise AND on the input and stored memory types.
*   The bitwise AND table looks like this for the integers 1 to 7
*   1  0  1  0  1  0  1
*   0  2  2  0  0  2  2     A match occurs when the original number
*   1  2  3  0  1  2  3     equals the ANDed number. For instance
*   0  0  0  4  4  4  4     input=3 and stored=7 gives a match since
*   1  0  1  4  5  4  5     the number 7 contains the bit pattern
*   0  2  2  4  4  6  6     of the number 3
*   1  2  3  4  5  6  7
            IF ( MEMTYP .EQ. IAND( CMEMTY( J ), MEMTYP ) ) THEN
               NMEM = NMEM + 1

*   Memory identifier
               MEMID( NMEM ) = J

*   Memory size in x
               MEMSIX( NMEM ) = CMEMSX( J )

*   Memory size in y
               MEMSIY( NMEM ) = CMEMSY( J )

*   Memory depth
               MEMDEP( NMEM ) = CMEMDE( J )

*   Memory ITT depths
               ITTDEP( NMEM ) = CITTDE( J )

            ENDIF
         ENDDO

*   Otherwise have to read the configuration from permanent storage
      ELSE

*   Configuration mode
         MODCON = CONMOD

*   Open the description file
         OPEN( UNIT = 1, FILE = FNAME, IOSTAT = ISTAT, STATUS = 'OLD',
     :         READONLY )
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = IDI__WDTER
            GOTO 99
         ENDIF

*   Define the possible formats
 9001    FORMAT( A64 )
 9002    FORMAT( A64, I8 )
 9003    FORMAT( A64, F8.3 )

*   Skip over the characteristics
         NREAD = 26 + CURN
         DO K = 1, NREAD
            READ( 1, 9001, ERR = 98 ) STRING
         ENDDO

*   Look for the required configuration
         DO K = 0, CONNUM - 1
            GOTCON = .FALSE.
            READ( 1, 9002, ERR = 97 ) STRING, JNUM
            IF ( K .EQ. NCONF ) THEN
               GOTCON = .TRUE.
            ENDIF

*   Read the configuration into local variables
            READ( 1, 9002, ERR = 97 ) STRING, JCNMEM
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JMEMID( J )
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JMEMDE( J )
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JMEMSX( J )
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JMEMSY( J )
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JITTDE( J )
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9003, ERR = 97 ) STRING, RNUM
            ENDDO
            DO J = 0, JCNMEM - 1
               READ( 1, 9002, ERR = 97 ) STRING, JMEMTY( J )
            ENDDO

*   See if this is the correct configuration
            IF ( GOTCON ) THEN

*   Loop through all the memories in this configuration until either
*   the input limit has been reached or the memories have run out
               NMEM = 0
               DO J = 0, MIN( NMEMAX - 1, JCNMEM - 1 )

*   Check the memory types against the requested one.
*   Do a bitwise AND on the input and stored memory types.
*   The bitwise AND table looks like this for the integers 1 to 7
*   1  0  1  0  1  0  1
*   0  2  2  0  0  2  2     A match occurs when the original number
*   1  2  3  0  1  2  3     equals the ANDed number. For instance
*   0  0  0  4  4  4  4     input=3 and stored=7 gives a match since
*   1  0  1  4  5  4  5     the number 7 contains the bit pattern
*   0  2  2  4  4  6  6     of the number 3
*   1  2  3  4  5  6  7
                  IF ( MEMTYP .EQ. IAND( JMEMTY( J ), MEMTYP ) ) THEN
                     NMEM = NMEM + 1

*   Memory identifier
                     MEMID( NMEM ) = J

*   Memory size in x
                     MEMSIX( NMEM ) = JMEMSX( J )

*   Memory size in y
                     MEMSIY( NMEM ) = JMEMSY( J )

*   Memory depth
                     MEMDEP( NMEM ) = JMEMDE( J )

*   Memory ITT depths
                     ITTDEP( NMEM ) = JITTDE( J )

                  ENDIF
               ENDDO
            ENDIF
         ENDDO

*   Skip the error handling
         GOTO 98

*   Abort if error during file read
  97     CONTINUE
         STATUS = IDI__WDTER
         CLOSE( UNIT = 1, IOSTAT = ISTAT )
         GOTO 99

*   Close the workstation description table
  98     CONTINUE
         CLOSE( UNIT = 1, IOSTAT = ISTAT )

      ENDIF

  99  CONTINUE

      END

