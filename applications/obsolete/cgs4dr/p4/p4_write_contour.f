*+  P4_WRITE_CONTOUR - Writes the generated contours to a file
      SUBROUTINE P4_WRITE_CONTOUR( PORT, STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     31-Dec-1993: Original version                               (PND)
*      4-Aug-1994: Convert to use FIO for Unix port               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'          ! Defines SAI__OK etc
*    Status :
      INTEGER STATUS
*    Import :
      INTEGER PORT
*    External references :
      INTEGER CHR_LEN
*    Common block :
      INCLUDE 'P4COM.INC'
*    Local Variables :
      INTEGER
     :  CLEN,                    ! String length
     :  CIVALEN,                 ! String length
     :  CRVALEN,                 ! String length
     :  LUN,                     ! Logical unit number
     :  NTICKS,                  ! Time ticks
     :  I                        ! A counter
      CHARACTER*132
     :  COMMENT,                 ! What to write
     :  OFILE                    ! Where to write it to
      CHARACTER*32
     :  USER,                    ! Calling username
     :  SYSNAM,                 ! The system name (OS)
     :  NODENAME,                ! The system nodename
     :  RELEASE,                 ! Software release
     :  VERSION,                 ! Software version id
     :  MACHINE,                 ! Machine type
     :  CALTIME,                 ! Calendar time
     :  CIVAL,                   ! Character encoded value
     :  CRVAL                    ! Character encoded value
*-

*   Return if status on entry is not SAI__OK
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Do some PoSiX calls to get some information
      CALL PSX_CUSERID( USER, STATUS )
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, CALTIME, STATUS )
      CALL PSX_UNAME( SYSNAM, NODENAME,
     :   RELEASE, VERSION, MACHINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_OUT( ' ',
     :     'Unable to obtain PoSiX information '/
     :     /'(error ignored)', STATUS )
         CALL ERR_ANNUL( STATUS )
      ENDIF

*   Open the output file
      CLEN = CHR_LEN( DISPLAY_DATA( PORT ) )
      OFILE = DISPLAY_DATA(PORT)(1:CLEN)//'_contours.lis'
      CALL P4_CHECK_INPUT( OFILE, STATUS )
      CALL FIO_OPEN( OFILE(1:CHR_LEN(OFILE)), 'APPEND', 'LIST', 0, LUN, STATUS )

*   Write a header and a version number identifier into the file.
      CALL FIO_WRITE( LUN, '*+ ', STATUS )
      COMMENT = '* ' // OFILE(1:CHR_LEN(OFILE))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

*   Write some process, hardware and software specific values
      COMMENT = '* Written on node ' // NODENAME(1:CHR_LEN(NODENAME)) /
     :   / ' by ' // USER(1:CHR_LEN(USER)) // ' on ' /
     :   / CALTIME(1:CHR_LEN(CALTIME))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      COMMENT = '* Written under ' // SYSNAM(1:CHR_LEN(SYSNAM)) // ' / '
     :   // RELEASE(1:CHR_LEN(RELEASE)) // ' / '
     :   // VERSION(1:CHR_LEN(VERSION)) // ' / '
     :   // MACHINE(1:CHR_LEN(MACHINE)) // ' '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      CALL FIO_WRITE( LUN, '*- ', STATUS )

*  Tell the user what this file is all about
      COMMENT = '* Contour levels generated for ' /
     :   / DISPLAY_DATA( PORT )( 1:CLEN ) // ' were: '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

*  Write the contour levels out
      DO I = 1, CONTOUR_LEVELS( PORT ), 1

         CALL CHR_ITOC( I, CIVAL, CIVALEN )
         CALL CHR_RTOC( ARRAY_CONTOURS(I), CRVAL, CRVALEN )
         COMMENT = '* Level '// CIVAL(1:CIVALEN) /
     :      / ' = ' // CRVAL(1:CRVALEN)
         CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      ENDDO

*  Write out a success message
      CALL FIO_CLOSE( LUN, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'FILE', OFILE )
         CALL MSG_OUT( ' ',
     :      'Contour levels written to file ^FILE', STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', OFILE )
         CALL ERR_REP( ' ', 'P4_WRITE_CONTOUR: '/
     :      /'Unable to write contour levels to file ^FILE', STATUS )
      ENDIF

      END
