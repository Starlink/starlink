#include <config.h>
*+  PARSECON_OPENIFL - Open interface file and return its channel number
      SUBROUTINE PARSECON_OPENIFL ( IFLNAM, LUCON, STATUS )
*    Description :
*     Open the interface file and return its logical unit number.
*    Invocation :
*     CALL PARSECON_OPENIFL ( IFLNAM, LUCON, STATUS )
*    Parameters :
*     IFLNAM=CHARACTER*(*) (given)
*           name of interface file
*     LUCON=INTEGER (returned)
*           logical unit number of interface file
*     STATUS=INTEGER
*    Method :
*     A free FORTRAN unit number is obtained, and the file is opened.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     18.09.1984:  Original (REVAD::BDK)
*     25.10.1984:  assume file is in default directory rather than 
*                  ADAM_IFL (REVAD::BDK)
*     23.05.1985:  use EXENAME instead of TASKNAME (REVAD::BDK)
*     17.06.1991:  Portably obtain Fortran unit number (RLVAD::AJC)
*     16.07.1991:  assume full file name is given (RLVAD::AJC)
*     02.10.1991:  prefix error reports with PARSECON: (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) IFLNAM             ! name of execution module

*    Export :
      INTEGER LUCON                     ! FORTRAN unit number

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER ISTAT                     ! Local status
      LOGICAL OPEN                      ! Whether unit number is in use

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain an unused unit number
      DO LUCON = 1,99
*     For numbers 1 to 99 inquire if number is in use
         INQUIRE (UNIT=LUCON, OPENED=OPEN )
         IF ( .NOT. OPEN ) THEN
*        If number is not in use, use it to open the file
            ISTAT = 0
            OPEN ( UNIT = LUCON, FILE = IFLNAM, STATUS = 'OLD',
#if HAVE_FC_OPEN_READONLY
     :           READONLY,
#endif
     :           IOSTAT = ISTAT )

*        If failed, report reason
            IF ( ISTAT .NE. 0 ) THEN
               STATUS = PARSE__IFLOPN
               CALL EMS_SETC( 'FILE', IFLNAM )
               CALL EMS_REP( 'PCN_OPENIFL1', 
     :          'PARSECON: Failed to open interface file ^FILE',
     :           STATUS )
               CALL EMS_FIOER( 'FIOSTAT', ISTAT )
               CALL EMS_REP( 'PCN_OPENIFL2', '^FIOSTAT', STATUS )
            ENDIF

*        and exit
            GOTO 100

         ENDIF

      ENDDO

*  No unit number available - report
      STATUS = PARSE__IFLOPN
      CALL EMS_SETC( 'FILE', IFLNAM )
      CALL EMS_REP( 'PCN_OPENIFL3', 
     : 'PARSECON: Failed to open interface file ^FILE', STATUS )
      CALL EMS_REP( 'PCN_OPENIFL4',
     : 'No Fortran unit numbers available', STATUS )

100   CONTINUE

      END

