*-----------------------------------------------------------------------
*+  IKNERR - Get Error

      SUBROUTINE IKNERR ( STATUS, MESSAG, MESLEN )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDERR.
*     The arguments are identical to those in IIDERR.
*
*    Invocation :
*     CALL IKNERR( STATUS, MESSAG, MESLEN )
*
*    Method :
*     Assign appropriate string to the output according to the status
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     January 1989
*     December 1990  Changed name from IIDERR
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Error status value
      INTEGER STATUS

*    Export :
*     Error message
      CHARACTER * ( * ) MESSAG

*     Length of error message
      INTEGER MESLEN
*-

*   Convert the error status to a string
      IF ( STATUS .EQ. IDI__OK ) THEN
         MESSAG = 'IDI__OK : Normal status condition'
         MESLEN = 33

      ELSEIF ( STATUS .EQ. IDI__COOVF ) THEN
         MESSAG = 'IDI__COOVF : No more room in common block storage'
         MESLEN = 49

      ELSEIF ( STATUS .EQ. IDI__DEVNM ) THEN
         MESSAG = 'IDI__DEVNM : Problem with device name'
         MESLEN = 37

      ELSEIF ( STATUS .EQ. IDI__ERROR ) THEN
         MESSAG = 'IDI__ERROR : Unspecified error'
         MESLEN = 30

      ELSEIF ( STATUS .EQ. IDI__INCID ) THEN
         MESSAG = 'IDI__INCID : Invalid cursor number'
         MESLEN = 34

      ELSEIF ( STATUS .EQ. IDI__INCON ) THEN
         MESSAG = 'IDI__INCON : Configuration number invalid'
         MESLEN = 41

      ELSEIF ( STATUS .EQ. IDI__INDID ) THEN
         MESSAG = 'IDI__INDID : Invalid display identifier'
         MESLEN = 39

      ELSEIF ( STATUS .EQ. IDI__INITT ) THEN
         MESSAG = 'IDI__INITT : Invalid ITT number'
         MESLEN = 31

      ELSEIF ( STATUS .EQ. IDI__INLUT ) THEN
         MESSAG = 'IDI__INLUT : Invalid look-up table number'
         MESLEN = 41

      ELSEIF ( STATUS .EQ. IDI__INMID ) THEN
         MESSAG = 'IDI__INMID : Invalid memory identifier'
         MESLEN = 38

      ELSEIF ( STATUS .EQ. IDI__INRID ) THEN
         MESSAG = 'IDI__INRID : Invalid ROI identifier'
         MESLEN = 35

      ELSEIF ( STATUS .EQ. IDI__IOERR ) THEN
         MESSAG = 'IDI__IOERR : Error writing to or reading from device'
         MESLEN = 52

      ELSEIF ( STATUS .EQ. IDI__NOCAP ) THEN
         MESSAG = 'IDI__NOCAP : No capability of that number'
         MESLEN = 41

      ELSEIF ( STATUS .EQ. IDI__NOEVA ) THEN
         MESSAG = 'IDI__NOEVA : No such evaluator'
         MESLEN = 30

      ELSEIF ( STATUS .EQ. IDI__NOINT ) THEN
         MESSAG = 'IDI__NOINT : No such interactor'
         MESLEN = 31

      ELSEIF ( STATUS .EQ. IDI__NOOBJ ) THEN
         MESSAG = 'IDI__NOOBJ : No such object'
         MESLEN = 27

      ELSEIF ( STATUS .EQ. IDI__NOREC ) THEN
         MESSAG = 'IDI__NOREC : Error recovering characteristics'
         MESLEN = 45

      ELSEIF ( STATUS .EQ. IDI__NOROI ) THEN
         MESSAG = 'IDI__NOROI : No ROI available'
         MESLEN = 29

      ELSEIF ( STATUS .EQ. IDI__NOTIM ) THEN
         MESSAG = 'IDI__NOTIM : Not implemented'
         MESLEN = 28

      ELSEIF ( STATUS .EQ. IDI__RANGE ) THEN
         MESSAG = 'IDI__RANGE : Parameter outside permissible range'
         MESLEN = 48

      ELSEIF ( STATUS .EQ. IDI__TWOVF ) THEN
         MESSAG = 'IDI__TWOVF : Transfer window overflow'
         MESLEN = 37

      ELSEIF ( STATUS .EQ. IDI__WDTER ) THEN
         MESSAG = 'IDI__WDTER : Error with workstation description file'
         MESLEN = 52

      ELSE
         MESSAG = ' '
         MESLEN = 0
      ENDIF

      END

