*+ TAPECREATE - Create a Mag. Tape dataset
      SUBROUTINE TAPECREATE( STATUS )
*    Description :
*     Create the Mag. Tape dataset specified by the %TAPE parameter.
*    Parameters :
*     DRIVE     =MAG_TAPE(READ)
*                Name of the tape dataset to be created.
*     DEVICE    =_CHAR(READ)
*                device name of tape drive
*     DEVDATASET=UNIV(WRITE)
*                name of device dataset
*    Method :
*     Get the location of the dataset, name of the tape drive and
*     name of tape drive as known to the HOST system.
*     Use MAG1_CRTDS to create data structure.
*    Authors :
*     Sid Wright. (UCL::SLW)
*    History :
*     22-Apr-1982:  Device dataset modifications.  (UCL::SLW)
*     16-Jul-1983:  Starlink-ised version.  (UCL::SLW)
*     23-Aug-1984:  Name changed from crtape. (RAL::AJC)
*      5-Sep-1984:  Added parameters to prolog. (RAL::AJC)
*     28-FEB-1990:  Create container file if it doesn't exist (RAL::AJC)
*     03-MAR-1993:  Change the call to MAG$_CRTDS to one to MAG1_CRTDS
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'         ! DAT HDS constants
      INCLUDE 'PAR_ERR'			! Par Errors
      INCLUDE 'MAG_SYS'			! Mag Constants
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) ELOC	! Locator to dataset storage
      CHARACTER*(DAT__SZNAM) TAPE	! Tape dataset name
      CHARACTER*(MAG__SZNAM) DEVICE	! Tape drive id.
*-
    
*    Allowed to execute ?
      IF (STATUS .NE. SAI__OK) RETURN

*
*     Get locator to device dataset container
*
      CALL DAT_EXIST ( 'DEVDATASET', 'UPDATE', ELOC, STATUS )

      IF ( ( STATUS .EQ. PAR__NULL ) .OR. ( STATUS .EQ. PAR__ABORT ) ) 
     :  THEN

         RETURN

      ELSE IF ( STATUS .NE. SAI__OK ) THEN

*       Does not exist so try and create it
         STATUS = SAI__OK
         CALL DAT_CREAT ( 'DEVDATASET', 'DEVICES', 0, 0, STATUS )
         CALL DAT_ASSOC ( 'DEVDATASET', 'UPDATE', ELOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            RETURN
         ENDIF
      ENDIF

*    Get name of tape drive
      CALL PAR_GET0C( 'DRIVE', TAPE, STATUS )
      IF ( (STATUS.EQ.PAR__NULL) .OR. (STATUS.EQ.PAR__ABORT) ) THEN
         RETURN
      ELSEIF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC( 'DRIVE', TAPE )
         CALL ERR_REP( 'MTS_ERROR', '^DRIVE : ^STATUS', STATUS)
         RETURN
      ENDIF

*    Get name of tape drive as known to HOST SYSTEM
      CALL PAR_GET0C( 'DEVICE', DEVICE, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC( 'DRIVE', DEVICE )
         CALL ERR_REP( 'MTS_ERROR', '^DRIVE : ^STATUS', STATUS)
         RETURN
      ENDIF

*    Create data structure
      CALL MAG1_CRTDS( ELOC, TAPE, DEVICE, STATUS )
      CALL DAT_ANNUL( ELOC, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( 'MTS_ERROR', '^STATUS', STATUS )
      ENDIF

      END
