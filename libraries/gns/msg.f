      SUBROUTINE GNS_MSG(STATUS, LEN, MSG)
*+
*  Name:
*     GNS_MSG
*
*  Purpose:
*     Translates a GNS error code to a message
*
*  Invocation:
*     CALL GNS_MSG(STATUS, LEN, MSG)
*
*  Description:
*     The given status code is converted to a text message
*
*  Arguments:
*     STATUS = INTEGER (Given)
*         GNS error status code
*     LEN = INTEGER (Returned)
*         Length of message text (can be zero)
*     MSG = CHARACTER
*         Text of message
* 
*  Authors:
*     DLT: D L Terrett (Starlink)

*  History:
*     17-MAY-1989 (DLT):
*        Modified.
*-
      IMPLICIT NONE
      INTEGER STATUS, LEN
      CHARACTER*(*) MSG
      
      INCLUDE 'GNS_ERR'

      IF (STATUS.EQ.GNS__INWKID) THEN
         MSG = 'Invalid GKS workstation identifier'
         LEN = 34
      ELSE IF (STATUS.EQ.GNS__WKNDEF) THEN
         MSG = 'Specified workstation type is not in the GNS database'
         LEN = 54
      ELSE IF (STATUS.EQ.GNS__NOTINT) THEN
         MSG = 'No terminal available - not an interactive job'
         LEN = 46
      ELSE IF (STATUS.EQ.GNS__NAMNR) THEN
         MSG = 'Device name not recognized'
         LEN = 26
      ELSE IF (STATUS.EQ.GNS__PKGNS) THEN
         MSG = 'Package not supported by GNS'
         LEN = 28
      ELSE IF (STATUS.EQ.GNS__DBINV) THEN
         MSG = 'The GNS database file has an invalid format'
         LEN = 43
      ELSE IF (STATUS.EQ.GNS__DBOVF) THEN
         MSG = 'Too many workstation names have been defined'
         LEN = 44
      ELSE IF (STATUS.EQ.GNS__DBOPE) THEN
         MSG = 'Unable to open GNS database file'
         LEN = 32
      ELSE IF (STATUS.EQ.GNS__DBRDE) THEN
         MSG = 'Error while reading the GNS database'
         LEN = 36
      ELSE IF (STATUS.EQ.GNS__DBFME) THEN
         MSG = 'GNS database has an invalid format'
         LEN = 34
      ELSE IF (STATUS.EQ.GNS__UNKCHA) THEN
         MSG = 'Unknown workstation characteristic'
         LEN = 34
      ELSE IF (STATUS.EQ.GNS__AMBNAM) THEN
         MSG = 'Ambiguous workstation name'
         LEN = 26
      ELSE IF (STATUS.EQ.GNS__VNSUP) THEN
         MSG = 'Description file version not supported'
         LEN = 38
      ELSE IF (STATUS.EQ.GNS__AGTND) THEN
         MSG = 'AGI type not defined'
         LEN = 20
      ELSE IF (STATUS.EQ.GNS__AGNNR) THEN
         MSG = 'AGI name not recognised'
         LEN = 23
      ELSE IF (STATUS.EQ.GNS__CIOVF) THEN
         MSG = 'No more connection identifiers can be assigned'
         LEN = 46
      ELSE IF (STATUS.EQ.GNS__NOVER) THEN
         MSG = 'No overlay on the device'
         LEN = 24
      ELSE
         MSG = ' '
         LEN = 0
      END IF
      END
       
