*+  RED4_SET_FORMAT_2.FOR - Sets the format of the reduction files
      SUBROUTINE RED4_SET_FORMAT_2( FORMAT, STATUS )
*    Description :
*    Invocation :
*     CALL RED4_SET_FORMAT_2( FORMAT, STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*           Global ADAM status. This must be SAI__OK on entry, or the
*           routine will not execute. It will be returned SAI__OK if
*           the routine is successful. Otherwise it will contain an
*           error status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      9-Nov-1993: Original version                        (PND)
*     17-Jan-1994: Add template files                      (PND)
*     10-Nov-1994: Attempt to make vaguely portable        (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Input:
      CHARACTER*(*)
     :  FORMAT                     ! Format type
*    Global constants :
      INCLUDE 'SAE_PAR'            ! Contains SAI__OK
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Status :
      INTEGER
     :  STATUS                     ! Global ADAM status
*    External references :
      INTEGER CHR_LEN
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER*20 LPREFIX         ! Prefix for files
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set the appropriate common block variable
      NDF = .FALSE.
      IF ( FORMAT .EQ. 'NDF' ) THEN

         NDF = .TRUE.
         CALL RED4_GET_PREFIX ('NDF_TEMPLATE', LPREFIX, STATUS)
         FITS_STRUCTURE       = 'MORE.FITS'
         CALIB_STRUCTURE      = 'AXIS[1]'
         integration_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'integration_template'
         observation_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'observation_template'
         mask_template        = LPREFIX(:CHR_LEN(LPREFIX))/
     ;      /'mask_template'
         intred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'intred_template'
         obsred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'obsred_template'
         CALL MSG_OUT( ' ', 'Format is NDF', STATUS )
      ELSE IF ( FORMAT .EQ. 'DST' ) THEN

         NDF = .FALSE.
         CALL RED4_GET_PREFIX ('DST_TEMPLATE', LPREFIX, STATUS)
         FITS_STRUCTURE       = 'FITS'
         CALIB_STRUCTURE      = 'X'
         integration_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'integration_template'
         observation_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'observation_template'
         mask_template        = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'mask_template'
         intred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'intred_template'
         obsred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :      /'obsred_template'
         CALL MSG_OUT( ' ', 'Format is DST', STATUS )
      ELSE IF ( FORMAT .EQ. 'EITHER' ) THEN

         CALL RED4_GET_PREFIX ('TEMPLATE', LPREFIX, STATUS)
         integration_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :    /'integration_template'
         observation_template = LPREFIX(:CHR_LEN(LPREFIX))/
     :    /'observation_template'
         mask_template        = LPREFIX(:CHR_LEN(LPREFIX))/
     :    /'mask_template'
         intred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :    /'intred_template'
         obsred_template      = LPREFIX(:CHR_LEN(LPREFIX))/
     :    /'obsred_template'
         CALL MSG_OUT( ' ', 'Auto-sensing of file format enabled', STATUS )
      ELSE

        STATUS = SAI__ERROR
        CALL ERR_REP( ' ',
     :     'Unknown format - must be DST, NDF or EITHER!', STATUS )
      ENDIF

*   Exit subroutine
      END
