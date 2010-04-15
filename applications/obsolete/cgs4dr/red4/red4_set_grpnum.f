*+  RED4_SET_GRPNUM - Set GRPNUM in a reduced group file
      SUBROUTINE RED4_SET_GRPNUM( STATUS )
*    Description :
*     This routine sets GRPNUM in a reduced OBJECT or SKY
*    Invocation :
*     CALL RED4_SET_GRPNUM( STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*           Global ADAM status. This must be ADAM__OK on entry, or the
*           routine will not execute. It will be returned ADAM__OK if
*           the routine is successful. Otherwise it will contain an
*           error status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*     18-Oct-1991: Original version                        (PND)
*     23-Feb-1993: Conform to error strategy               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'      ! Contains ADAM__OK
      INCLUDE 'SAI_ERR'       ! Contains SAI__ERROR
*    Status :
      INTEGER
     :  STATUS                ! Global ADAM status
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER
     :  GRPNUM                ! The group number
      CHARACTER*80
     :  OBSRED                ! Name of reduced observation file
*    Internal References :
*    Local data :
      CHARACTER*20
     :  OBSTYPE               ! The observation type (should be OBJECT or SKY)
      CHARACTER*4
     :  COMMENT               ! Dummy comment
      INTEGER
     :  DSA_STATUS            ! DSA status
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of a reduced observation
      CALL PAR_GET0C( 'OBSRED', OBSRED, STATUS )

*   Obtain a group number to be used
      CALL PAR_GET0I( 'GRPNUM', GRPNUM, STATUS )

*   Check these parameters have been obtained successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*     Open DSA
         DSA_STATUS = ADAM__OK
         CALL DSA_OPEN( DSA_STATUS )
         CALL RED4_CHECK_INPUT( OBSRED, STATUS )
         CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED, DSA_STATUS )

*     Get the observation type and check this is 'OBJECT' or 'SKY'
         CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBSTYPE,
     :     COMMENT, DSA_STATUS )

         IF ( ( OBSTYPE .EQ. 'OBJECT' ) .OR.
     :        ( OBSTYPE .EQ. 'SKY' )  ) THEN

*     Set the group number in the FITS header of this observation
            CALL DSA_PUT_FITS_I( 'OBSRED', 'GRPNUM', GRPNUM,
     :        COMMENT, DSA_STATUS )
         ELSE

            CALL MSG_SETC( 'OBS', OBSRED )
            CALL MSG_OUT( ' ', 'Reduced observation ^OBS'/
     :        /'is not an OBJECT or SKY', STATUS )
         END IF

*     Close DSA
         CALL DSA_CLOSE( DSA_STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_SET_GRPNUM: '/
     :     /'Error obtaining %OBSRED and '/
     :     /'%GRPNUM parameters', STATUS )
      END IF

      END
