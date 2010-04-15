*+  RED4_CHECK_FORMAT - Checks the format of an observation or integration
      SUBROUTINE RED4_CHECK_FORMAT( NAME, STATUS )
*    Description :
*      This routine uses the FITS tree to determine the file type.
*    Invocation :
*      CALL RED4_CHECK_FORMAT( NAME, STATUS )
*    Parameters :
*    Method :
*    Deficiencies :
*      If the FITS tree does not exist, it won't work.
*    Bugs :
*    Authors :
*     P.N.Daly     (JACH::PND)
*    History :
*     17-Dec-1993: Original version                              (PND)
*     02-Mar-1994: Update to use DSA                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*) NAME               ! The observation / integration name
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'RED4_COMMON.INC'
*    Global variables:
*    Status :
      INTEGER STATUS                   ! ADAM inherited status
*    External references:
*    Local Constants :
*    Local variables :
      CHARACTER*80 SNAME               ! Structure name
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check the FITS structure
      CALL DSA_OPEN( STATUS )
      CALL DSA_NAMED_INPUT( 'INPUT', NAME, STATUS )
      CALL DSA_SPECIFIC_STRUCTURE( 'INPUT', 'FITS', ' ', SNAME, STATUS )
      CALL DSA_CLOSE( STATUS )
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'SNAME', SNAME )
         CALL MSG_OUT( ' ', 'FITS structure is ^SNAME', STATUS)
      END IF

*   We have a file with a .SDF filetype
      IF ( INDEX( SNAME, 'MORE.FIGARO' ) .GT. 0 )THEN
          CALL RED4_SET_FORMAT_2( 'NDF', STATUS )
      ELSE
          CALL RED4_SET_FORMAT_2( 'DST', STATUS )
      ENDIF

*    Exit subroutine
      END
