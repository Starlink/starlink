*+  RED4_EFFICIENCY - Determine efficiency at which data were observed.
      SUBROUTINE RED4_EFFICIENCY( STATUS )
*    Description :
*     This routine determines the efficiency at which a particular set
*     of data (an observation or group) were observed. It assumes that
*     the FITS items EXPOSED and SKYEXP contain the total exposure times
*     on sky and object, in seconds, and that RUTSTART and RUTEND are
*     the times, in hours, when the observations started and finished.
*     The efficiency is reported as a percentage.
*    Invocation :
*     CALL RED4_EFFICIENCY( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     24-Oct-1990: Original version.                           (SMB)
*     18-Feb-1993: Conform to error strategy                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER
     :  STATUS               ! Global status
*    Global variables :
*    Local constants :
*    Local variables :
      CHARACTER*80
     :  DATA                 ! Name of data to be examined

*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the data structure to be examined.
      CALL PAR_GET0C( 'DATA', DATA, STATUS )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*     Open DSA
         CALL DSA_OPEN( STATUS )

*      Calculate and report the efficiency. (A separate routine is
*      used so it may be called separately if necessary).
         CALL RED4_EFFICIENCY_2( DATA, STATUS )

*      Close DSA. Note this will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_EFFICIENCY: Error obtaining %DATA parameter', STATUS )
      END IF

      END
