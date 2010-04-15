*+  CRED4_CHECK_GRP - Check a group has been sky-subtracted properly.
      SUBROUTINE CRED4_CHECK_GRP( GROUP, NOBJ, NSKY, EXPOSED, SKYEXP,
     :  OK, STATUS )
*    Description :
*     This routine checks that a given group has been sky-subtracted
*     properly. Either the number of OBJECT and SKY observations
*     and the OBJECT or SKY exposure times should be the same (to
*     within a given tolerance), or there should be no SKY observations
*     and the SKY exposure time should be zero.
*     This routine assumes that 'RGDIR:' has already added to
*     the beginning of the reduced group file name.
*    Invocation :
*     CALL CRED4_CHECK_GRP( GROUP, EXPOSED, SKYEXP, OK, STATUS )
*    Parameters :
*     GROUP        = CHARACTER*(*)( READ )
*          Name of group to be checked (RGDIR:RGyymmdd_gggg).
*     NOBJ         = INTEGER( WRITE )
*          The number of OBJECT observations making up the group.
*     NSKY         = INTEGER( WRITE )
*          The number of SKY observations making up the group.
*     EXPOSED      = REAL( WRITE )
*          The total OBJECT exposure time.
*     SKY          = REAL( WRITE )
*          The total SKY exposure time.
*     OK           = LOGICAL( WRITE )
*          Flag which is .TRUE. if the group has been sky subtracted
*          properly, or .FALSE. if it has not.
*     STATUS       = INTEGER( UPDATE )
*          Global ADAM status. This indicates errors which occurred
*          while accessing the group file. The status is returned
*          as SAI__OK even if the group has not been sky-subtracted
*          properly.
*    Deficiencies :
*     DSA statuses do not have values defined by the ADAM system, and
*     therefore have to be handled separately.
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     26-Oct-1990: Original version.                            (SMB)
*      1-Nov-1990: Modified to check for the presence of
*                  'RGDIR:' at the beginning of the group
*                  name.                                        (SMB)
*     21-Feb-1991: Modified to check the number of OBJECT and
*                  SKY exposures contributing.                  (SMB)
*     11-Feb-1993: Conform to error strategy                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)
     :  GROUP             ! Name of group
*    Export :
      INTEGER
     :  NOBJ,             ! Number of OBJECT observations.
     :  NSKY              ! Number of SKY observations.
      REAL
     :  EXPOSED,          ! Total OBJECT exposure time.
     :  SKYEXP            ! Total SKY exposure time.
      LOGICAL
     :  OK                ! T if the group has been sky-subtracted properly
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local Constants :
      INTEGER DSA__OK
      PARAMETER ( DSA__OK = 0 )
      REAL TOLER          ! Tolerance to which EXPOSED and SKYEXP should match,
*                         !    expressed as a fraction.
      PARAMETER ( TOLER = 0.02 )   ! i.e. 2%
*    Local variables :
      CHARACTER*4
     :  COMMENT           ! Dummy comment
      REAL
     :  DEVIATION         ! Deviation of EXPOSED from SKYEXP as a fraction
*                         !    of EXPOSED.
      INTEGER
     :  DSA_STATUS        ! DSA status.
*    Local data :
*-

*   Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*   Initialise the DSA_STATUS
      DSA_STATUS = DSA__OK

*   Open DSA
      CALL DSA_OPEN( DSA_STATUS )

*   Open the specified GROUP file.
      CALL DSA_NAMED_INPUT( 'GROUP', GROUP, DSA_STATUS )

*   Obtain the number of OBJECT observations (NOBJ) and the number
*   of SKY observations (NSKY) from the FITS parameters.
      CALL DSA_GET_FITS_I( 'GROUP', 'NOBJ', 0, NOBJ,
     :   COMMENT, DSA_STATUS )

      CALL DSA_GET_FITS_I( 'GROUP', 'NSKY', 0, NSKY,
     :  COMMENT, DSA_STATUS )

*   Obtain the object exposure time (EXPOSED) and sky exposure time
*   (SKYEXP) for this group from the FITS parameters.
      CALL DSA_GET_FITS_F( 'GROUP', 'EXPOSED', 0, EXPOSED,
     :  COMMENT, DSA_STATUS )

      CALL DSA_GET_FITS_F( 'GROUP', 'SKYEXP', 0, SKYEXP,
     :  COMMENT, DSA_STATUS )

*   Check everything has worked so far.
      IF ( DSA_STATUS .EQ. DSA__OK ) THEN

*      Check whether EXPOSED or SKYEXP are zero, or if there are
*      no contributing OBJECT or SKY observations.
         IF ( ( NOBJ .EQ. 0 ) .OR. ( EXPOSED .LE. 0.0 ) ) THEN

*         If there are no OBJECT observations, or if EXPOSED is zero,
*         the group is invalid because it contains no information.
            OK = .FALSE.

         ELSE IF ( ( NSKY .EQ. 0 ) .OR. ( SKYEXP .LE. 0.0 ) ) THEN

*         If there are some OBJECT observations, EXPOSED is non-zero
*         but there are no SKY observations, or SKYEXP is zero, the
*         group is valid because is was either taken in CHOP mode or
*         has no sky subtraction at all. (It is perfectly valid to rely
*         on an optimal extraction later on to remove sky).
            OK = .TRUE.

         ELSE

*         NOBJ, NSKY, EXPOSED and SKYEXP are all non-zero. For the group
*         to be valid they should be equal to within the defined tolerance.
            DEVIATION = ABS( EXPOSED - SKYEXP ) / EXPOSED

            IF ( ( NOBJ .EQ. NSKY ) .AND.
     :           ( DEVIATION .LE. TOLER ) ) THEN

               OK = .TRUE.
            ELSE

               OK = .FALSE.
            END IF
         END IF
      ELSE

*      A DSA error occurred while accessing the reduced group file.
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_CHECK_GRP: '/
     :     /'Error accessing reduced group file '/
     :     /'(DSA status follows)', STATUS )
         CALL MSG_SETI( 'DSA_STATUS', DSA_STATUS )
         CALL ERR_REP( ' ', 'CRED4_CHECK_GRP: '/
     :    / 'DSA_STATUS = ^DSA_STATUS', STATUS )
      END IF

*   Close DSA (regardless of the current value of DSA_STATUS)
      CALL DSA_CLOSE( DSA_STATUS )

      END
