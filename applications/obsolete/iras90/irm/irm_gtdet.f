      SUBROUTINE IRM_GTDET( PARAM, NDET, ADET, DETOUT, NOUT, STATUS )
*+
*  Name:
*     IRM_GTDET

*  Purpose:
*     Get a set of detector numbers from the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GTDET( PARAM, NDET, ADET, DETOUT, NOUT, STATUS )

*  Description:
*     This routine obtains a set of detector numbers from the
*     environment using the parameter specified by PARAM. The detectors
*     must be selected from the set of available detectors specified by
*     ADET. The detectors are specified by a group expression in which
*     each member of the group can be any of the following:
*
*     - An integer in the range 1 to 62 specifying an explicit
*     detector number.
*
*     - One of the strings "12um", "25um", "60um" and "100um". These
*     cause all available detectors from the specified wave band to be
*     selected.
*
*     - A range of detectors, specified by two detector numbers
*     sepatated by a colon (":"). All available detectors whose
*     focal plane Z positions are between those of the two detectors are
*     selected. If the first detector number is omitted, no lower limit
*     is imposed. If the second detector is omitted, no upper limit is
*     imposed.
*
*      - The string "ALL". This specifies that all available detectors
*      should be selected.
*
*      - The string "SMALL". This specifies that all available small
*      detectors (i.e. less than 4 arc-minutes cross-scan size) should
*      be selected.
*
*      - The string "LARGE". This specifies that all available detectors
*      should be selected, excluding the small detectors.
*
*      - The string "NONE". This specifies that no detectors should be
*      selected, and may not be mixed with other options. A null
*      parameter value is equivalent to "NONE".
*
*      - The string "LIST". This causes a list of available detectors
*      to be displayed, but no detectors are selected. The user is
*      re-prompted if this option is selected. This option may not be
*      mixed with any other options.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter.
*     NDET = INTEGER (Given)
*        The total number of available detectors.
*     ADET( NDET ) = INTEGER (Given)
*        Array holding the available detector numbers
*     DETOUT( NDET ) = INTEGER (Returned)
*        Array holding the selected detector numbers in the order of
*        increasing focal plane Z coordinate.
*     NOUT = INTEGER (Returned)
*        The number of selected detectors. Returned equal to 1 if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  "ALL", "SMALL", "LARGE", "NONE" and "LIST" can be abbreviated.
*     -  The union of the specified detectors are returned. Thus for
*     instance, if all 62 detectors are available, and the group
*     expression "12um,small" is supplied, the returned set of
*     detectors consists of all 12um detectors together with all small
*     detectors from all 4 bands, not just the small detectors from the
*     12um band.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (WG):
*        Original version. (Based on INTERIM version GTDETS by DSB)
*     10-NOV-1992 (DSB):
*        Re-written to use a group expression rather than an array of
*        character strings. Selection of detectors by waveband and
*        "LARGE" added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS satellite and mission data.
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.

*  Arguments Given:
      CHARACTER*(*) PARAM
      INTEGER NDET
      INTEGER ADET( NDET )

*  Arguments Returned:
      INTEGER DETOUT( NDET )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Find used length of a string

*  Local Constants:
      INTEGER NOTAV              ! A detector is not available
                                 ! if DTSTAT = NOTAV.
      PARAMETER ( NOTAV = 0 )

      INTEGER AVAIL              ! A detector is available but not
                                 ! selected if DTSTAT = AVAIL.
      PARAMETER ( AVAIL = 1 )

      INTEGER SELECT             ! A detector is available and
                                 ! selected if DTSTAT = SELECT.
      PARAMETER ( SELECT = 3 )


*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression obtained from
                                 ! environment.
      CHARACTER LIS*512          ! Available detector list
      CHARACTER SPEC*(GRP__SZNAM)! A detector specification.


      DOUBLE PRECISION DD( 2*I90__DETS )! Workspace for sorted detector
                                 ! numbers and Z values.

      INTEGER ADDED              ! No. of specifications added by last
                                 ! call to GRP_GROUP.
      INTEGER BAND               ! Requested wvaeband index.
      INTEGER COLON              ! Position of ":" within specification.
      INTEGER DTSTAT( I90__DETS )! State of each detector.
      INTEGER HIDET              ! Last detector number of a range
      INTEGER I                  ! DO loop index
      INTEGER IGRP1              ! GRP identifier.
      INTEGER IGRP2              ! GRP identifier.
      INTEGER IGRP3              ! GRP identifier.
      INTEGER K                  ! DO loop index
      INTEGER LNSPEC             ! Used length of a detector
                                 ! specification.
      INTEGER LODET              ! First detector number of a range
      INTEGER NBAD               ! Number of re-prompts.
      INTEGER NGOOD              ! Number of good integer values.
      INTEGER NSPEC              ! Number of supplied detector
                                 ! specifications.
      INTEGER NUM                ! A detector number
      INTEGER WAVEL              ! Requested wave band wavelength.


      LOGICAL FLAG               ! True if more detector specifications
                                 ! are yet to be given.
      LOGICAL OK                 ! True if available detectors satisfy
                                 ! the requested property.
      LOGICAL PROMPT             ! Re-prompt flag


      REAL HIZ                   ! Higher z position limit of a range
      REAL LOZ                   ! Lower z position limit of a range
      REAL TMP                   ! Temporary storage of range limits
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set all detector states to "not available".
      DO I = 1, I90__DETS
         DTSTAT( I ) = NOTAV
      END DO

*  Create a group to hold the detector numbers.
      CALL GRP_NEW( 'Detector specifications', IGRP1, STATUS )

*  Ensure that the group is case insensitive.
      CALL GRP_SETCS( IGRP1, .FALSE., STATUS )

*  Initialise the number of prompts made, and a flag indicating that
*  another prompt is required.
      NBAD = 0
      PROMPT = .TRUE.

*  Enter a do loop until a valid input string is obtained, or an error
*  occurrs.
      DO WHILE ( PROMPT .AND. STATUS .EQ. SAI__OK )

*  Set the flag to indicate that no more prompts are required. This is
*  updated if an error occurrs, or if "LIST" is selected.
         PROMPT = .FALSE.

*  Loop through all the available detectors, setting the detector
*  states to "available" for all valid detectors specified by argument
*  ADET. Report an error if any invalid detector numbers have been
*  supplied.
         DO I = 1, MIN( I90__DETS, NDET )
            NUM = ADET( I )

            IF( NUM .GT. 0 .AND. NUM .LE. I90__DETS ) THEN
               DTSTAT( NUM ) = AVAIL

            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'NUM', NUM )
               CALL ERR_REP( 'IRM_GTDET_ERR1',
     :       'IRM_GTDET: Bad detector "^NUM" supplied in argument ADET',
     :                    STATUS )
               GOTO  999
            END IF

         END DO

*  Obtain a group of detector specifications. Loop until the group
*  expression obtained from the environment does not terminate with a
*  minus sign. The parameter is cancelled each time.
         FLAG = .TRUE.
         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

            CALL PAR_GET0C( PARAM, GRPEXP, STATUS )
            CALL PAR_CANCL( PARAM, STATUS )
            CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP1, NSPEC, ADDED,
     :                      FLAG, STATUS )
            IF( FLAG ) CALL MSG_OUTIF( MSG__NORM, 'IRM_GTDET_MSG1',
     :                      '  Please specify more detectors...',
     :                                 STATUS )

         END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Create a new group from which repeated elements are purged. Set the
*  size of the old group to zero.
         CALL GRP_PURGE( IGRP1, IGRP2, STATUS )
         CALL GRP_SETSZ( IGRP1, 0, STATUS )

*  Create a group from which any blank elements have been removed.
*  Delete the intermediate group.
         CALL GRP_REMOV( IGRP2, ' ', IGRP3, STATUS )
         CALL GRP_DELET( IGRP2, STATUS )

*  Get the size of the new group.
         CALL GRP_GRPSZ( IGRP3, NSPEC, STATUS )

*  Each element of the group gives a specification. Process them one
*  by one until all have been processed or an error occurs.
         K = 0
         DO WHILE( K .LT. NSPEC .AND. STATUS .EQ. SAI__OK )
            K = K + 1

*  Get the next element from the group.
            CALL GRP_GET( IGRP3, K, 1, SPEC, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 20

*  Remove all blanks and get the used length of the specification (this
*  will never be zero because completely blank names have already been
*  removed from the group).
            CALL CHR_RMBLK( SPEC )
            LNSPEC = CHR_LEN( SPEC )

*  Get the position of any colon in the specification.
            COLON= INDEX( SPEC, ':' )

*  First process "LIST" specifications.
            IF( INDEX( 'LIST', SPEC( : LNSPEC ) ) .EQ. 1 ) THEN

*  Report an error if any other detector specifications are contained in
*  the group.
               IF( NSPEC .NE. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'IRM_GTDET_ERR2',
     :             'IRM_GTDET: "LIST" can only be specified by itself.',
     :                          STATUS )
                  GO TO 20

               END IF

*  Store the available detectors, and the focal plane Z coordinate
*  at the detector centres.
               DO I = 1, NDET
                  DD( I ) = DBLE( ADET( I ) )
                  DD( I + NDET ) = DBLE( I90__DETZ( ADET( I ) ) )
               END DO

*  Sort the detectors so that they have monotonic increasing Z values.
               CALL IRM_SORTD( .TRUE., 2, NDET, 2, DD, NGOOD, STATUS )

*  Format the available detector numbers in a list, and
*  output the list.
               WRITE( LIS, 10 )   ( NINT( DD( I ) ), I = 1, NDET )
 10            FORMAT( 62( I2: ',' ) )
               CALL CHR_RMBLK( LIS )

               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL MSG_OUTIF( MSG__NORM, 'IRM_GTDET_MSG1',
     :                      '  The following detectors are available: ',
     :                         STATUS )
               CALL MSG_SETC( 'LIS', LIS )
               CALL MSG_OUTIF( MSG__NORM, 'IRM_GTDET_MSG2',
     :                         '  ^LIS', STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Indicate that another prompt is required.
               PROMPT = .TRUE.

*  If 'ALL' is specified, select all available detectors.
            ELSE IF ( INDEX( 'ALL', SPEC( : LNSPEC ) ) .EQ. 1 ) THEN

               DO I = 1, NDET
                  DTSTAT( ADET( I ) ) = SELECT
               END DO

*  If 'SMALL' is specified, select available small detectors.
            ELSE IF ( INDEX( 'SMALL', SPEC( : LNSPEC ) ) .EQ. 1 ) THEN

*  Loop through all small detectors.
               OK = .FALSE.
               DO I = 1, I90__NSMAL
                  NUM = I90__SMALL( I )

*  Check whether this small detector is an available detector. If so,
*  select it.
                  IF ( DTSTAT( NUM ) .NE. NOTAV ) THEN
                     DTSTAT( NUM ) = SELECT
                     OK = .TRUE.
                  END IF

               END DO

*  Report an error if no small detectors are available.
               IF( .NOT. OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'IRM_GTDET_ERR3',
     :                        'IRM_GTDET: No small detectors available',
     :                          STATUS )
                  GO TO 20
               END IF

*  If 'LARGE' is specified, select all available non-small detectors.
            ELSE IF ( INDEX( 'LARGE', SPEC( : LNSPEC ) ) .EQ. 1 ) THEN

*  Loop through all available detectors.
               OK = .FALSE.
               DO I = 1, NDET
                  NUM = ADET( I )

*  Check whether this is a small detector. If not, select it.
                  IF ( I90__DETDZ( NUM ) .GT. I90__SMSIZ ) THEN
                     DTSTAT( NUM ) = SELECT
                     OK = .TRUE.
                  END IF

               END DO

*  Report an error if no large detectors are available.
               IF( .NOT. OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'IRM_GTDET_ERR4',
     :                        'IRM_GTDET: No large detectors available',
     :                          STATUS )
                  GO TO 20
               END IF

*  If 'NONE' is specified, report an error if any other detector
*  specifications are conatined in the group.
            ELSE IF ( INDEX( 'NONE', SPEC( : LNSPEC ) ) .EQ. 1 ) THEN

               IF( NSPEC .NE. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'IRM_GTDET_ERR5',
     :             'IRM_GTDET: "NONE" can only be specified by itself.',
     :                          STATUS )
                  GO TO 20
               END IF

*  If a range of detectors is defined.
            ELSE IF( COLON .NE. 0 )  THEN
               OK = .FALSE.

*  If the ':' is the first character, set the low Z limit to a
*  extreme value.
               IF( COLON .EQ. 1 ) THEN
                  LOZ = VAL__MINR

*  Otherwise, attempt to read an integer from the string preceeding the
*  ':' character.
               ELSE
                  CALL CHR_CTOI( SPEC( : COLON - 1 ), LODET, STATUS )

*  Add a report if an error occurred (CHR_CTOI doesn't make a report
*  if an error occurrs.. naughty!!!...this may be corrected in future).
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'S', SPEC( : COLON - 1) )
                     CALL ERR_REP( 'IRM_GTDET_ERR6',
     :                    'IRM_GTDET: Bad numeric value "^S" supplied.',
     :                             STATUS )
                     GO TO 20
                  END IF

*  If all went well, and the number represents a valid, available
*  detector, get the cross scan position of the specified detector, and
*  select the detector.
                  IF( LODET .GT. 1 .AND. LODET .LE. I90__DETS ) THEN

                     IF( DTSTAT( LODET ) .NE. NOTAV ) THEN
                        LOZ = I90__DETZ( LODET )
                        DTSTAT( LODET ) = SELECT
                        OK = .TRUE.

*  If the detector is not available, report an error.
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'D', LODET )
                        CALL ERR_REP( 'IRM_GTDET_ERR7',
     :                      'IRM_GTDET: Detector #^D is not available.',
     :                                STATUS )
                        GO TO 20
                     END IF

*  If a bad detector number was given, report an error.
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'D', LODET )
                     CALL ERR_REP( 'IRM_GTDET_ERR8',
     :                        'IRM_GTDET: Detector #^D does not exist.',
     :                              STATUS )
                     GO TO 20
                  END IF

               END IF

*  If the ':' is the last character, set the high Z limit to a
*  extreme value.
               IF( COLON .EQ. LNSPEC ) THEN
                  HIZ = VAL__MAXR

*  Otherwise, attempt to read an integer from the string following the
*  ':' character.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_CTOI( SPEC( COLON + 1 : ), HIDET, STATUS )

*  Add a report if an error occurred (CHR_CTOI doesn't make a report
*  if an error occurrs.. naughty!!!...this may be corrected in future).
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'S', SPEC( COLON + 1 : ) )
                     CALL ERR_REP( 'IRM_GTDET_ERR9',
     :                    'IRM_GTDET: Bad numeric value "^S" supplied.',
     :                             STATUS )
                     GO TO 20
                  END IF

*  If all went well, and the number represents a valid, available
*  detector, get the cross scan position of the specified detector, and
*  select the detector.
                  IF( HIDET .GT. 1 .AND. HIDET .LE. I90__DETS ) THEN

                     IF( DTSTAT( HIDET ) .NE. NOTAV ) THEN
                        HIZ = I90__DETZ( HIDET )
                        DTSTAT( HIDET ) = SELECT
                        OK = .TRUE.

*  If the detector is not available, report an error.
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'D', HIDET )
                        CALL ERR_REP( 'IRM_GTDET_ERR10',
     :                      'IRM_GTDET: Detector #^D is not available.',
     :                                STATUS )
                        GO TO 20
                     END IF

*  If a bad detector number was given, report an error.
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'D', HIDET )
                     CALL ERR_REP( 'IRM_GTDET_ERR11',
     :                        'IRM_GTDET: Detector #^D does not exist.',
     :                             STATUS )
                     GO TO 20
                  END IF

               END IF

*  If range limits are given in wrong order, swap them round.
               IF ( HIZ .LT. LOZ ) THEN
                  TMP = HIZ
                  HIZ = LOZ
                  LOZ =TMP
               END IF

*  Select all avalable detectors whose z position are between
*  LOZ and HIZ.
               DO I = 1, NDET

                  IF ( I90__DETZ( ADET( I ) ) .GE. LOZ .AND.
     :                 I90__DETZ( ADET( I ) ) .LE. HIZ ) THEN

                     DTSTAT( ADET( I ) ) = SELECT
                     OK = .TRUE.

                  END IF

               END DO

*  Report an error if no detectors are available within this range.
               IF( .NOT. OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETR( 'LO', LOZ )
                  CALL MSG_SETR( 'HI', HIZ )
                  CALL ERR_REP( 'IRM_GTDET_ERR12',
     :     'IRM_GTDET: No detectors available in the range Z=(^LO,^HI)',
     :                          STATUS )
                  GO TO 20
               END IF

*  If the specification end with the string "um" then select the
*  available detectors from the specified band.
            ELSE IF( SPEC( LNSPEC - 1 : ) .EQ. 'UM' ) THEN
               CALL CHR_CTOI( SPEC( : LNSPEC - 2 ), WAVEL, STATUS )

*  Add a report if an error occurred (CHR_CTOI doesn't make a report
*  if an error occurrs.. naughty!!!...this may be corrected in future).
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'S', SPEC( : LNSPEC - 2 ) )
                  CALL ERR_REP( 'IRM_GTDET_ERR13',
     :                    'IRM_GTDET: Bad numeric value "^S" supplied.',
     :                          STATUS )
                  GO TO 20
               END IF

*  Find the waveband index.
               BAND = 0
               DO I = 1, I90__BANDS
                  IF( I90__WAVEL( I ) .EQ. WAVEL ) BAND = I
               END DO

*  Report an error if the wave band does not exist.
               IF( BAND .EQ. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'B', WAVEL )
                  CALL ERR_REP( 'IRM_GTDET_ERR14',
     :                          'IRM_GTDET: ^B um band does not exist.',
     :                          STATUS )
                  GO TO 20
               END IF

*  Loop through each available detector.
               OK = .FALSE.
               DO I = 1, NDET
                  NUM = ADET( I )

*  If this detector belongs to the requested waveband, select it.
                  IF( I90__DBAND( NUM ) .EQ. BAND ) THEN
                     DTSTAT( NUM ) = SELECT
                     OK = .TRUE.
                  END IF

               END DO

*  Report an error if no detectors are available in this band.
               IF( .NOT. OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'W', WAVEL )
                  CALL ERR_REP( 'IRM_GTDET_ERR15',
     :            'IRM_GTDET: No detectors available in the ^W um band',
     :                          STATUS )
                  GO TO 20
               END IF

*  If none of LIST, ALL, SMALL, NONE, RANGE and BAND is specified, this
*  section must be a detector number. Attempt to convert it to numeric
*  form.
            ELSE
               CALL CHR_CTOI( SPEC( : LNSPEC), NUM, STATUS )

*  Add a report if an error occurred (CHR_CTOI doesn't make a report
*  if an error occurrs.. naughty!!!...this may be corrected in future).
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'S', SPEC( : LNSPEC ) )
                     CALL ERR_REP( 'IRM_GTDET_ERR16',
     :                    'IRM_GTDET: Bad numeric value "^S" supplied.',
     :                             STATUS )
                     GO TO 20
                  END IF

*  If the number corresponds to a valid, available detector, select it.
               IF( NUM .GT. 0 .AND. NUM .LE. I90__DETS ) THEN

                  IF( DTSTAT( NUM ) .NE. NOTAV ) THEN
                     DTSTAT( NUM ) = SELECT

*  If the detector is not available, report an error.
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'D', NUM )
                     CALL ERR_REP( 'IRM_GTDET_ERR17',
     :                      'IRM_GTDET: Detector #^D is not available.',
     :                              STATUS )
                     GO TO 20
                  END IF

*  If a bad detector number was given, report an error.
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'D', NUM )
                  CALL ERR_REP( 'IRM_GTDET_ERR18',
     :                        'IRM_GTDET: Detector #^D does not exist.',
     :                          STATUS )
                  GO TO 20
               END IF

            END IF

*  Arrive here if an error occurrs processing the current specification.
  20        CONTINUE

*  Go back to process the next specification, if any are still left and
*  no error has been reported.
         END DO

*  Delete the group holding the detector specifications.
         CALL GRP_DELET( IGRP3, STATUS )

*  If an error occurred while processing the supplied group of detector
*  specifications (other than parameter abort)...
         IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN

*  ...display a list of available detectors if this is the first
*  re-prompt.
            IF( NBAD .EQ. 0 ) THEN
               WRITE( LIS, 10 )   ( ADET( I ), I = 1, NDET )
               CALL ERR_REP( 'IRM_GTDET_ERR19',
     :                      '  The following detectors are available: ',
     :                       STATUS )
               CALL MSG_SETC( 'LIS', LIS )
               CALL ERR_REP( 'IRM_GTDET_ERR20', '  ^LIS', STATUS )
            END IF

*  Add a report asking the user to give a new group of detector
*  specifications.
            CALL ERR_REP( 'IRM_GTDET_ERR21',
     :                    '  Please give a new group of detectors',
     :                    STATUS )

*  Flush the errors.
            CALL ERR_FLUSH( STATUS )

*  Increment the number of re-prompts given.
            NBAD = NBAD + 1

*  Indicate that another prompt is required, so long as the max. number
*  of re-prompts has not been reached.
            IF( NBAD .LE. 4 ) PROMPT = .TRUE.

         END IF

      END DO

*  A list of detector has been selected. Order the selected detectors
*  such that they are from the hightest z position to the lowest. and
*  count the number of selected detector.
      NOUT = 0
      DO I = 1, I90__DETS

         NUM = I90__XDETS( I )

         IF( DTSTAT( NUM ) .EQ. SELECT ) THEN
            NOUT = NOUT + 1
            DETOUT( NOUT ) = NUM
         END IF

      END DO

*  Delete the group used to store the strings supplied by the user.
  999 CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )

*  If an error has occurred, give a context message, and return a value
*  of 1 for NOUT to avoid possible adjustable array dimension errors
*  when calling later subroutines.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'IRM_GTDET_ERR22',
     :          'IRM_GTDET: Unable to get a group of detectors using '//
     :          'parameter %^P', STATUS )
         NOUT = 1
      END IF

      END
