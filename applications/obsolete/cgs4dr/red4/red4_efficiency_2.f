*+  RED4_EFFICIENCY_2 - Determine efficiency at which data were observed.
      SUBROUTINE RED4_EFFICIENCY_2( DATA, STATUS )
*    Description :
*     This routine determines the efficiency at which the given set
*     of data (an observation or group) were observed. It assumes that
*     the FITS items EXPOSED and SKYEXP contain the total exposure times
*     on sky and object, in seconds, and that RUTSTART and RUTEND are
*     the times, in hours, when the observations started and finished.
*     The efficiency is reported as a percentage.
*     It is assumed that DSA has already been opened.
*    Invocation :
*     CALL RED4_EFFICIENCY_2( DATA, STATUS )
*    Parameters :
*     DATA      = CHARACTER*(*)( READ )
*           The name of the data structure to be examined.
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
*     24-Oct-1990: Original version.                              (SMB)
*     25-Oct-1990: Spurious errors were being generated because
*                  rounding errors made the subtraction of
*                  RUTEND and RUTSTART only accurate to ~0.5 sec.
*                  Modified to assume 100% efficiency if the
*                  elapsed time is less than but within 0.5 sec
*                  of the exposure time.                          (SMB)
*      8-Feb-1991: Mistake corrected. Efficiency needs to be
*                  multiplied by DETNINCR.                        (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.   (SMB)
*     19-Feb-1993: Conform to error strategy                      (PND)
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
*    Import :
      CHARACTER*(*)
     :  DATA                 ! Name of data to be examined
*    Global variables :
*    Local constants :
      REAL SECS_PER_HOUR                   ! The number of seconds in one hour
      PARAMETER ( SECS_PER_HOUR = 3600.0 )
      REAL PERCENT                         ! Factor to convert fraction to %
      PARAMETER ( PERCENT = 100.0 )
      REAL ACCURACY                        ! Accuracy of TOTAL_TIME in seconds
      PARAMETER ( ACCURACY = 0.5 )
*    Local variables :
      CHARACTER*4
     :  ACCESS,              ! Access code for FITS item
     :  COMMENT              ! Dummy comment
      LOGICAL
     :  EXIST                ! Flag indicating if FITS item exists
      INTEGER
     :  DET_NINCR,           ! Number of detector (oversampling) steps.
     :  ELEMENTS,            ! Number of elements in FITS item
     :  STRLEN               ! Size of character FITS item
      REAL
     :  EXPOSED,             ! Total exposure time on object, in seconds
     :  SKYEXP,              ! Total exposure time on sky, in seconds
     :  RUTSTART,            ! Start time, in hours
     :  RUTEND,              ! End time, in hours
     :  USEFUL_TIME,         ! Total useful time, in seconds.
     :  TOTAL_TIME,          ! Total elapsed time, in seconds.
     :  EFFIC                ! The percentage efficiency.
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Open the data structure
      CALL RED4_CHECK_INPUT( DATA, STATUS )
      CALL DSA_NAMED_INPUT( 'DATA', DATA, STATUS )

*   Check that everything has worked up to this point
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Obtain the total object exposure time in seconds from
*      the EXPOSED fits item (which ought to exist).
         CALL DSA_GET_FITS_F( 'DATA', 'EXPOSED', 0, EXPOSED,
     :     COMMENT, STATUS )

*      Check if the SKYEXP item exists. If it does, then the data
*      has been sky-subtracted and the sky exposure time in SKYEXP
*      should be taken into account. If SKYEXP does not exist, set
*      it to zero.
         CALL DSA_SEEK_FITS( 'DATA', 'SKYEXP', EXIST, ACCESS,
     :     ELEMENTS, STRLEN, STATUS )

         IF ( EXIST ) THEN

            CALL DSA_GET_FITS_F( 'DATA', 'SKYEXP', 0, SKYEXP,
     :        COMMENT, STATUS )
         ELSE

            SKYEXP = 0.0
         END IF

*      Obtain the start and end times in hours from the RUSTART
*      and RUTEND fits items (which ought to exist).
         CALL DSA_GET_FITS_F( 'DATA', 'RUTSTART', 0, RUTSTART,
     :     COMMENT, STATUS )
         CALL DSA_GET_FITS_F( 'DATA', 'RUTEND', 0, RUTEND,
     :     COMMENT, STATUS )

*      Obtain the detector oversampling factor.
         CALL DSA_GET_FITS_I( 'DATA', 'DETNINCR', 0, DET_NINCR,
     :     COMMENT, STATUS )

*      Check the FITS items have been obtained successfully.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Obtain the total useful time and elapsed time in seconds.
            USEFUL_TIME = DET_NINCR * ( EXPOSED + SKYEXP )
            TOTAL_TIME = ( RUTEND - RUTSTART ) * SECS_PER_HOUR

*         Check these figures are sensible.
            IF ( ( USEFUL_TIME .GT. 0 ) .AND.
     :           ( TOTAL_TIME .GT. 0 ) ) THEN

*            Check that the USEFUL_TIME is indeed less than or
*            equal to the TOTAL_TIME.
               IF ( USEFUL_TIME .LE. TOTAL_TIME ) THEN

*               Calculate the efficiency as a percentage and report this.
                  EFFIC = PERCENT * USEFUL_TIME / TOTAL_TIME
                  CALL MSG_SETC( 'DATA', DATA )
                  CALL MSG_FMTR( 'EFFIC', 'F6.1', EFFIC )
                  CALL MSG_OUT( ' ', '^DATA was observed with '/
     :              /'^EFFIC percent efficiency.', STATUS )
               ELSE

*               The USEFUL_TIME is greater than TOTAL_TIME, but the
*               TOTAL_TIME is only accurate to ACCURACY seconds.
*               If the difference between USEFUL_TIME and TOTAL_TIME is
*               within this limit assume 100% efficiency. Otherwise
*               there must be an error.
                  IF ( (USEFUL_TIME-TOTAL_TIME) .LE. ACCURACY ) THEN

                     CALL MSG_SETC( 'DATA', DATA )
                     CALL MSG_OUT( ' ', '^DATA was observed with '/
     :                 /'~100 percent efficiency.', STATUS )
                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :                 /'Efficiency is greater than 100 percent!',
     :                 STATUS )
                     CALL MSG_SETR( 'USEFUL_TIME', USEFUL_TIME )
                     CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :                 /'Total exposure time = ^USEFUL_TIME', STATUS )
                     CALL MSG_SETR( 'TOTAL_TIME', TOTAL_TIME )
                     CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :                 /'Total elapsed time = ^TOTAL_TIME', STATUS )
                  END IF
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :           /'Invalid FITS items in data structure', STATUS )
               CALL MSG_SETR( 'USEFUL_TIME', USEFUL_TIME )
               CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :            /'Total exposure time = ^USEFUL_TIME', STATUS )
               CALL MSG_SETR( 'TOTAL_TIME', TOTAL_TIME )
               CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :            /'Total elapsed time = ^TOTAL_TIME', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :        /'Error accessing FITS items in data structure', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_EFFICIENCY_2: '/
     :     /'Error accessing data structure', STATUS )
      END IF

      END
