      SUBROUTINE SCULIB_REM_SKY(MODE, N_BOLS, N_POS, SCUDATA, SCUVAR,
     :     SCUQUAL, CLIP, N_SKYBOLS, SKYBOLS, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_REM_SKY

*  Purpose:
*     To remove sky background from SCUBA data

*  Invocation:
*     SUBROUTINE SCULIB_REM_SKY(MODE, N_BOLS, N_POS, SCUDATA, SCUVAR,
*    :     SCUQUAL, CLIP, N_SKYBOLS, SKYBOLS, BADBIT, STATUS)

*  Description:
*     This routine goes through each jiggle position in turn, finding
*     the sky background level by using the selected SKY bolometers.
*     This level is removed from each bolometer.
*     The background level is determined by the median of the SKY data
*     for each jiggle.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        Clip mode
*     N_BOLS = INTEGER (Given)
*        Number of bolometers in data set
*     N_POS = INTEGER (Given)
*        Number of jiggle positions in data set
*     SCUDATA(N_BOLS, N_POS) = REAL (Given & Returned)
*        The data
*     SCUVAR(N_BOLS, N_POS) = REAL (Given & Returned)
*        The variance on the data
*     SCUQUAL(N_BOLS, N_POS) = BYTE (Given & Returned)
*        The data quality
*     CLIP = _REAL (Given)
*        Iterative sigma clipping level
*     N_SKYBOLS = INTEGER (Given)
*        Number of sky bolometers
*     SKYBOLS (N_SKYBOLS) = INTEGER (Given)
*        List of sky bolometers
*     BADBIT = BYTE (Given)
*        Bad bit mask
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:
*     Uses Kappa library routines

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     1996 November 17 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! VAL__ constants
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'MSG_PAR'               ! MSG__ constants

*  Arguments Given:
      BYTE    BADBIT
      REAL    CLIP
      CHARACTER * (*) MODE
      INTEGER N_BOLS
      INTEGER N_POS
      INTEGER N_SKYBOLS
      INTEGER SKYBOLS(N_SKYBOLS)

*  Arguments Given and Returned:
      REAL    SCUDATA(N_BOLS, N_POS)
      BYTE    SCUQUAL(N_BOLS, N_POS)
      REAL    SCUVAR(N_BOLS, N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  Local constants
      INTEGER MAX__BOL
      PARAMETER (MAX__BOL = 100) ! Maximum number of sky bolometers

*  Local Variables:
      REAL    BACKGROUND           ! Sky background level
      INTEGER BOL                  ! Loop counter
      INTEGER I                    ! Loop counter
      INTEGER GOOD                 ! Number of good data points
      REAL    ERRMEAN              ! Error in mean of SKY_DATA
      DOUBLE PRECISION MEAN        ! Mean of sky values
      DOUBLE PRECISION MEDIAN      ! Median of sky values
      INTEGER N_GOOD               ! Number of good data points in SKY
      INTEGER SKY                  ! Loop counter for SKY bolometers
      REAL    SKYVAR               ! Variance on sky background
      REAL    SKY_DATA(MAX__BOL)   ! Sky data for each jiggle
      BYTE    SKY_QUAL(MAX__BOL)   ! Sky quality (1)
      DOUBLE PRECISION STDEV       ! Standard deviation
      DOUBLE PRECISION SUM         ! Sum of data
      DOUBLE PRECISION SUMSQ       ! Sum of squares data
      REAL    QSORT(MAX__BOL)      ! Scratch sort array
*    External functions:
      INCLUDE 'NDF_FUNC'
*.


      IF (STATUS .NE. SAI__OK) RETURN

*     Loop over all data
      DO I = 1, N_POS

*     Loop over sky bolometers
         N_GOOD = 0
         DO SKY = 1, N_SKYBOLS
            IF ((SCUDATA(SKYBOLS(SKY), I) .NE. VAL__BADR) .AND.
     :           (NDF_QMASK(SCUQUAL(SKYBOLS(SKY), I), BADBIT))) THEN
               N_GOOD = N_GOOD + 1
               SKY_DATA(N_GOOD) = SCUDATA(SKYBOLS(SKY), I)
               SKY_QUAL(N_GOOD) = 0

            END IF
         END DO

*     Find statistics of SKY data

         CALL SCULIB_STATR(N_GOOD, CLIP, SKY_DATA, SKY_QUAL, BADBIT,
     :        GOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV, QSORT,
     :        STATUS)

*     If there were no good points returned then finishe

         IF (GOOD .GT. 0) THEN

            IF (MODE .EQ. 'MEAN') THEN
               BACKGROUND = SNGL(MEAN)
               IF (GOOD .GT. 0) THEN
                  ERRMEAN = SNGL(STDEV/SQRT(DBLE(GOOD)))
               ELSE
                  ERRMEAN = 0.0
               END IF

               SKYVAR = ERRMEAN * ERRMEAN

               CALL MSG_SETI('BOL', I)
               CALL MSG_SETR('BG', BACKGROUND)
               CALL MSG_SETR('ERR', ERRMEAN)
               CALL MSG_SETI('GOOD', GOOD)
               
               CALL MSG_OUTIF(MSG__VERB, ' ',
     :              '^BOL: MEAN = ^BG +- ^ERR (^GOOD points)', STATUS)
            ELSE
               BACKGROUND = SNGL(MEDIAN)
               
               CALL MSG_SETI('BOL', I)
               CALL MSG_SETR('BG', BACKGROUND)
               
               CALL MSG_OUTIF(MSG__VERB, ' ',
     :              '^BOL: MEDIAN = ^BG', STATUS)
            END IF

*     Remove background value from entire JIGGLE
            IF (BACKGROUND .NE. VAL__BADR) THEN
               DO BOL = 1, N_BOLS
                  IF (SCUDATA(BOL, I) .NE. VAL__BADR) THEN

                     SCUDATA(BOL, I) = SCUDATA(BOL, I) - BACKGROUND
                     
                     IF (MODE .EQ. 'MEAN') THEN
                        SCUVAR(BOL, I) = SCUVAR(BOL, I) + SKYVAR
                     END IF

                  END IF
               END DO
            END IF
         ELSE

            CALL MSG_SETI('POS', I)
            CALL MSG_OUTIF(MSG__VERB, ' ',
     :           '^POS: No good data present', STATUS)

         END IF

      END DO

      END
