      SUBROUTINE SCULIB_REM_SKY(MODE, ADD_BACK, N_BOLS, N_POS, SCUDATA,
     :     SCUVAR, SCUQUAL, CLIP, N_SKYBOLS, SKYBOLS, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_REM_SKY

*  Purpose:
*     To remove sky background from SCUBA data

*  Invocation:
*     CALL SCULIB_REM_SKY(MODE, ADD_BACK, N_BOLS, N_POS, SCUDATA,
*    :     SCUVAR, SCUQUAL, CLIP, N_SKYBOLS, SKYBOLS, BADBIT, STATUS)

*  Description:
*     This routine goes through each jiggle position in turn, finding
*     the sky background level by using the selected SKY bolometers.
*     This level is removed from each bolometer.
*     The background level is determined by the median of the SKY data
*     for each jiggle.
*     If requested, via ADD_BACK, the mean of the level that was
*     removed is added back onto the data so that flux is conserved
*     in the image.

*  Arguments:
*     MODE = CHARACTER * ( * ) (Given)
*        Clip mode
*     ADD_BACK = LOGICAL (Given)
*        Add the mean removed level back onto the final data set. (if TRUE)
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

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995-1999, 2004, 2006 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1996 November 17 (TIMJ):
*       Original version
*     $Log$
*     Revision 1.10  2007/03/07 13:49:11  pdraper
*     Move CNF_PAR declarations out of the executable code.
*
*     Revision 1.9  2006/05/03 22:05:43  timj
*     protect against all data being bad. Minor other fixes for bad data
*
*     Revision 1.8  2004/09/01 00:52:23  timj
*     use CNF_PVAL
*
*     Revision 1.7  1999/08/06 02:24:48  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.6  1999/08/03 19:35:26  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.5  1998/02/16 06:16:30  timj
*     Calculate the sky noise
*
*     Revision 1.4  1997/11/30 23:40:03  timj
*     Write a message even if we are not adding on the constant offset.
*
*     Revision 1.3  1997/10/17 02:21:07  timj
*     Add the mean sky level that was removed, back onto the data
*     after sky removal.
*
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
      INCLUDE 'CNF_PAR'               ! CNF_PVAL function

*  Arguments Given:
      LOGICAL ADD_BACK
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
      DOUBLE PRECISION BACKGROUND  ! Sky background level
      INTEGER BOL                  ! Loop counter
      BYTE    BTEMP                ! Temporary byte
      INTEGER DQ_PTR               ! Dummy quality array
      INTEGER DQ_PTR_END           ! End of dummy quality
      INTEGER I                    ! Loop counter
      INTEGER IERR                 ! For VEC_
      INTEGER GOOD                 ! Number of good data points
      REAL    ERRMEAN              ! Error in mean of SKY_DATA
      REAL    LCLIP                ! Local value for CLIP
      DOUBLE PRECISION MEAN        ! Mean of sky values
      DOUBLE PRECISION MEAN_LEVEL  ! Mean level removed from data
      DOUBLE PRECISION MEDIAN      ! Median of sky values
      INTEGER NERR                 ! For VEC_
      INTEGER NLOOPS               ! Number of times a sky level was removed
      INTEGER N_GOOD               ! Number of good data points in SKY
      INTEGER SKY                  ! Loop counter for SKY bolometers
      REAL    SKYVAR               ! Variance on sky background
      REAL    SKY_DATA(MAX__BOL)   ! Sky data for each jiggle
      INTEGER SKY_PTR_END          ! End of SKY_PTR
      INTEGER SKY_PTR              ! Array of sky values (REAL)
      BYTE    SKY_QUAL(MAX__BOL)   ! Sky quality (1)
      DOUBLE PRECISION STDEV       ! Standard deviation
      DOUBLE PRECISION SUM         ! Sum of data
      DOUBLE PRECISION SUMSQ       ! Sum of squares data
      REAL    QSORT(MAX__BOL)      ! Scratch sort array
      INTEGER QS_PTR               ! Another sort array
      INTEGER QS_PTR_END           ! End of QS_PTR

*    External functions:
      INCLUDE 'NDF_FUNC'
*.


      IF (STATUS .NE. SAI__OK) RETURN

*     Return if no sky bolometers were selected

      IF (N_SKYBOLS .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SCULIB_REM_SKY: No sky bolometers selected',
     :        STATUS)
      END IF

*     Keep track of the levels removed and the number of times
*     this occurred so that we can add the level back on at the end

*     Need to get some memory for this (store as DOUBLE PRECISION)

      SKY_PTR = 0
      SKY_PTR_END = 0
      CALL SCULIB_MALLOC(N_POS * VAL__NBR, SKY_PTR, SKY_PTR_END,
     :     STATUS)

      NLOOPS = 0

*     Loop over all data
      DO I = 1, N_POS

*     Initialisations
         SKYVAR = VAL__BADR

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

*     If there were no good points returned then finish

         IF (GOOD .GT. 0) THEN

            IF (MODE .EQ. 'MEAN') THEN
               BACKGROUND = MEAN
               IF (GOOD .GT. 0) THEN
                  ERRMEAN = SNGL(STDEV/SQRT(DBLE(GOOD)))
               ELSE
                  ERRMEAN = 0.0
               END IF

               SKYVAR = ERRMEAN * ERRMEAN

               CALL MSG_SETI('POS', I)
               CALL MSG_SETD('BG', BACKGROUND)
               CALL MSG_SETR('ERR', ERRMEAN)
               CALL MSG_SETI('GOOD', GOOD)

               CALL MSG_OUTIF(MSG__VERB, ' ',
     :              '^POS: MEAN = ^BG +- ^ERR (^GOOD points)', STATUS)
            ELSE
               BACKGROUND = MEDIAN

               CALL MSG_SETI('POS', I)
               CALL MSG_SETD('BG', BACKGROUND)

               CALL MSG_OUTIF(MSG__VERB, ' ',
     :              '^POS: MEDIAN = ^BG', STATUS)
            END IF

*     Remove background value from entire JIGGLE
            IF (BACKGROUND .NE. VAL__BADD) THEN
               DO BOL = 1, N_BOLS
                  IF (SCUDATA(BOL, I) .NE. VAL__BADR) THEN

                     SCUDATA(BOL, I) =SCUDATA(BOL, I) - SNGL(BACKGROUND)

                     IF (MODE .EQ. 'MEAN') THEN
                        SCUVAR(BOL, I) = SCUVAR(BOL, I) + SKYVAR
                     END IF

                  END IF
               END DO

*     Store the level
               CALL VEC_DTOR(.FALSE., 1, BACKGROUND,
     :              %VAL(CNF_PVAL(SKY_PTR) + (NLOOPS * VAL__NBR)), IERR,
     :              NERR, STATUS)
               NLOOPS = NLOOPS + 1

            END IF
         ELSE

            CALL MSG_SETI('POS', I)
            CALL MSG_OUTIF(MSG__VERB, ' ',
     :           '^POS: No good data present', STATUS)

         END IF

      END DO

*     Calculate the statistics of the sky noise

*     Set up the scratch arrays
      DQ_PTR = 0
      DQ_PTR_END = 0
      QS_PTR = 0
      QS_PTR_END = 0

*     See if we had any data at all in the file
      STDEV = VAL__BADD
      MEAN_LEVEL = VAL__BADD
      SUM = VAL__BADD
      SUMSQ = VAL__BADD
      IF (NLOOPS .GT. 0) THEN

         CALL SCULIB_MALLOC(NLOOPS * VAL__NBUB, DQ_PTR, DQ_PTR_END,
     :        STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            BTEMP = 0
            CALL SCULIB_CFILLB(NLOOPS, BTEMP, %VAL(CNF_PVAL(DQ_PTR)))
         END IF
         CALL SCULIB_MALLOC(NLOOPS * VAL__NBR, QS_PTR, QS_PTR_END,
     :        STATUS)
         LCLIP = -1.0

         CALL SCULIB_STATR(NLOOPS, LCLIP, %VAL(CNF_PVAL(SKY_PTR)),
     :        %VAL(CNF_PVAL(DQ_PTR)),
     :        BADBIT, GOOD, MEAN_LEVEL, MEDIAN, SUM, SUMSQ, STDEV,
     :        %VAL(CNF_PVAL(QS_PTR)), STATUS)


*     Free the memory for statistics
         CALL SCULIB_FREE('DUMMY_SQUAL', DQ_PTR, DQ_PTR_END, STATUS)
         CALL SCULIB_FREE('SKY_SORTED', QS_PTR, QS_PTR_END, STATUS)

      END IF

*     Free main array
      CALL SCULIB_FREE('SKY_ARRAY', SKY_PTR, SKY_PTR_END, STATUS)

*     Print out the sky noise value
      CALL MSG_SETI('NGOOD', GOOD)
      IF (STDEV .NE. VAL__BADD) THEN
         CALL MSG_SETR('NOISE',SNGL(STDEV))
      ELSE
         CALL MSG_SETC('NOISE', 'BAD')
      END IF
      CALL MSG_OUTIF(MSG__NORM, ' ','Sky noise: ^NOISE (^NGOOD points)',
     :     STATUS)


*     Now add the background level back on if requested

      IF (ADD_BACK .AND. (NLOOPS .GT. 0) .AND. STATUS .EQ. SAI__OK
     :     .AND. MEAN_LEVEL .NE. VAL__BADD) THEN

*     Add the mean back on (ignore the slight change in variance associated
*     with the fact that the MEAN has an error on it)

         CALL SCULIB_ADDCAR(N_POS * N_BOLS, SCUDATA,
     :        SNGL(MEAN_LEVEL), SCUDATA)

*     and print a message in verbose mode

         CALL MSG_SETR('BAC', SNGL(MEAN_LEVEL))

         CALL MSG_OUTIF(MSG__NORM, ' ', 'Adding mean background level'//
     :        ' back onto data (value=^BAC)', STATUS)

      ELSE

         CALL MSG_OUTIF(MSG__NORM, ' ', 'Not adding back average sky '//
     :        'level', STATUS)

      END IF


      END
