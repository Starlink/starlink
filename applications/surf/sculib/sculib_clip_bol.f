      SUBROUTINE SCULIB_CLIP_BOL(N_POS, SCUDATA, SCUQUAL,
     :     N_SIGMA, BADBIT, NSPIKES, STATUS)
*+
*  Name:
*     SCULIB_CLIP_BOL

*  Purpose:
*     To clip a bolometer at +/- nsigma

*  Invocation:
*     SUBROUTINE SCULIB_CLIP_BOL(N_POS, SCUDATA, SCUQUAL,
*    :     N_SIGMA, BADBIT, NSPIKES, STATUS)

*  Description:
*     This routine despikes a single bolometer at the +/- n sigma
*     level. A 1-dimensional data set is assumed.

*  Arguments:
*     N_POS = INTEGER (Given)
*        Number of jiggle positions in data set
*     SCUDATA(N_POS) = REAL (Given)
*        The data
*     SCUQUAL(N_POS) = BYTE (Given & Returned)
*        The data quality
*     N_SIGMA = DOUBLE (Given)
*        Number of sigma to clip at
*     BADBIT = BYTE (Given & Returned)
*        Bad bit mask
*     NSPIKES = INTEGER (Returned)
*        Number of spikes that were detected.
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     1996 November 17 (TIMJ):
*       Original version
*     $Log$
*     Revision 1.4  1997/10/17 02:25:47  timj
*     Dont reset the despiking quality bit during the despiking. (ie
*     allow existing clips to remain valid).
*
*     Revision 1.3  1997/10/17 02:19:34  timj
*     Return the actual number of spikes that were removed.
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

*  Arguments Given:
      INTEGER N_POS
      DOUBLE PRECISION N_SIGMA

*  Arguments Given and Returned:
      BYTE    BADBIT
      REAL    SCUDATA(N_POS)
      BYTE    SCUQUAL(N_POS)

*  Arguments returned:
      INTEGER NSPIKES

*  Status:
      INTEGER STATUS                 ! Global status

*  External references:
      BYTE    SCULIB_BITOFF        ! Turn off badbits
      BYTE    SCULIB_BITON         ! Turn on badbits

*  Local Constants:
      INTEGER   SKYBIT               ! Sky Quality bit
      PARAMETER (SKYBIT = 4)  

*  Local Variables:
      REAL    CLIP                 ! Number of sigma to clip at
      DOUBLE PRECISION CLIPMAX     ! Max clipping level
      DOUBLE PRECISION CLIPMIN     ! Min clipping level
      INTEGER I                    ! Loop counter
      DOUBLE PRECISION MEAN        ! Mean of data
      DOUBLE PRECISION MEDIAN      ! Median of data
      INTEGER NGOOD                ! Number of good data points
      INTEGER SPNTR                ! Pointer to scratch data
      INTEGER SPNTR_END            ! Pointer to end of scratch data
      DOUBLE PRECISION STDEV       ! Standard deviation
      DOUBLE PRECISION SUM         ! Sum
      DOUBLE PRECISION SUMSQ       ! Sum squares

*    External functions:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Generate some scratch memory for sort
      CALL SCULIB_MALLOC(N_POS * VAL__NBR, SPNTR, SPNTR_END, STATUS)

*  Find the statistics (Mean and standard deviation

      CLIP = -1.0

      CALL SCULIB_STATR(N_POS, CLIP, SCUDATA, SCUQUAL, BADBIT, NGOOD,
     :     MEAN, MEDIAN, SUM, SUMSQ, STDEV, %val(SPNTR), STATUS)

*     Free memory

      CALL SCULIB_FREE('CLIPSORT', SPNTR, SPNTR_END, STATUS)

*     Set the number of spikes to 0
      NSPIKES = 0
      
*  Loop over all data removing those points which are greater than nsigma

      IF (MEAN .NE. VAL__BADD .AND. STDEV.NE.VAL__BADD) THEN

         CLIPMAX = MEAN + (N_SIGMA * STDEV)
         CLIPMIN = MEAN - (N_SIGMA * STDEV)

         DO I = 1, N_POS

            IF ((SCUDATA(I) .NE. VAL__BADR) .AND.
     :           (NDF_QMASK(SCUQUAL(I), BADBIT))) THEN

*     We have found a spike
               IF (SCUDATA(I) .GT. REAL(CLIPMAX) .OR.
     :              SCUDATA(I) .LT. REAL(CLIPMIN)) THEN

                  SCUQUAL(I) = SCULIB_BITON(SCUQUAL(I), SKYBIT)

                  NSPIKES = NSPIKES + 1

               END IF
            END IF
         END DO

*  Turn on SKY bad bit 
      BADBIT = SCULIB_BITON(BADBIT, SKYBIT)


      END IF

      END
