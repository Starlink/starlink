      SUBROUTINE SCULIB_CLIP_BOL(N_POS, SCUDATA, SCUQUAL,
     :     N_SIGMA, BADBIT, NSPIKES, STATUS)
*+
*  Name:
*     SCULIB_CLIP_BOL

*  Purpose:
*     To clip a bolometer at +/- nsigma

*  Invocation:
*     CALL SCULIB_CLIP_BOL(N_POS, SCUDATA, SCUQUAL,
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
*        Number of sigma to clip at. If this number is positive
*        then a clipped mean and standard deviation are calculated.
*        If it is negative the statistics are calculated without
*        iterative clipping.
*     BADBIT = BYTE (Given & Returned)
*        Bad bit mask
*     NSPIKES = INTEGER (Returned)
*        Number of spikes that were detected.
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:

*  Authors:
*     TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1996 November 17 (TIMJ):
*       Original version
*     $Log$
*     Revision 1.11  2007/03/07 13:49:11  pdraper
*     Move CNF_PAR declarations out of the executable code.
*
*     Revision 1.10  2004/09/01 00:52:22  timj
*     use CNF_PVAL
*
*     Revision 1.9  1999/08/06 02:24:39  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.8  1999/08/03 19:34:49  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.7  1998/05/21 00:35:32  timj
*     Sigma clipping is now used unless N_SIGMA is negative.
*
*     Revision 1.6  1998/03/04 05:45:39  timj
*     Initialise pointers
*
*     Revision 1.5  1997/11/13 19:14:38  timj
*     Remove unwanted SCULIB_BITOFF declaration.
*
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
      INCLUDE 'CNF_PAR'               ! CNF_PVAL function

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
      BYTE    SCULIB_BITON         ! Turn on badbits

*  Local Constants:
      INTEGER   SKYBIT               ! Sky Quality bit
      PARAMETER (SKYBIT = 4)

*  Local Variables:
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
      SPNTR = 0
      SPNTR_END = 0
      CALL SCULIB_MALLOC(N_POS * VAL__NBR, SPNTR, SPNTR_END, STATUS)

*  Find the statistics (Mean and standard deviation

      CALL SCULIB_STATR(N_POS, REAL(N_SIGMA), SCUDATA, SCUQUAL, BADBIT,
     :     NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV,
     :     %VAL(CNF_PVAL(SPNTR)), STATUS)

*     Free memory

      CALL SCULIB_FREE('CLIPSORT', SPNTR, SPNTR_END, STATUS)

*     Set the number of spikes to 0
      NSPIKES = 0

*  Loop over all data removing those points which are greater than nsigma

      IF (MEAN .NE. VAL__BADD .AND. STDEV.NE.VAL__BADD) THEN

         CLIPMAX = MEAN + (ABS(N_SIGMA) * STDEV)
         CLIPMIN = MEAN - (ABS(N_SIGMA) * STDEV)

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
