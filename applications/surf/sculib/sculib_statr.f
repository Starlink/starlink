      SUBROUTINE SCULIB_STATR(N_POS, CLIP, SCUDATA, SCUQUAL, BADBIT,
     :     NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV, QSORT, STATUS)
*+
*  Name:
*     SCULIB_STATR

*  Purpose:
*     To calculate mean and standard deviation of a REAL array

*  Invocation:
*     CALL SCULIB_STATR(N_POS, SCUDATA, SCUQUAL, BADBIT,
*    :     NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV, QSORT, STATUS)

*  Description:
*     This routine calculates the mean and standard deviation of
*     a real array. If a positive value of CLIP is given, the data
*     set will be clipped at CLIP sigma repeatedly until all
*     points are within CLIP sigma. Mean and standard deviation are those
*     values derived after iterative clipping.

*  Arguments:
*     N_POS = _INTEGER (Given)
*        Number of jiggle positions in data set
*     CLIP = _REAL (Given)
*        Number of sigma to clip iteratively
*     SCUDATA(N_POS) = _REAL (Given)
*        The data
*     SCUQUAL(N_POS) = _BYTE (Given)
*        The data quality
*     BADBIT = _BYTE (Given)
*        Bad bit mask
*     NGOOD = _INTEGER (Returned)
*        Number of good pixels in mean
*     MEAN = _DOUBLE (Returned)
*        Mean value
*     MEDIAN = _DOUBLE (Returned)
*        Median of good data
*     SUM = _DOUBLE (Returned)
*        Data sum
*     SUMSQ = _DOUBLE (Returned)
*        Sum of squares
*     STDEV = _DOUBLE (Returned)
*        Standard deviation
*     QSORT = _REAL (Returned)
*        Sorted data set
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:
*     - Deals with bad pixels
*     - Uses PDA sort routine

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1996 November 18 (TIMJ):
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

*  Arguments Given:
      INTEGER N_POS
      REAL    CLIP
      BYTE    BADBIT
      REAL    SCUDATA(N_POS)
      BYTE    SCUQUAL(N_POS)

*  Arguments Returned:
      REAL   QSORT(N_POS)
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION MEDIAN
      INTEGER NGOOD
      DOUBLE PRECISION STDEV
      DOUBLE PRECISION SUM
      DOUBLE PRECISION SUMSQ

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      LOGICAL CLIPPING             ! Are we still clipping?
      INTEGER I                    ! Loop counter
      REAL    LLIMIT               ! Lower limit of clip
      INTEGER MIDPOINT             ! Middle of Good data
      INTEGER NKEPT                ! Number of good points per iteration
      INTEGER NREM                 ! Number of points clipped per iteration
      REAL    ULIMIT               ! Upper limit of clip
      DOUBLE PRECISION VALUE       ! Value of data point

*    External functions:
      INCLUDE 'NDF_FUNC'
*.

      IF (STATUS .NE. SAI__OK) RETURN

      NGOOD = 0

*     Loop over all data and remove bad points
      DO I = 1, N_POS
         IF ((SCUDATA(I) .NE. VAL__BADR) .AND.
     :        (NDF_QMASK(SCUQUAL(I), BADBIT))) THEN
            NGOOD = NGOOD + 1
            QSORT(NGOOD) = SCUDATA(I)
         END IF
      END DO


* Set defaults
      MEDIAN = VAL__BADD
      MEAN   = VAL__BADD
      CLIPPING = .TRUE.


*     Find Standard deviation and mean and median if there is some good data
      IF (NGOOD .GT. 0) THEN

*     Sort good data into ascending order (with PDA routine)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PDA_QSAR(NGOOD, QSORT)
         END IF

*     Median
         MIDPOINT = NGOOD / 2

         IF (MOD(NGOOD, 2) .EQ. 0) THEN    ! Even
            MEDIAN = DBLE(QSORT(MIDPOINT) + QSORT(MIDPOINT+1))/2.0D0
         ELSE    ! Odd
            MEDIAN = DBLE(QSORT(MIDPOINT+1))
         END IF

*     Loop until CLIP okay
         DO WHILE (CLIPPING)

*     Go through all data, initialise values
            NKEPT = 0
            SUM = 0.0D0
            SUMSQ = 0.0D0
            STDEV = VAL__BADD

            DO I = 1, NGOOD
               IF (QSORT(I) .NE. VAL__BADR) THEN
                  VALUE = DBLE(QSORT(I))
                  SUM = SUM + VALUE
                  SUMSQ = SUMSQ + ( VALUE * VALUE )
                  NKEPT = NKEPT + 1
               END IF
            END DO

*     Mean and STDEV

            IF (NKEPT .GT. 0) THEN

               MEAN = SUM / DBLE( NKEPT )
               STDEV = SUMSQ - ( MEAN * MEAN * DBLE( NKEPT ) )
               IF ( ( NKEPT .EQ. 1 ) .OR.
     :              ( STDEV .LT. 0.0D0 ) ) THEN
                  STDEV = 0.0D0

*     Otherwise, calculate the standard deviation normally.
               ELSE
                  STDEV = SQRT( STDEV / DBLE( NKEPT - 1 ) )
               END IF

*     Now loop through and clip points above CLIP level
               IF (CLIP.GT.0.0 .AND. MEAN.NE.VAL__BADD .AND.
     :              STDEV.GT.0.0D0) THEN

                  NREM = 0
                  ULIMIT =  REAL(MEAN + (DBLE(CLIP) * STDEV))
                  LLIMIT =  REAL(MEAN - (DBLE(CLIP) * STDEV))

                  DO I = 1, NGOOD
                     IF (QSORT(I) .NE. VAL__BADR .AND.
     :                    (QSORT(I) .GT. ULIMIT .OR.
     :                    QSORT(I) .LT. LLIMIT )) THEN
                        QSORT(I) = VAL__BADR
                        NREM = NREM + 1 ! Number removed
                     END IF
                  END DO
*     Stop clipping if didnt remove any points
                  IF (NREM.EQ.0) CLIPPING = .FALSE.

               ELSE
                  CLIPPING = .FALSE.
               END IF
            ELSE
               CLIPPING = .FALSE.
            END IF

         END DO

         NGOOD = NKEPT   ! Update NGOOD

      END IF

      END
