      SUBROUTINE SCULIB_3POS_DECONV (N_EXPOSURES, N_INTEGRATIONS,
     :  N_MEASUREMENTS, DEMOD_POINTER, N_BOL, N_POS, IN_DATA,
     :  IN_VARIANCE, IN_QUALITY, SAMPLE_DX, BEAM_SEP,
     :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, BADBIT, STATUS)
*+
*  Name:
*     SCULIB_3POS_DECONV

*  Purpose:
*     deconvolve 3-position chop from scan

*  Description:
*     This routine deconvolves the 3 position chopped beam response from the
*     individual scans of a SCUBA raster map.
*        To achieve this it cycles through the scans making up the
*     observation, calling SCULIB_FIND_SWITCH to locate the start and
*     finish indices of each scan in the demodulated data array. If
*     SCULIB_FIND_SWITCH indicates that there is no data for this scan,
*     which may happen if the observation was aborted, then no further
*     action is taken. If a scan is too long to be handled by the routine
*     an error message will be output and the routine will return with
*     bad status.
*        Otherwise, SCULIB_GENSYCONFN and SCULIB_3POS_CONFN will be
*     called to generate the convolution functions needed to deconvolve
*     the chop. The routine then cycles through the bolometers, calling
*     SCULIB_CONVOLVE to do the required convolutions with the scan data
*     for each.

*  Invocation:
*     CALL SCULIB_3POS_DECONV (N_EXPOSURES, N_INTEGRATIONS,
*    :  N_MEASUREMENTS, DEMOD_POINTER, N_BOL, N_POS, IN_DATA,
*    :  IN_VARIANCE, IN_QUALITY, SAMPLE_DX, BEAM_SEP,
*    :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, STATUS)

*  Arguments:
*     N_EXPOSURES                 = INTEGER (Given)
*           maximum number of exposures per integration
*     N_INTEGRATIONS              = INTEGER (Given)
*           number of integrations in the observation
*     N_MEASUREMENTS              = INTEGER (Given)
*           number of measurements in the observation
*     DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS)
*           array pointing to start and finish of scans in IN_DATA
*     N_BOL                       = INTEGER (Given)
*           the number of bolometers for which data was taken
*     N_POS                       = INTEGER (Given)
*           the number of positions measured in the scan
*     IN_DATA (N_BOL, N_POS)      = REAL (Given)
*           the measured data
*     IN_VARIANCE (N_BOL, N_POS)  = REAL (Given)
*           the variance on IN_DATA
*     IN_QUALITY (N_BOL, N_POS)   = BYTE (Given)
*           the quality on IN_DATA
*     SAMPLE_DX                   = REAL (Given)
*           the measurement spacing along the scan (arcseconds)
*     BEAM_SEP                    = REAL (Given)
*           the beam separation (arcseconds)
*     OUT_DATA (N_BOL, N_POS)     = REAL (Returned)
*           the deconvolved data
*     OUT_VARIANCE (N_BOL, N_POS) = REAL (Returned)
*           the variance on OUT_DATA
*     OUT_QUALITY (N_BOL, N_POS)  = BYTE (Returned)
*           the quality on OUT_DATA
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     7-OCT-1995: original version
*    endhistory

*  Method:

*  Deficiencies:

*  Bugs:


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                        ! for VAL__BADI
      INCLUDE 'MSG_PAR'                        ! MSG__ constants

*  Arguments Given:
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEMOD_POINTER (N_EXPOSURES, N_INTEGRATIONS,
     :  N_MEASUREMENTS)
      INTEGER N_BOL
      INTEGER N_POS
      REAL    IN_DATA (N_BOL, N_POS)
      REAL    IN_VARIANCE (N_BOL, N_POS)
      BYTE IN_QUALITY (N_BOL, N_POS)
      REAL    SAMPLE_DX
      REAL    BEAM_SEP
      BYTE    BADBIT

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    OUT_DATA (N_BOL, N_POS)
      REAL    OUT_VARIANCE (N_BOL, N_POS)
      BYTE OUT_QUALITY (N_BOL, N_POS)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER MAX_CONV                         ! the maximum size of the
      PARAMETER (MAX_CONV = 4001)              ! convolution functions
      INTEGER MAX_SCAN                         ! the maximum length of a scan
      PARAMETER (MAX_SCAN = 2000)

*  Local variables:
      REAL    AS_CONV_FUNCTION (MAX_CONV)      ! the `asymmetric' convolution
                                               ! function
      INTEGER BOL                              ! bolometer index
      INTEGER EXPOSURE                         ! exposure number of scan
      INTEGER INTEGRATION                      ! integration number of scan
      INTEGER MEASUREMENT                      ! measurement number of scan
      REAL    NORM                             ! normalisation factor
      INTEGER N_CONV                           ! number of points in convolution
                                               ! function
      INTEGER N_SCAN                           ! number of points in scan
      INTEGER POS                              ! index of datum in scan
      INTEGER SCAN_END                         ! index of end of scan in
                                               ! demodulated data array
      INTEGER SCAN_START                       ! index of start of scan in
                                               ! demodulated data array
      REAL    SCRATCH1_DATA (MAX_SCAN)         ! scratch data
      BYTE    SCRATCH1_QUALITY (MAX_SCAN)      ! scratch variance
      REAL    SCRATCH1_VARIANCE (MAX_SCAN)     ! scratch quality
      REAL    SCRATCH2_DATA (MAX_SCAN)         ! scratch data
      BYTE    SCRATCH2_QUALITY (MAX_SCAN)      ! scratch variance
      REAL    SCRATCH2_VARIANCE (MAX_SCAN)     ! scratch quality
      REAL    SY_CONV_FUNCTION (MAX_CONV)      ! the `symmetric' convolution
                                               ! function

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  cycle through the scans

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS
            DO EXPOSURE = 1, N_EXPOSURES

               CALL SCULIB_FIND_SWITCH (DEMOD_POINTER, 1,
     :           N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :           1, EXPOSURE, INTEGRATION, MEASUREMENT, SCAN_START,
     :           SCAN_END, STATUS)

               IF ((SCAN_START .EQ. VAL__BADI) .OR.
     :             (SCAN_START .EQ. 0))        THEN
                  CALL MSG_SETI ('E', EXPOSURE)
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUTIF (MSG__NORM, ' ',
     :                 'SCULIB: no data for exp ^E '//
     :                 'in int ^I, meas ^M', STATUS)
               ELSE

*  OK, there is some data for this scan

                  CALL MSG_SETI('NI', INTEGRATION)
                  CALL MSG_SETI('NE', EXPOSURE)

                  CALL MSG_OUTIF(MSG__NORM, ' ',
     :                 '3POS_DECONV: Processing exposure ^NE'//
     :                 ' of integration ^NI', STATUS)


                  N_SCAN = SCAN_END - SCAN_START + 1

                  IF (2*N_SCAN + 1 .GT. MAX_CONV) THEN
                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('LEN', N_SCAN)
                        CALL MSG_SETI ('MAX', (MAX_CONV - 1) / 2)
                        CALL ERR_REP (' ', 'SCULIB_3POS_DECONV: '//
     :                    'scan longer (^LEN) than maximum allowed '//
     :                    '(^MAX)', STATUS)
                     END IF
                  END IF

*  the data should contain NO information at the spatial frequency of the
*  half-chop throw (distance between -ve spike and central spike of chop)
*  or its harmonics. Any signal there must be noise. This can
*  be removed in the Fourier domain by multiplying the signal by a function
*  with zeroes at the frequencies of zero responsivity and 1 everywhere else.
*  The same effect is achieved by convolving the inverse FT of this function
*  with the scan data. This is NOD2's `symmetric function'. SCULIB_GENSYCONFN
*  generates the required function.

                  NORM = 1.0

                  CALL SCULIB_GENSYCONFN (BEAM_SEP, SAMPLE_DX, N_SCAN,
     :              N_CONV, SY_CONV_FUNCTION, STATUS)

*  The deconvolution of the chop-beam could be managed by dividing the FT
*  of the map by the FT of the beam chop, then calculating the inverse
*  FT to give the desired result. The equivalent is achieved by convolving
*  the map data with the function whose FT is the inverse of that of the
*  beam chop. This is the `asymmetric function' used in NOD2 and
*  SCULIB_3POS_CONFN calculates it for the 3-position chop.

                  CALL SCULIB_3POS_CONFN (BEAM_SEP, SAMPLE_DX, N_SCAN,
     :              N_CONV, AS_CONV_FUNCTION, STATUS)

*  now cycle through the bolometers

                  DO BOL = 1, N_BOL

*  copy the scan data from the input array to the input scratch array

                     IF (STATUS .EQ. SAI__OK) THEN
                        DO POS = SCAN_START, SCAN_END
                           SCRATCH1_DATA (POS - SCAN_START + 1) =
     :                       IN_DATA (BOL,POS)
                           SCRATCH1_VARIANCE (POS - SCAN_START + 1) =
     :                       IN_VARIANCE (BOL,POS)
                           SCRATCH1_QUALITY (POS - SCAN_START + 1) =
     :                       IN_QUALITY (BOL,POS)
                        END DO
                     END IF

*  do the convolutions

                     CALL SCULIB_CONVOLVE (SCRATCH1_DATA,
     :                 SCRATCH1_VARIANCE, SCRATCH1_QUALITY,
     :                 SY_CONV_FUNCTION, N_SCAN, N_CONV, N_SCAN, NORM,
     :                 SCRATCH2_DATA, SCRATCH2_VARIANCE,
     :                 SCRATCH2_QUALITY, BADBIT, STATUS)

                     CALL SCULIB_CONVOLVE (SCRATCH2_DATA,
     :                 SCRATCH2_VARIANCE, SCRATCH2_QUALITY,
     :                 AS_CONV_FUNCTION, N_SCAN, N_CONV, N_SCAN, NORM,
     :                 SCRATCH1_DATA, SCRATCH1_VARIANCE,
     :                 SCRATCH1_QUALITY, BADBIT, STATUS)

*  copy the convolved data from the output scratch array to the appropriate
*  position in the output array

                     IF (STATUS .EQ. SAI__OK) THEN
                        DO POS = SCAN_START, SCAN_END
                           OUT_DATA (BOL,POS) =
     :                       SCRATCH1_DATA (POS - SCAN_START + 1)
                           OUT_VARIANCE (BOL,POS) =
     :                       SCRATCH1_VARIANCE (POS - SCAN_START + 1)
                           OUT_QUALITY (BOL,POS) =
     :                       SCRATCH1_QUALITY (POS - SCAN_START + 1)
                        END DO
                     END IF

                  END DO

               END IF

            END DO
         END DO
      END DO

      END
