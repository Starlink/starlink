      SUBROUTINE SCULIB_COPY_DEMOD_SWITCH (NBOLS, NDATA, NJIG, LEVEL,
     :  SWITCH, SWITCH_DATA, SWITCH_VARIANCE, SWITCH_CALIBRATOR,
     :  SWITCH_CAL_VARIANCE, SWITCH_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_COPY_DEMOD_SWITCH

*  Purpose:
*     copy a switch of demodulated data into
*     arrays holding the switch chop data, chop
*     variance, calibrator, cal variance and quality
*     separately

*  Description:
*     This routine copies the demodulated data for the jiggle positions
*     measured in the last switch into separate arrays for the switch
*     chop data, chop variance, calibrator, cal variance and quality.
*     Quality is marked bad if BIT 1 is set and LEVEL is greater than requested

*  Invocation:
*     CALL SCULIB_COPY_DEMOD_SWITCH (NBOLS, NDATA, NJIG,
*    :  SWITCH, SWITCH_DATA, SWITCH_VARIANCE, SWITCH_CALIBRATOR,
*    :  SWITCH_CAL_VARIANCE, SWITCH_QUALITY, STATUS)

*  Arguments:
*     NBOLS                           = INTEGER (Given)
*           number of bolometers being measured
*     NDATA                           = INTEGER (Given)
*           number of data per measured position; 5 for jiggle data, 4 for
*           raster
*     NJIG                            = INTEGER (Given)
*           number of jiggle positions measured
*     LEVEL                           = INTEGER (Given)
*           level at which spikes are marked as bad quality
*     SWITCH (5, NBOLS, NJIG)         = REAL (Given)
*           switch datablock
*     SWITCH_DATA (NBOLS, NJIG)       = REAL (Returned)
*           chop data for this switch of the exposure
*     SWITCH_VARIANCE (NBOLS, NJIG)   = REAL (Returned)
*           variance on the chop signal
*     SWITCH_CALIBRATOR (NBOLS, NJIG) = REAL (Returned)
*           calibrator for this switch of the exposure
*     SWITCH_CAL_VARIANCE (NBOLS, NJIG)= REAL (Returned)
*           variance on the calibrator signal
*     SWITCH_QUALITY (NBOLS, NJIG)    = BYTE (Returned)
*           quality for this switch of the exposure
*     STATUS                          = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     5-AUG-1993: Original version.
*     9-JUL-1996: Modified to handle v200 demodulated data with 5 data
*                 per measured point (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NBOLS, NDATA, NJIG, LEVEL
      REAL SWITCH (NDATA, NBOLS, NJIG)

*  Inport-Export:

*  Arguments Returned:
      REAL SWITCH_DATA (NBOLS, NJIG)
      REAL SWITCH_VARIANCE (NBOLS, NJIG)
      REAL SWITCH_CALIBRATOR (NBOLS, NJIG)
      REAL SWITCH_CAL_VARIANCE (NBOLS, NJIG)
      BYTE SWITCH_QUALITY (NBOLS, NJIG)

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITON

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER J
      INTEGER BOL
      INTEGER NQUAL      ! Integer quality

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  cycle through jiggle points measured

      DO J = 1, NJIG

*  cycle through bolometers measured

         DO BOL = 1, NBOLS

            SWITCH_DATA (BOL, J) = SWITCH (1, BOL, J)
            SWITCH_VARIANCE (BOL, J) = SWITCH (2, BOL, J)
            SWITCH_CALIBRATOR (BOL, J) = SWITCH (3, BOL, J)

            IF (NDATA .EQ. 4) THEN
               SWITCH_CAL_VARIANCE (BOL, J) = 0.0
               NQUAL = NINT (SWITCH(4, BOL, J))
            ELSE
               SWITCH_CAL_VARIANCE (BOL, J) = SWITCH (4, BOL, J)
               NQUAL = NINT (SWITCH(5, BOL, J))
            END IF

*  check the quality

            SWITCH_QUALITY(BOL,J) = 0

            IF (NQUAL .GT. 0) THEN
               IF (MOD(NQUAL, 2) .EQ. 1)  THEN   ! ODD
                  SWITCH_QUALITY(BOL,J) =
     :                 SCULIB_BITON(SWITCH_QUALITY(BOL,J), 1)
                  NQUAL = NQUAL - 1
               ENDIF
               IF (NQUAL .GT. LEVEL) THEN
                  SWITCH_QUALITY(BOL,J) =
     :                 SCULIB_BITON(SWITCH_QUALITY(BOL,J), 2)
               ENDIF
            ENDIF

         END DO
      END DO

      END
