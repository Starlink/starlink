*+  SCULIB_COPY_DEMOD_SWITCH - copy a switch of demodulated data into
*                              arrays holding the switch chop data, chop 
*                              variance, calibrator, cal variance and quality
*                              separately
      SUBROUTINE SCULIB_COPY_DEMOD_SWITCH (NBOLS, NJIG,
     :  SWITCH, SWITCH_DATA, SWITCH_VARIANCE, SWITCH_CALIBRATOR,
     :  SWITCH_CAL_VARIANCE, SWITCH_QUALITY, STATUS)
*    Description :
*     This routine copies the demodulated data for the jiggle positions
*     measured in the last switch into separate arrays for the switch
*     chop data, chop variance, calibrator, cal variance and quality.
*    Invocation :
*     CALL SCULIB_COPY_DEMOD_SWITCH (NBOLS, NJIG,
*    :  SWITCH, SWITCH_DATA, SWITCH_VARIANCE, SWITCH_CALIBRATOR,
*    :  SWITCH_CAL_VARIANCE, SWITCH_QUALITY, STATUS)
*    Parameters :
*     NBOLS                           = INTEGER (Given)
*           number of bolometers being measured
*     NJIG                            = INTEGER (Given)
*           number of jiggle positions measured
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
*     SWITCH_QUALITY (NBOLS, NJIG)    = INTEGER (Returned)
*           quality for this switch of the exposure
*     STATUS                          = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     5-AUG-1993: Original version.
*     9-JUL-1996: Modified to handle v200 demodulated data with 5 data
*                 per measured point (JFL).
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER NBOLS, NJIG
      REAL SWITCH (5, NBOLS, NJIG)
*    Inport-Export :
*    Export :
      REAL SWITCH_DATA (NBOLS, NJIG)
      REAL SWITCH_VARIANCE (NBOLS, NJIG)
      REAL SWITCH_CALIBRATOR (NBOLS, NJIG)
      REAL SWITCH_CAL_VARIANCE (NBOLS, NJIG)
      INTEGER SWITCH_QUALITY (NBOLS, NJIG)
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER J 
      INTEGER BOL
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  cycle through jiggle points measured

      DO J = 1, NJIG

*  cycle through bolometers measured

         DO BOL = 1, NBOLS

            SWITCH_DATA (BOL, J) = SWITCH (1, BOL, J)
            SWITCH_VARIANCE (BOL, J) = SWITCH (2, BOL, J)
            SWITCH_CALIBRATOR (BOL, J) = SWITCH (3, BOL, J)
            SWITCH_CAL_VARIANCE (BOL, J) = SWITCH (4, BOL, J)
            SWITCH_QUALITY (BOL, J) = NINT (SWITCH (5, BOL, J))

         END DO
      END DO

      END
