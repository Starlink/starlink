      SUBROUTINE SCULIB_GET_SUB_BOLS (N_BOL_IN, N_POS, N_BEAM,
     :  IN_DATA, IN_VARIANCE, IN_QUALITY, N_BOL_OUT, IN_POINTER,
     :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_GET_SUB_BOLS

*  Purpose:
*     copy bolometers belonging to a particular sub-
*     instrument from the input data array to the output

*  Description:
*     This routine extracts data for bolometers belonging to a particular
*     sub-instrument from an input data array which may contain data for
*     several sub-instruments. The input array IN_POINTER points to the
*     indices in the first dimension of the input data array that contain
*     the data of interest. IN_POINTER should have been calculated by an
*     earlier call to SCULIB_CALC_SUB_BOLS.

*  Invocation:
*     CALL SCULIB_GET_SUB_BOLS (N_BOL_IN, N_POS, N_BEAM, IN_DATA,
*    :  IN_VARIANCE, IN_QUALITY, N_BOL_OUT, IN_POINTER, OUT_DATA,
*    :  OUT_VARIANCE, OUT_QUALITY, STATUS)

*  Arguments:
*     N_BOL_IN                     = INTEGER (Given)
*           number of bolometers in input array
*     N_POS                        = INTEGER (Given)
*           number of positions measured in input array
*     N_BEAM                       = INTEGER (Given)
*           the number of beams in the input array
*     IN_DATA (N_BOL_IN,N_POS,N_BEAM)
*                                  = REAL (Given)
*           input data array
*     IN_VARIANCE (N_BOL_IN,N_POS,N_BEAM)
*                                  = REAL (Given)
*           variance on IN_DATA
*     IN_QUALITY (N_BOL_IN,N_POS,N_BEAM)
*                                  = BYTE (Given)
*           quality on IN_DATA
*     N_BOL_OUT                    = INTEGER (Given)
*           number of bolometers in output array
*     IN_POINTER (N_BOL_OUT)       = INTEGER (Given)
*           pointers from bolometers in output array to their indices in input
*     OUT_DATA (N_BOL_OUT,N_POS,N_BEAM)
*                                  = REAL (Returned)
*           output data array
*     OUT_VARIANCE (N_BOL_OUT,N_POS,N_BEAM)
*                                  = REAL (Returned)
*           variance on OUT_DATA
*     OUT_QUALITY (N_BOL_OUT,N_POS,N_BEAM)
*                                  = BYTE (Returned)
*           quality on OUT_DATA
*     STATUS                       = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     3-AUG-1995: original version
*    10-JUN-1996: added N_BEAM dimension to arrays (JFL)
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_BOL_IN
      INTEGER N_POS
      INTEGER N_BEAM
      REAL    IN_DATA (N_BOL_IN, N_POS, N_BEAM)
      REAL    IN_VARIANCE (N_BOL_IN, N_POS, N_BEAM)
      BYTE IN_QUALITY (N_BOL_IN, N_POS, N_BEAM)
      INTEGER N_BOL_OUT
      INTEGER IN_POINTER (N_BOL_OUT)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    OUT_DATA (N_BOL_OUT, N_POS, N_BEAM)
      REAL    OUT_VARIANCE (N_BOL_OUT, N_POS, N_BEAM)
      BYTE OUT_QUALITY (N_BOL_OUT, N_POS, N_BEAM)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER      BEAM                    ! beam index in DO loop
      INTEGER      OUT_BOL                 ! bolometer index in output array
      INTEGER      POS                     ! position index in DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  copy over the required data

      IF (N_POS .GT. 0) THEN

	 DO BEAM = 1, N_BEAM
            DO POS = 1, N_POS
               DO OUT_BOL = 1, N_BOL_OUT
                  OUT_DATA (OUT_BOL,POS,BEAM) =
     :              IN_DATA (IN_POINTER(OUT_BOL),POS,BEAM)
                  OUT_VARIANCE (OUT_BOL,POS,BEAM) =
     :              IN_VARIANCE (IN_POINTER(OUT_BOL),POS,BEAM)
                  OUT_QUALITY (OUT_BOL,POS,BEAM) =
     :              IN_QUALITY (IN_POINTER(OUT_BOL),POS,BEAM)
               END DO
            END DO
         END DO

      END IF

      END
