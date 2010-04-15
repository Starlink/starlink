      SUBROUTINE SCULIB_APPARENT_2_TP (N_POS, BOL_XPOS, BOL_YPOS,
     :  RA_CEN, DEC_CEN, ROTATION, SHIFT_DX, SHIFT_DY, STATUS)
*+
*  Name:
*     SCULIB_APPARENT_2_TP

*  Purpose:
*     calculate tangent plane coordinates from apparent
*     RA, Decs

*  Description:
*     This routine converts a list of apparent RA,Decs to tangent plane
*     offsets from a tangent point whose position is also given in apparent
*     RA,Dec. In addition, a rotation is applied to the tangent plane so that
*     it can be aligned with a coordinate system other than apparent RA,Dec,
*     and a shift is added to allow the map to be moved about in that output
*     frame.

*  Invocation:
*     CALL SCULIB_APPARENT_2_TP (N_POS, BOL_XPOS, BOL_YPOS, RA_CEN,
*    :  DEC_CEN, ROTATION, SHIFT_DX, SHIFT_DY, STATUS)

*  Arguments:
*     N_POS                  = INTEGER (Given)
*           the number of positions to be converted
*     BOL_XPOS (N_POS)       = DOUBLE PRECISION (Given and returned)
*           apparent RA on input (radians), x tangent plane offset on
*           output (radians)
*     BOL_YPOS (N_POS)       = DOUBLE PRECISION (Given and returned)
*           apparent Dec on input (radians), y tangent plane offset on
*           output (radians)
*     RA_CEN                 = DOUBLE PRECISION (Given)
*           apparent RA of tangent point (radians)
*     DEC_CEN                = DOUBLE PRECISION (Given)
*           apparent Dec of tangent point (radians)
*     ROTATION               = DOUBLE PRECISION (Given)
*           angle between output North and apparent North (radians,
*           measured anticlockwise from output North)
*     SHIFT_DX               = DOUBLE PRECISION (Given)
*           value to be added to x offsets (radians)
*     SHIFT_DY               = DOUBLE PRECISION (Given)
*           value to be added to y offsets (radians)
*     STATUS                 = INTEGER (Given and returned)

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
*     14-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N_POS
      DOUBLE PRECISION RA_CEN
      DOUBLE PRECISION DEC_CEN
      DOUBLE PRECISION ROTATION
      DOUBLE PRECISION SHIFT_DX
      DOUBLE PRECISION SHIFT_DY

*  Arguments Given & Returned:
      DOUBLE PRECISION BOL_XPOS (N_POS)
      DOUBLE PRECISION BOL_YPOS (N_POS)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION COS_ROT                ! cos (ROTATION)
      LOGICAL          ERROR                  ! .TRUE. if an error is
                                              ! returned by SLA_DS2TP
      DOUBLE PRECISION ETA                    ! x tangent plane coord
      INTEGER          POS                    ! position index in DO loop
      DOUBLE PRECISION SIN_ROT                ! sine (ROTATION)
      INTEGER          SLA_STATUS             ! status returned by SLA_DS2TP
      DOUBLE PRECISION XI                     ! y tangent plane coord

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (N_POS .GT. 0) THEN
         ERROR = .FALSE.
         SIN_ROT = SIN (ROTATION)
         COS_ROT = COS (ROTATION)

         DO POS = 1, N_POS

*  calculate tangent plane coords in apparent RA,Dec system

            CALL SLA_DS2TP (BOL_XPOS(POS), BOL_YPOS(POS),
     :        RA_CEN, DEC_CEN, XI, ETA, SLA_STATUS)

            IF (SLA_STATUS .NE. 0) THEN
               ERROR = .TRUE.
               BOL_XPOS (POS) = 0.0D0
               BOL_YPOS (POS) = 0.0D0
            ELSE

*  rotate the coords into the required output system and add the shift

               BOL_XPOS (POS) = XI * COS_ROT + ETA * SIN_ROT +
     :           SHIFT_DX
               BOL_YPOS (POS) = -XI * SIN_ROT + ETA * COS_ROT +
     :           SHIFT_DY
            END IF
         END DO

         IF (ERROR) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_APPARENT_2_TP: error(s) have '//
     :        'occurred in calculating the tangent plane coords ',
     :        STATUS)
         END IF
      END IF

      END
