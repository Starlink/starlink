      SUBROUTINE CAP_CCTMO (RM, DM, KQ, EQ, KP, EP, PX, PRMS,
     :  AOB, ZOB, HOB, DOB, ROB, STATUS)
*+
*  Name:
*     CAP_CCTMO
*  Purpose:
*     Convert a mean place to observed place.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CCTMO (RM, DM, KQ, EQ, KP, EP, PX, PRMS;
*       AOB, ZOB, HOB, DOB, ROB; STATUS)
*  Description:
*     Convert a mean place to observed place.
*
*     The array PRMS contains precomputed J2000-to-apparent and
*     apparent-to-observed parameters as required by the routines
*     sla_MAPQKZ (first 21 elements) and sla_AOPQK (remaining 14
*     elements).
*  Arguments:
*     RM  =  (Given)
*        Given mean Right Ascension (Radians).
*     DM  =  (Given)
*        Given mean Declination (Radians).
*     KQ  =  (Given)
*        Time system for coordinate equinox: 'B' or 'J'.
*     EQ  =  (Given)
*        Equinox of the coordinates (years).
*     KP  =  (Given)
*        Time system for coordinate epoch: 'B' or 'J'.
*     EP  =  (Given)
*        Epoch of the coordinates (years).
*     PX  =  (Given)
*        Parallax (radians).
*     PRMS  =  (Given)
*        Conversion array: J2000 to observed parameters.
*     AOB  =  (Returned)
*        Observed azimuth (radians; Noprth=0, East=90 degrees).
*     ZOB  =  (Returned)
*        Observed zenith distance (radians).
*     HOB  =  (Returned)
*        Observed Hour Angle (radians).
*     DOB  =  (Returned)
*        Observed Declination (radians).
*     ROB  =  (Returned)
*        Observed Right Ascension (radians).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     (None available).
*  Implementation Deficiencies:
*     <...>
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     PTW: P T Wallace   (Starlink, RAL)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/10/92 (PTW): Original version COCOMO for ASTROM).
*     20/5/98  (ACD): CAP_CCTMO created for CURSA: added running status
*        argument and 'ADAM' style prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAP_PAR'           ! CAP parametric constants.
*  Arguments Given:
      DOUBLE PRECISION RM,DM
      CHARACTER KQ*(*)
      DOUBLE PRECISION EQ
      CHARACTER KP*(*)
      DOUBLE PRECISION EP,PX,PRMS(35)
*  Arguments Returned:
      DOUBLE PRECISION AOB, ZOB, HOB, ROB, DOB
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      DOUBLE PRECISION R2000,D2000,RAP,DAP
      DOUBLE PRECISION PXASEC
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Convert the parallax from radians to seconds of arc.

         PXASEC = (PX * 1.8D2 * 6.0D1 * 6.0D1) / CAP__PI

*
*       Given to J2000.

         CALL CAP_CCTMM (RM, DM, 1, 0.0D0, 0.0D0, PXASEC, 0.0D0,
     :      KQ, EQ, KP, EP,  'J', 2.0D3, KP, EP,
     :      R2000, D2000, STATUS)

*
*       J2000 to apparent.

         CALL sla_MAPQK(R2000,D2000,0D0,0D0,PXASEC,0D0,PRMS,RAP,DAP)

*
*       Apparent to observed.

         CALL sla_AOPQK(RAP,DAP,PRMS(22),AOB,ZOB,HOB,DOB,ROB)

      END IF

      END
