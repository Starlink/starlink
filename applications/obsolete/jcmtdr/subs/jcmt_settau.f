      SUBROUTINE JCMT_SETTAU (TAU, ENDTAU, TAUINTERP, NTOT, DATA,
     :   FBAD, LST, TAUARRAY, STATUS)
*+
*  Name:
*     JCMT_SETTAU

*  Purpose:
*     Fill the TAUARRAY with values given a start and end TAU

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_SETTAU (TAU, ENDTAU, TAUINTERP, NTOT, DATA, FBAD, LST,
*    :   LST, TAUARRAY, STATUS)

*  Description:
*     {routine_description}

*  Arguments:
*     TAU = REAL (Given)
*        zenith optical depth at the start of observation
*     ENDTAU = REAL (Given)
*        zenith optical depth at end of observation
*     TAUINTERP = LOGICAL (Given)
*        switch - if .true. then interpolate between start and end times
*        else fill whole array with TAU
*     NTOT = INTEGER (Given)
*        total number of pixels in the map
*     DATA = REAL (NTOT) (Given)
*        data array
*     FBAD = REAL (Given)
*        magic value specifying bad data point
*     LST (NTOT) = DOUBLE PRECISION (Given)
*        The local sidereal time of each map point
*     TAUARRAY (NTOT) = REAL (Returned)
*        The array of interpolated airmasses
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1990 (JBVAD::PAH):
*        Original version.
*     21-MAY-1991 (REVAD::JFL):
*        Modified as a result of move from MAKEMAP to JCMTEXTC, plus now
*        takes account of bad values in input data and ignores LST's associated
*        with them which may be invalid
*     11-NOV-1991 (REVAD::JFL):
*        Modified to set TAU to FBAD if associated map data point is
*        invalid
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL TAU, ENDTAU           ! zenith opacity at beginning and end of
                                 ! observation
      LOGICAL TAUINTERP          ! T if want to interpolate between TAU and
                                 ! ENDTAU
      INTEGER NTOT               ! number of data points
      REAL DATA (NTOT)           ! data
      REAL FBAD                  ! value specifying bad, or unset, data point
      DOUBLE PRECISION LST(NTOT) ! local sidereal time

*  Arguments Returned:
      REAL TAUARRAY (NTOT)

*  Status:
      INTEGER STATUS             ! Global status

*  local variables
      INTEGER I                  !
      INTEGER IGNORE             !
      REAL TAUSTEP               ! Rate of change of tau with LST
      DOUBLE PRECISION LSTMIN, LSTMAX
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (TAUINTERP) THEN

*  find lowest and highest LST in data set

         LSTMIN = LST(1)
         LSTMAX = LST(1)
         DO I = 1, NTOT
            IF (DATA(I) .NE. FBAD) THEN
               LSTMIN = MIN (LSTMIN, LST(I))
               LSTMAX = MAX (LSTMAX, LST(I))
            END IF
         END DO

*  find gradient of opacity change with time

         IF ((LSTMAX-LSTMIN) .NE. 0) THEN
            TAUSTEP = (ENDTAU-TAU) / (LSTMAX - LSTMIN)
         ELSE
            IGNORE = 0
            CALL PAR_WRUSER ('JCMT_SETTAU - All data taken at '//
     :     'same LST, opacities set to TAU', IGNORE)
            TAUSTEP = 0.0
         END IF

      ELSE

         TAUSTEP = 0.0

      END IF

*  go

      DO I = 1, NTOT
         IF (DATA(I) .NE. FBAD) THEN
            TAUARRAY (I) = TAU + (LST(I)-LSTMIN) * TAUSTEP
         ELSE
            TAUARRAY (I) = FBAD
         END IF
      END DO

      END

