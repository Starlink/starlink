      SUBROUTINE IRAS_TAG
*+
*  Name:
*     IRAS_TAG

*  Purpose:
*     To add an IRAS astrometry extension to a reduced JCMT map

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRAS_TAG (STATUS)

*  Description:

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.lightfoot
*     {enter_new_authors_here}

*  History:
*     15-JUN-1992: Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! for DAT__SZLOC

*  Dynamic memory include file - defines DYNAMIC_MEM

*  Data structure error codes

*  functions:
      INTEGER          CHR_LEN   ! CHR string-length function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      INTEGER          INDF      ! NDF identifier
      DOUBLE PRECISION RACEN     ! RA of centre of input map (radians)
      DOUBLE PRECISION DECCEN    ! Dec of input map centre (radians)
      CHARACTER*(64)   SYSTEM    ! coordinate system of input map
      INTEGER          ICEN      ! i index of centre pixel in input map
      INTEGER          JCEN      ! j index of centre pixel in input map
      DOUBLE PRECISION PIXSIZE   ! size of pixels in input map
      INTEGER          IDA       ! IRA identifier of astrometry structure
      INTEGER          NP        ! Number of project parameters
      DOUBLE PRECISION P(8)      ! Parameter values of projection
      DOUBLE PRECISION EPOCH     ! Julian epoch at which observations were made
                                 !   (difference between JUlian and Besselian is
                                 !   ignored)
      CHARACTER*(30)   SCS       ! Name of Sky Coordinate System
      CHARACTER*(DAT__SZLOC) LOC !
*.

      STATUS = SAI__OK

*  begin an NDF context and initialise IRA

      CALL NDF_BEGIN
      CALL IRA_INIT (STATUS)

*  import the JCMT NDF to which the IRAS astrometry structure is to be added

      CALL NDF_ASSOC ('INPUT', 'UPDATE', INDF, STATUS)

*  read the RA, Dec and coordinate system from the MORE.JCMT_COORDS structure

      CALL NDF_XGT0D (INDF, 'JCMT_COORDS', 'RACEN', RACEN, STATUS)
      CALL NDF_XGT0D (INDF, 'JCMT_COORDS', 'DECCEN', DECCEN, STATUS)
      CALL NDF_XGT0C (INDF, 'JCMT_COORDS', 'SYSTEM', SYSTEM, STATUS)
      CALL NDF_XGT0I (INDF, 'JCMT_COORDS', 'ICEN', ICEN, STATUS)
      CALL NDF_XGT0I (INDF, 'JCMT_COORDS', 'JCEN', JCEN, STATUS)
      CALL NDF_XGT0D (INDF, 'JCMT_COORDS', 'PIXSIZE', PIXSIZE, STATUS)

*  create the astrometry structure

      NP = 8
      P (1) = RACEN
      P (2) = DECCEN
      P (3) = DBLE (ICEN) - 0.5D0
      P (4) = DBLE (JCEN) - 0.5D0
      P (5) = PIXSIZE
      P (6) = PIXSIZE
      P (7) = 0.0D0
      P (8) = 0.0D0

      IF (SYSTEM(:CHR_LEN(SYSTEM)) .EQ. 'FK4 B1950.0') THEN
         SCS = 'EQUATORIAL(1950.0)'
         EPOCH = 1950.0D0
      ELSE IF (SYSTEM(:CHR_LEN(SYSTEM)) .EQ. 'FK5 J2000.0') THEN
         SCS = 'EQUATORIAL(2000.0)'
         EPOCH = 2000.0D0
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SYSTEM', SYSTEM)
            CALL ERR_REP (' ', 'IRAS_TAG - bad coordinate system in '//
     :        'input file: ^SYSTEM', STATUS)
         END IF
      END IF
      CALL NDF_XNEW (INDF, 'IRAS', 'IRAS_EXTENSION', 0, 0, LOC, STATUS)
      CALL IRA_CREAT ('GNOMONIC', NP, P, SCS, EPOCH, INDF, IDA, STATUS)

*  close IRA and the NDF context

      CALL IRA_CLOSE (STATUS)
      CALL NDF_END (STATUS)

      END

