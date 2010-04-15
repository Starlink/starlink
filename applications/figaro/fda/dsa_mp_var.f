      SUBROUTINE DSA_MAP_VARIANCE( DSAREF, MODE, TYPE,
     :   ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_VARIANCE

*  Purpose:
*     Map the variance array in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_VARIANCE( DSAREF, MODE, TYPE, ADDRESS, MSLOT, STATUS )

*  Description:
*     This routine maps the variance data array in an NDF. If there is in
*     fact no variance data array, then an array
*     of zeros is generated and its address is returned.
*
*     The variance array is mapped without consultation of any quality
*     that might be present. The variance array should not contain bad
*     values, but this routine will not test this.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        One of 'READ','WRITE', or 'UPDATE', indicating the way the data
*        is going to be accessed. Only the first character is
*        significant.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be used to map the array. E.g. 'FLOAT' or
*        'DOUBLE'.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the mapped array.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Jun 1989 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Completely reworked to handle the case where the data structure
*        actually has a variance array, and not rely on processing an
*        uncertainty array mapped by DSA_MAP_ERRORS.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     06 Oct 1992 (hme):
*        Zero-initialise the array in case of write access, both in the
*        data structure and in the work space.
*     26 Oct 1994 (ks):
*        Now uses new calling sequence for DSA_MAP_ARRAY.
*     18 Dec 1995 (hme):
*        FDA library.
*     12 Feb 1996 (hme):
*        For UW access mark reference as variance-modified.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     20 Feb 1996 (hme):
*        Use given type for creation of new variance.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL QMFLAG             ! Previous quality masking flag
      LOGICAL VTHERE             ! True if variance exists
      INTEGER SLOT               ! The reference slot
      INTEGER PLACE              ! NDF placeholder
      INTEGER NELM               ! Mapped array size
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! The type in NDF speak
      CHARACTER * ( 1 ) MODEUC   ! Upper case mode

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find reference slot and translate data type. Upper-case mode.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )
      MODEUC = MODE(:1)
      CALL CHR_UCASE( MODEUC )

*  Find map slot.
      CALL DSA1_MSNEW( MSLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get current quality masking flag, and set it to false.
*  In Figaro variance is de-coupled from quality and never contains bad
*  values.
      CALL NDF_QMF( DSA__REFID1(SLOT), QMFLAG, STATUS )
      CALL NDF_SQMF( .FALSE., DSA__REFID1(SLOT), STATUS )

*     See if the variance component exists. If not we will have to
*     generate one.
         CALL NDF_STATE( DSA__REFID1(SLOT), 'VARIANCE', VTHERE, STATUS )

*     Write access or update access to absent variance.
         IF ( MODEUC .EQ. 'W' .OR.
     :        ( MODEUC .EQ. 'U' .AND. .NOT. VTHERE ) ) THEN
            IF ( .NOT. VTHERE ) CALL NDF_STYPE( NDFTYP,
     :         DSA__REFID1(SLOT), 'VARIANCE', STATUS )
            CALL NDF_MAP( DSA__REFID1(SLOT), 'VARIANCE', NDFTYP,
     :         'WRITE/ZERO', ADDRESS, NELM, STATUS )
            CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'VARIANCE',
     :         STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__MAPUSD(MSLOT) = .TRUE.
               DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
               DSA__MAPREF(MSLOT) = SLOT
               DSA__MAPNAM(MSLOT) = 'VARIANCE'
            END IF

*     Read access to absent variance.
         ELSE IF ( MODEUC .EQ. 'R' .AND. .NOT. VTHERE ) THEN
            IF ( DSA__REFID3(SLOT) .NE. NDF__NOID ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
               CALL ERR_REP( 'FDA_E024', 'DSA_MAP_VARIANCE: Cannot ' //
     :            'access variance for reference ^FDA_T001, ' //
     :            'since the buffer for the variance/error array ' //
     :            'is already in use.', STATUS )
               GO TO 400
            END IF
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :         'VARIANCE,NODATA,NOQUALITY,NOTITLE,NOLABEL,' //
     :         'NOHISTORY,NOEXTENSION()',
     :         PLACE, DSA__REFID3(SLOT), STATUS )
            CALL NDF_STYPE( NDFTYP, DSA__REFID3(SLOT), 'VARIANCE',
     :         STATUS )
            CALL NDF_MAP( DSA__REFID3(SLOT), 'VARIANCE', NDFTYP,
     :            'WRITE/ZERO', ADDRESS, NELM, STATUS )
            CALL NDF_SBAD( .FALSE., DSA__REFID3(SLOT), 'VARIANCE',
     :         STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__MAPUSD(MSLOT) = .TRUE.
               DSA__MAPID1(MSLOT) = DSA__REFID3(SLOT)
               DSA__MAPREF(MSLOT) = SLOT
               DSA__MAPNAM(MSLOT) = 'VARIANCE'
            END IF

*     Read or update access to existing variance.
         ELSE
            CALL NDF_MAP( DSA__REFID1(SLOT), 'VARIANCE', NDFTYP,
     :         MODE, ADDRESS, NELM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DSA__MAPUSD(MSLOT) = .TRUE.
               DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
               DSA__MAPREF(MSLOT) = SLOT
               DSA__MAPNAM(MSLOT) = 'VARIANCE'
            END IF

         END IF

*  Set the variance-modified flag.
      IF ( INDEX( 'UuWw', MODE(:1) ) .NE. 0 )
     :   DSA__REFMDV(SLOT) = .TRUE.

*  Tidy up (2).
 400  CONTINUE

*  Restore the original quality masking flag.
      CALL NDF_SQMF( QMFLAG, DSA__REFID1(SLOT), STATUS )

*  Tidy up (1).
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
