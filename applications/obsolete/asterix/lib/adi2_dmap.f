      SUBROUTINE ADI2_DMAP( MODID, FITID, CACHEID, TYPE, MODE, ENDIM,
     :                      EDIMS, PSID, PTR, NELM, STATUS )
*+
*  Name:
*     ADI2_DMAP

*  Purpose:
*     Map the specified FITS data item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DMAP( MODID, FITID, CACHEID, TYPE, MODE, ENDIM, EDIMS, PSID,
*                     PTR, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     MODID = INTEGER (given)
*        Abstract data model
*     FITID = INTEGER (given)
*        FITS file id
*     CACHEID = INTEGER (given)
*        FITS data object to map
*     TYPE = CHARACTER*(*) (given)
*        Data type in which to map
*     MODE = CHARACTER*(*) (given)
*        Mapping mode
*     ENDIM = INTEGER (given)
*        Expect dimensionality
*     EDIMS[] = INTEGER (given)
*        Expected dimensions
*     PTR = INTEGER (returned)
*        Mapped data pointer
*     NELM = INTEGER (returned)
*        Number of mapped elements
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     Richard Beard: (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jun 1996 (DJA):
*        Original version.
*     11 Feb 1997 (RB):
*        Axis data and axis width capability
*     10 Mar 1997 (RB):
*        Move axis point from edge to middle of bin
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			MODID, CACHEID, ENDIM, EDIMS(*)
      INTEGER			PSID, FITID
      CHARACTER*(*)		TYPE, MODE

*  Arguments Returned:
      INTEGER			PTR, NELM

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC
      EXTERNAL			ADI2_ARYWB

*  Local Variables:
      CHARACTER*8		ATYPE			! Actual data type
      CHARACTER*20		ITEM			! Item name

      INTEGER			ENELM			! Expected # elements
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Actual dimensions
      INTEGER			PARENT			! Parent of cache obj

      LOGICAL			THERE			! Object exists?

      CHARACTER*1		AXIS
      INTEGER			IAX, I
      INTEGER			AXID, AXPTR
      REAL			BASE, DELTA
      REAL			PLTSCL, PIXSIZ
      LOGICAL			ISAXDAT, ISAXWID
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Expected number of data elements
      CALL ARR_SUMDIM( ENDIM, EDIMS, ENELM )

*  Are we getting axis data or axis widths? (mega fudge - rb)
      CALL ADI_NAME( PSID, ITEM, STATUS )
      ISAXDAT = .FALSE.
      IF ( ITEM(1:4) .EQ. 'Axis' .AND. ITEM(8:11) .EQ. 'Data' ) THEN
        ISAXDAT = .TRUE.
      END IF
      ISAXWID = .FALSE.
      IF ( ITEM(1:4) .EQ. 'Axis' .AND. ITEM(8:12) .EQ. 'Width' ) THEN
        ISAXWID = .TRUE.
      END IF

*  Get array shape and total number of elements
      IF ( ISAXDAT .OR. ISAXWID ) THEN
        NDIM = ENDIM
        DO I = 1, ENDIM
          DIMS(I) = EDIMS(I)
        END DO
      ELSE
        CALL ADI_CGET0C( CACHEID, 'TYPE', ATYPE, STATUS )
        CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, DIMS, NDIM,
     :                   STATUS )
      END IF
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  If number of elements differ we report an error
      IF ( ENELM .NE. NELM ) THEN
        CALL ADI_NAME( PSID, ITEM, STATUS )
        CALL MSG_SETC( 'IT', ITEM )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'The dimensions of item ^IT '/
     :           /'differ from those expected - check the program '/
     :                /'which created this file', STATUS )
        GOTO 99
      END IF

*  FITS mapping always works by creating the data array in the cache
*  object. Map that data if it exists, otherwise read it from the file
      IF ( ISAXDAT ) THEN
        CALL CHR_CTOI( ITEM(6:6), IAX, STATUS )
        CALL ADI2_GKEY0I( FITID, ' ', 'NAXIS'//ITEM(6:6), .FALSE.,
     :                    .FALSE., DIMS(IAX), ' ', STATUS )

*    Try for the axis base
        CALL ADI2_GKEY0R( FITID, ' ', 'CRPIX'//ITEM(6:6), .FALSE.,
     :                    .FALSE., BASE, ' ', STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          BASE = DIMS(IAX) / 2.0 + 0.5
        END IF

*    Try for the axis delta
        CALL ADI2_GKEY0R( FITID, ' ', 'CDELT'//ITEM(6:6), .FALSE.,
     :                    .FALSE., DELTA, ' ', STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL ADI2_GKEY0R( FITID, ' ', 'CD'//ITEM(6:6)//'_'//ITEM(6:6),
     :                      .FALSE., .FALSE., DELTA, ' ', STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            IF ( ITEM(6:6) .EQ. '1' ) THEN
              AXIS = 'X'
            ELSE
              AXIS = 'Y'
            END IF
            CALL ADI2_GKEY0R( FITID, ' ', 'PLTSCALE',
     :                        .FALSE., .FALSE., PLTSCL, ' ', STATUS )
            CALL ADI2_GKEY0R( FITID, ' ', AXIS//'PIXELSZ',
     :                        .FALSE., .FALSE., PIXSIZ, ' ', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              DELTA = 1.0
            ELSE
              DELTA = (PLTSCL * PIXSIZ) / (1000.0 * 3600.0)
              IF ( AXIS .EQ. 'X' ) THEN
                DELTA = -1.0 * DELTA
              END IF
            END IF
          END IF
        END IF

*    Now construct the axis data array
        CALL ADI_NEW( TYPE, 1, DIMS(IAX), AXID, STATUS )
        CALL ADI_MAP( AXID, TYPE, 'WRITE', AXPTR, STATUS )
        CALL ADI2_DMAP_AXINV ( BASE, DELTA, DIMS(IAX), %VAL(AXPTR),
     :                         STATUS )
        PTR = AXPTR

*  Or invent some axis widths
      ELSE IF ( ISAXWID ) THEN
        CALL CHR_CTOI( ITEM(6:6), IAX, STATUS )
        CALL ADI2_GKEY0R( FITID, ' ', 'CDELT'//ITEM(6:6), .FALSE.,
     :                    .FALSE., DELTA, ' ', STATUS )
        CALL ADI_NEW( TYPE, 1, DIMS(IAX), AXID, STATUS )
        CALL ADI_MAP( AXID, TYPE, 'WRITE', AXPTR, STATUS )
        CALL ADI2_DMAP_AXWID ( DELTA, DIMS(IAX), %VAL(AXPTR), STATUS )
        PTR = AXPTR

*  Otherwise carry on as normal
      ELSE
        CALL ADI_THERE( CACHEID, 'Value', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CMAP( CACHEID, 'Value', TYPE, MODE, PTR, STATUS )
        ELSE
          CALL ADI2_DCOP_IN( CACHEID, PTR, NELM, TYPE, STATUS )
        END IF
      END IF

*  Store mapping details
      CALL ADI2_STOMAP( PSID, CACHEID, 'dyn', 0, PTR, ENDIM, EDIMS, 0,
     :                  0, UTIL_PLOC(ADI2_ARYWB), TYPE, MODE, STATUS )

*  Unless the the mode is read the data is potentially modified
      IF ( MODE(1:1) .NE. 'R' ) THEN
        CALL ADI_CPUT0L( CACHEID, 'Modified', .TRUE., STATUS )
        CALL ADI_CGET0I( CACHEID, 'Parent', PARENT, STATUS )
        CALL ADI_CPUT0L( PARENT, 'Modified', .TRUE., STATUS )
      END IF

*  Always return expected number of elements
      NELM = ENELM

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DMAP', STATUS )
      END IF

      END


      SUBROUTINE ADI2_DMAP_AXINV( BASE, DELTA, NELM, AXDAT, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			BASE
      REAL			DELTA
      INTEGER			NELM
      REAL			AXDAT(*)
      INTEGER			STATUS

      INTEGER			I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NELM
        AXDAT(I) = (I - BASE - 0.5) * DELTA
      END DO

      END


      SUBROUTINE ADI2_DMAP_AXWID( DELTA, NELM, AXDAT, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			DELTA
      INTEGER			NELM
      REAL			AXDAT(*)
      INTEGER			STATUS

      INTEGER			I

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NELM
        AXDAT(I) = DELTA
      END DO

      END
