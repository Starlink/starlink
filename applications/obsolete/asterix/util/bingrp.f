      SUBROUTINE BINGRP( STATUS )
*+
*  Name:
*     BINGRP

*  Purpose:
*     Manipulates a dataset grouping array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL BINGRP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Constructs or deletes a dataset grouping array. Grouping is defined
*     on some or all of the input data dimensions. If all the dimensions
*     are not used then the full grouping array is obtained by cloning the
*     group sub-array through the missing dimensions with an additive
*     offset per copied sub-array.
*
*     The program recognises the following special keywords,
*
*       CANCEL 	- Cancel the current grouping
*       POLAR   - Construct grouping array in X,Y plane using radial
*                 and azimuthal bin specifications
*       ARD     - Construct grouping array in X,Y plane using ARD file
*       DUMP    - Write out a dataset whose single axis is group number
*                 and whose data is the grouped data
*
*     If none of these options is selected then the user is prompted
*     for axes on which groups are to be defined, after which ranges
*     defining group boundaries can be specified.

*  Usage:
*     bingrp {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset to be polared
*     OUT = CHAR (read)
*        Name of output dataset
*     XCENT = REAL (read)
*        X value of centre of polar
*     YCENT = REAL (read)
*        Y value of centre of polar
*     REG = LOGICAL (read)
*        Regular radial bins?
*     RBNDS = CHAR (read)
*        Radial bin boundaries
*     RBIN = REAL (read)
*        Size of radial bins
*     NRAD = INTEGER (read)
*        Number of radial bins
*     ABIN = REAL (read)
*        Size of azimuthal bins
*     AZSTART = REAL (read)
*        Start azimuth for azimuthal bins
*     POLAR = LOGICAL (read)
*        Work in POLAR mode?
*     ARD = LOGICAL (read)
*        Work in ARD mode?
*     DUMP = LOGICAL (read)
*        Work in DUMP mode?
*     OUT = CHAR (read)
*        Name of output grouped dataset
*     CANCEL = LOGICAL (read)
*        Work in CANCEL mode?

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

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     bingrp, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Mar 1996 V2.0-1 (DJA):
*        Original Version
*      2 Apr 1996 V2.0-2 (DJA):
*        Added irregular bins
*     15 Apr 1996 V2.0-3 (DJA):
*        Support DUMP mode
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'BINGRP Version V2.0-3' )
      INTEGER			MXRNG
        PARAMETER		( MXRNG = 100 )

*  Local Variables:
      CHARACTER*40		AUNITS(ADI__MXDIM)	! Axis units
      CHARACTER*6		MMODE			! Mapping mode

      REAL          		ABIN			! Azimuth bin size
      REAL          		AMIN(ADI__MXDIM),
     :         AMAX(ADI__MXDIM)     	! First and last elements of axes
      REAL          		ASCALE(ADI__MXDIM)      ! Width of each spatial axis bin
      REAL          		AZANG               	! Initial azimuth angle (degrees)
      REAL          		DIST			! Distance from centre to edge
      REAL          		PXCENT,PYCENT		! Coords of centre of
							! polar region in pixels
      REAL          		PRBIN			! Radial bin size in pixels
      REAL 	   		RBIN			! Radial bin size in axis units
      REAL			RBNDS(2*MXRNG)		! Radial boundaries
      REAL			SPARR(2)		! Spaced axis data
      REAL          		XCENT,YCENT		! Coords of centre of polar region in
							! axis units
      REAL          		XLOW,XHIGH,         	! Max and min values of axes
     :              		YLOW,YHIGH

      INTEGER			ARDID			! ARD group identifier
      INTEGER       		AXLP                	! Loop over input axes
      INTEGER			AXPTR(ADI__MXDIM)	! Mapped axis data
      INTEGER       		DIMS(ADI__MXDIM)    	! Input dimensions
      INTEGER			GPTR			! Grouping array
      INTEGER			GDPTR, GVPTR, GQPTR	! Grouped data
      INTEGER			I			! Loop over radial bounds
      INTEGER			IDPTR, IVPTR, IQPTR	! Input data
      INTEGER			IGRP			! Group number
      INTEGER       		IGPTR               	! Input data
      INTEGER			IFID			! Input dataset id
      INTEGER                   IGMIN, IGMAX            ! Group extrema
      INTEGER       		INELM               	! Input # elements
      INTEGER       		INDIM			! I/p dimensionality
      INTEGER       		K                   	! Length of strings
      INTEGER			MSKPTR			! ARD mask array
      INTEGER       		NABIN,NRBIN		! # of azimuthal & radial bins
      INTEGER                   NGELM                   ! # pixels in grped axes
      INTEGER			NGRP			! # groups
      INTEGER                   NOPAX                   ! # grouped axes
      INTEGER			OFID			! Input dataset id

      LOGICAL			ARD			! ARD file mode?
      LOGICAL			AXMODE			! Axis mode?
      LOGICAL			CANCEL			! Cancel mode?
      LOGICAL       		OK			! Object is ok?
      LOGICAL			POLAR			! Polar mode?
      LOGICAL			REG			! Regular radial bins?
      LOGICAL			UPDATE			! Update mode?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input and output data paths
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Initialise
      CANCEL = .FALSE.
      POLAR = .FALSE.
      ARD = .FALSE.
      AXMODE = .FALSE.

*  Get major mode
      CALL USI_GET0L( 'CANCEL', CANCEL, STATUS )
      IF ( .NOT. CANCEL ) CALL USI_GET0L( 'POLAR', POLAR, STATUS )
      IF ( .NOT. (CANCEL.OR.POLAR) ) THEN
        CALL USI_GET0L( 'ARD', ARD, STATUS )
      END IF
      IF ( .NOT. (CANCEL.OR.POLAR.OR.ARD) ) THEN
        CALL USI_GET0L( 'DUMP', DUMP, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. (CANCEL.OR.POLAR.OR.ARD.OR.DUMP) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Only POLAR, ARD, CANCEL and DUMP modes '/
     :                       /'are supported at the moment', STATUS )
        AXMODE = .TRUE.
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Does grouping already exist?
      CALL BDI_CHK( IFID, 'Grouping', OK, STATUS )

*  Cancel mode?
      IF ( CANCEL ) THEN
        IF ( .NOT. OK ) THEN
          CALL MSG_PRNT( 'No grouping array present in input!' )
        ELSE
          CALL BDI_DELETE( IFID, 'Grouping', STATUS )
        END IF
        GOTO 99

*  Grouping needs to be present in DUMP mode
      ELSE IF ( DUMP .AND. .NOT. OK ) THEN
        CALL MSG_PRNT( 'No grouping array present in input!' )
        GOTO 99

      END IF

*  Get input dimensionality
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, INDIM, STATUS )
      CALL ARR_SUMDIM( INDIM, DIMS, INELM )

*  Dump mode?
      IF ( DUMP ) THEN

*    Map the input data
        CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
        CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
        IF ( VOK ) THEN
          CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
        END IF
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPUB( IFID, 'Quality', 'READ', IQPTR, STATUS )
        END IF

*    Group the data
        CALL UTIL_GRPR( IFID, ' ', IDPTR, VOK, IVPTR, QOK, IQPTR,
     :                  OK, NGRP, GDPTR, GVPTR, GQPTR, STATUS )

*    No valid groups?
        IF ( NGRP .LE. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'No valid data groups in input', STATUS )
          GOTO 99
        END IF

*    Write out grouped dataset
        CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Define size of dataset
        CALL BDI_LINK( 'BinDS', 1, NGRP, 'REAL', OFID, STATUS )

*    Write elements
        CALL BDI_PUT1R( OFID, 'Data', NGRP, %VAL(GDPTR), STATUS )
        IF ( VOK ) THEN
          CALL BDI_PUT1R( OFID, 'Variance', NGRP, %VAL(GVPTR), STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDI_PUT1L( OFID, 'LogicalQuality', NGRP, %VAL(GQPTR),
     :                    STATUS )
        END IF
        SPARR(1) = 1.0
        SPARR(2) = 1.0
        CALL BDI_AXPUT1R( OFID, 1, 'Data', 2, SPARR, STATUS )
        CALL BDI_COPY( IFID, 'Label,Units,Title', OFID, ' ', STATUS )
        CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Group number', STATUS )

*    Copy ancillaries
        CALL UDI_COPANC( IFID, 'grf,grp', OFID, STATUS )

*    Write history
        CALL HSI_COPY( IFID, OFID, STATUS )
        CALL HSI_ADD( OFID, VERSION, STATUS )

*  POLAR or ARD modes?
      ELSE IF ( POLAR .OR. ARD ) THEN

*    Check dimensionality.
        IF ( INDIM .LT. 2 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Input must be at least 2 dimensional',
     :                  STATUS )
          GOTO 99
        END IF

*    Issue warning
        CALL MSG_PRNT( 'Assuming spatial dimensions are 1 and 2' )

*    Find axis values if present
        DO AXLP = 1, 2
          CALL BDI_AXMAPR( IFID, AXLP, 'Data', 'READ', AXPTR(AXLP),
     :                   STATUS )
          CALL ARR_ELEM1R( AXPTR(AXLP), DIMS(AXLP), 1, AMIN(AXLP),
     :                   STATUS )
          CALL ARR_ELEM1R( AXPTR(AXLP), DIMS(AXLP), DIMS(AXLP),
     :                   AMAX(AXLP), STATUS )
          ASCALE(AXLP) = (AMAX(AXLP)-AMIN(AXLP))/REAL(DIMS(AXLP)-1)
          CALL BDI_AXGET0C( IFID, AXLP, 'Units', AUNITS(AXLP), STATUS )
        END DO
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    ARD mode?
        IF ( ARD ) THEN

*      Get ARD text
          CALL ARX_OPEN( 'READ', ARDID, STATUS )
          CALL ARX_READ( 'ARDFILE', ARDID, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Map workspace
          CALL DYN_MAPI( 2, DIMS, MSKPTR, STATUS )

*      Define mask
          CALL ARX_MASK( ARDID, DIMS, AMIN, ASCALE, AUNITS,
     :                   %VAL(MSKPTR), STATUS )

*      Update mode?
          IF ( OK ) THEN
            CALL USI_GET0L( 'UPDATE', UPDATE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99
          ELSE
            UPDATE = .FALSE.
          END IF
          IF ( UPDATE ) THEN
            MMODE = 'UPDATE'
          ELSE
            MMODE = 'WRITE'
          END IF
          CALL BDI_MAPI( IFID, 'Grouping', MMODE, GPTR, STATUS )

*      If updating...
          IF ( UPDATE ) THEN

*        Determine number of groups in use in spatial plane
            CALL ARR_RANG1I( DIMS(1)*DIMS(2), %VAL(GPTR), IGMIN,
     :                       IGMAX, STATUS )

*        Adjust grouping array so that minimum is unity. This might not
*        be the case for a subsetted dataset
            CALL ARR_ADD1I( DIMS(1)*DIMS(2), %VAL(GPTR),
     :                      1 - IGMIN, %VAL(GPTR), STATUS )
            IGMAX = IGMAX - IGMIN

*        Announce this
            CALL MSG_SETI( 'NG', IGMAX )
            CALL MSG_PRNT( 'There are ^NG spatial groups already'/
     :                     /' defined' )

*        Get new group number
            CALL USI_DEF0I( 'IGRP', IGMAX+1, STATUS )
            CALL USI_GET0I( 'IGRP', IGRP, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

          ELSE
            CALL MSG_PRNT( 'Pixels inside ARD region will be '/
     :                     /'assigned to spatial group 1, those '/
     :                     /'outside to group 2' )

          END IF

*      Convert mask to index array
          CALL BINGRP_CNVARD( DIMS(1), DIMS(2), UPDATE, IGRP,
     :                     %VAL(MSKPTR), %VAL(GPTR), STATUS )

        ELSE

*      Tell user the axes ranges
          CALL MSG_SETR( 'XMIN',AMIN(1) )
          CALL MSG_SETR( 'XMAX',AMAX(1) )
          CALL MSG_PRNT( 'X Axis range: ^XMIN to ^XMAX' )
          CALL MSG_SETR( 'YMIN',AMIN(2) )
          CALL MSG_SETR( 'YMAX',AMAX(2) )
          CALL MSG_PRNT( 'Y Axis range: ^YMIN to ^YMAX' )

*      Because an axis may increase in any direction find max values.
          XLOW = MIN( AMIN(1), AMAX(1) )
          XHIGH = MAX( AMIN(1), AMAX(1) )
          YLOW = MIN( AMIN(2), AMAX(2) )
          YHIGH = MAX( AMIN(2), AMAX(2) )

*      Get central position
          CALL USI_GET0R( 'XCENT', XCENT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Test if this is within the image.
          IF ( (XCENT .LE. XLOW) .OR. (XCENT .GT. XHIGH) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'XCENT is outside range of image',
     :                    STATUS )
          END IF

*      Get Y centre
          CALL USI_GET0R( 'YCENT', YCENT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Test if this is within the image (pixel co-ords only)
          IF ( (YCENT .LE. YLOW) .OR. (YCENT .GT. YHIGH) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'YCENT is outside range of image',
     :                    STATUS )
          END IF
          IF (STATUS .NE. SAI__OK) GOTO 99

*      Find pixel position of the polar centre. This is measured in fractional
*      pixels from the extreme "left" and "bottom" edges of the 2d area, ie.
*      PXCENT runs from 0.0 to REAL(DIMS(1)).
          PXCENT = (XCENT-AMIN(1)) / ASCALE(1) + 0.5
          PYCENT = (YCENT-AMIN(2)) / ASCALE(2) + 0.5

*      Regular or irregular bins?
          CALL USI_GET0L( 'REG', REG, STATUS )
          IF (STATUS .NE. SAI__OK) GOTO 99

*      Regular? Use bin width and number
          IF ( REG ) THEN

*        Get radial bin width
            CALL USI_GET0R( 'RBIN', RBIN, STATUS )

*        Calculate number of output bins in each dimension
*        Find closest border distance in axis units
            DIST = MIN( (XHIGH-XCENT), (XCENT-XLOW), (YHIGH-YCENT),
     :                                              (YCENT-YLOW) )

*        See if limit on radial extent
            CALL USI_DEF0I('NRAD',MAX(1,INT(DIST/RBIN)),STATUS)
            CALL USI_GET0I('NRAD',NRBIN,STATUS)
            PRBIN=ABS(RBIN/ASCALE(1))

*      If irregular, get bounds
          ELSE

*        Find farthest border distance in axis units
            DIST = MAX( (XHIGH-XCENT), (XCENT-XLOW), (YHIGH-YCENT),
     :                                               (YCENT-YLOW) )

*        Get ranges
            CALL PRS_GETRANGES( 'RBNDS', MXRNG*2, 1, 0.0, DIST,
     :                          RBNDS, NRBIN, STATUS )

*        Convert axis values to pixels
            DO I = 1, NRBIN*2
              RBNDS(I) = RBNDS(I) / ABS(ASCALE(1))
            END DO

*        Add implicit bounds
            NRBIN = NRBIN + 2

          END IF
          IF (STATUS .NE. SAI__OK) GOTO 99

*      Calc number of azimuthal bins
          CALL USI_GET0R('ABIN',ABIN, STATUS)
          NABIN = NINT(360.0/ABIN)
          IF (NABIN .LT. 1) NABIN=1
          ABIN = 360.0/REAL(NABIN)

*      If an azimuthal axis is wanted get the start position from the user
          IF ( NABIN .NE. 1 ) THEN
            CALL USI_GET0R('AZSTART', AZANG, STATUS)
            IF (STATUS .NE. SAI__OK) GOTO 99
          ELSE
            AZANG = 0.0
          END IF

*      Tell user the binning being used.
          CALL MSG_SETI( 'NRBIN', NRBIN )
          IF ( NABIN .EQ. 1 ) THEN
            CALL MSG_PRNT( 'Grouping into ^NRBIN radial bins' )
          ELSE
            CALL MSG_SETI( 'NABIN', NABIN )
	    CALL MSG_PRNT( 'Grouping into ^NRBIN radial and ^NABIN'//
     :                                            ' azimuthal bins' )
          END IF

*      Map grouping array
          CALL BDI_MAPI( IFID, 'Grouping', 'WRITE', GPTR, STATUS )

*      In irregular case adjust bounds count
          IF ( .NOT. REG ) NRBIN = NRBIN - 1

*      Polar index the first slice
          CALL IMG_POLIDX( DIMS(1), DIMS(2), PXCENT, PYCENT, REG,
     :                     PRBIN, RBNDS,
     :                     AZANG, NRBIN, NABIN, %VAL(GPTR), STATUS )

*    End of ARD/POLAR switch
        END IF

*    We have operated on 2 dimensions
        NOPAX = 2

      END IF

*  Modifying grouping?
      IF ( .NOT. DUMP ) THEN

*    How many elements in grouped axes
        CALL ARR_SUMDIM( NOPAX, DIMS, NGELM )

*    Range of group numbers in sub-array
        CALL ARR_RANG1I( NGELM, %VAL(GPTR), IGMIN, IGMAX, STATUS )

*    Duplicate the selected sub-array through the grouping array
        IGPTR = GPTR
        DO K = 1, INELM/NGELM - 1

*      Advance to next input slice
          IGPTR = IGPTR + NGELM*VAL__NBI

*      Add offset to distinguish slices
          CALL ARR_ADD1I( NGELM, %VAL(GPTR), K*(IGMAX-IGMIN+1),
     :                    %VAL(IGPTR), STATUS )

        END DO

*    Unmap the grouping array
        CALL BDI_UNMAP( IFID, 'Grouping', GPTR, STATUS )

      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE BINGRP_CNVARD( NX, NY, UPDATE, IGRP, MASK,
     :                          GROUPS, STATUS )
*+
*  Name:
*     BINGRP_CNVARD

*  Purpose:
*     Convert ARD mask array to group array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GROUP_CNVARD( NX, NY, UPDATE, IGRP, MASK, GROUPS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        Size of spatial X dimennsion
*     NY = INTEGER (given)
*        Size of spatial Y dimennsion
*     UPDATE = LOGICAL (given)
*        Update mode?
*     IGRP = INTEGER (given)
*        New group number for selected points in update mode
*     MASK[NX,NY] = INTEGER (given)
*        The ARD mask array
*     GROUPS[NX,NY] = INTEGER (given and returned)
*        Group index array
*     STATUS = INTEGER (given)
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
*     {task_references}...

*  Keywords:
*     group, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			NX, NY, IGRP, MASK(NX,NY)
      LOGICAL			UPDATE

*  Arguments Given and Returned:
      INTEGER			GROUPS(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J 			! Loop over X,Y
      INTEGER			INGRP			! Group of inside pixels
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Group to assign pixels inside ARD region
      IF ( UPDATE ) THEN
        INGRP = IGRP
      ELSE
        INGRP = 1
      END IF

*  Loop over Y
      DO J = 1, NY

*    Loop over X
        DO I = 1, NX

*      Pixel inside ARD region?
          IF ( MASK(I,J) ) THEN
            GROUPS(I,J) = INGRP
          ELSE IF ( .NOT. UPDATE ) THEN
            GROUPS(I,J) = 2
          END IF

        END DO

      END DO

      END
