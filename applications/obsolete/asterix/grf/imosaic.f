*+  IMOSAIC - Merges several non-congruent 2-d data arrays into one output
      SUBROUTINE IMOSAIC( STATUS )
*
*    Description :
*
*     Up to 20 non-congruent 2-d data arrays may be input, along with
*     their relative offsets from the first data array, and these are
*     then made into a mosaic into one (usually larger) output 2-d data
*     array. Where the frames overlap, either the mean value or just the
*     sum is inserted into the output data array. Normally averaging is
*     performed. All data arrays are stored in IMAGE structures.
*
*     The pixelsize of the first frame is used in the output image. Frames
*     with a different pixelsize are rebinned before being merged. Frames
*     are rotated to have the same orientation as the first frame if
*     necessary; note rebinning and rotating can introduce half pixel
*     errors in the data before they are merged.
*
*     The quality method is used for processing bad data.  Bad
*     pixels are excluded from the averaging in overlap areas. Output
*     pixels that have been mapped or correspond to one or more input
*     arrays, yet have no good pixels contributing, are set to bad.
*     Pixels in the output data array not mapped by any of the input
*     arrays are set to zero.
*
*     Note: Calculating the pixel offsets from the astrometry information
*           in each datafile relies on the units of the axes being the
*           same for each file. It is not appropriate to test the 'UNITS'
*           string in the axes structure for compatability, so there is
*           a potential danger here.
*
*    Parameters :
*
*     NUMBER  =  INTEGER( READ )
*         Number of data arrays to be merged
*     AVERAGE  = LOGICAL( READ )
*         If true overlap regions are averaged, alternatively, they are
*           summed
*     INPn  =  IMAGE( READ )
*         nth IMAGE structure containing a data array to be a
*           constituent of a mosaic
*     OUT  =  IMAGE( WRITE )
*         Output IMAGE structure containing the merged data array
*     OTITLE  =  CHAR( READ )
*         Label for the output IMAGE structure
*     XOFFSET =  INTEGER( READ )
*         x offset of nth data array from the first
*     YOFFSET =  INTEGER( READ )
*         y offset of nth data array from the first
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get number of frames to be merged
*     Determine whether or not averaging of overlapped regions required
*     Initialise good frame flag and counter
*     For each frame
*        Get locator for input structure
*        If there is an error then
*           If error is not abort request then
*              Report error and set good data flag to bad
*              Increment first good frame counter if the current
*                frame number has the same value
*           Else
*              Return after unmapping and annulling data
*                successfully accessed
*           Endif
*        Else
*           Map its data-array component
*           If there is an error then
*              Annul current frame
*              If error is not abort request then
*                 Report error and set good data flag to bad
*                 Increment first good frame counter if the current
*                   frame number has the same value
*              Else
*                 Return after unmapping and annulling data
*                   successfully accessed
*              Endif
*           Else
*              If not the first frame then
*                 Get the x,y offsets
*                 If there is an error then
*                    Annul and unmap current frame
*                    If error is not abort request then
*                       Report error and set good data flag to bad
*                         current frame number has the same value
*                    Else
*                       Return after unmapping and annulling data
*                         successfully accessed
*                    Endif
*                 Else
*                    Set the good data flag to true
*                 Endif
*              Else
*                 Set the good data flag to true
*                 Set both offsets to be zero
*              Endif
*           Endif
*        Endif
*     Endfor
*     Find the maximum and minimum offsets of the good frames
*     Calculate size of output frame and inform user
*     If either dimension is zero, meaning there were no valid input
*       frames then
*        Report error, set error status and return
*     Endif
*     For each good frame
*        Redefine offsets to be relative to the minima
*     Endfor
*     Create output IMAGE structure
*     If no error so far
*        Map new data-array component in output structure
*        If no error so far
*           Create and map work space for mask
*           If no error then
*              Zero mask and output arrays
*              If no error then
*                 For all good frames
*                    Call routine which actually makes the mosaic
*                 Endfor
*                 If no error and averaging required then
*                    Normalise output array by the mask
*                 Endif
*              Endif
*              Tidy workspace
*           Else
*              Report error
*           Endif
*           Unmap output data array
*        Else
*           Report error context
*        Endif
*        Tidy output IMAGE structure
*     Else
*        Report error context
*     Endif
*     Tidy input structures
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie RAL (UK.AC.RL.STAR::CUR)
*     Mark McCaughrean UOE (REVA::MJM)
*     David J. Allan (ROSAT)
*
*    History :
*
*     16 Sep 85 : First implementation (REVA::MJM)
*      7 Aug 86 : Renamed algorithm subroutines (MOSAIC_ADD to MOSCAD,
*                 MOSAIC_DIV to MOSCDV). Correctly ordered arguments
*                 in MOSCAD (7th to 5th). Added invocation to prologue
*                 (RL.STAR::CUR).
*     29 Aug 86 : Completed prologue (method and arguments), added
*                 status checking, and NUMREA variable so that the
*                 correct tidying of input structures is done. Nearly
*                 conformed to Starlink standards (RL.STAR::CUR).
*     16 Oct 87 : Reordered tidying and extra status checks
*                 (RL.STAR::CUR)
*     16 Mar 88 : Substituted AIF_ANTMP to annul workspace
*                 (RL.STAR::CUR).
*     17 Mar 88 : Referred to `array' rather than `image'
*                 (RL.STAR::CUR)
*     31 May 88 : Separated normalisation operation, and added
*                 average option (RL.STAR::CUR).
*     21 Jun 88 : More reporting of error context, added good data
*                 flag and checking (RL.STAR::CUR)
*     20 Oct 88 : Bug fix in offsets introduced 1988 May 31
*                 (RL.STAR::CUR)
*      7 Jul 89 : V1.2-0 ASTERIX version - uses astrometry data to calculate
*                        the relative offsets of each image. (RDS)
*     30 Apr 93 : V1.7-0 Redid type declaration to make this portable. (DJA)
*     27 Aug 93 : V1.7-1 Handles N-d dataset where first 2 dimensions
*                        are the spatial ones. Handles lack of variance
*                        much more efficiently (DJA)
*     24 Apr 95 : V1.8-0 Use new data interfaces - use WCI for world
*                        coordinates maniuplation. Fixed bug for fields
*                        near the poles (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE            ! no implicit typing allowed
*
*    Global constants :
*
      INCLUDE  'SAE_PAR'       ! global SSE definitions
      INCLUDE  'ADI_PAR'
      INCLUDE  'PAR_PAR'
      INCLUDE  'PRM_PAR'
      INCLUDE  'PAR_ERR'       ! parameter-system errors
      INCLUDE  'QUAL_PAR'
      INCLUDE  'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER                MXFRAM       	! Max number of frames allowed
        PARAMETER            ( MXFRAM = 20 )
*
*    Local variables :
*
      INTEGER
     :  NUMBER,                ! number of frames to be merged
     :  NUMREA,                ! number of frames located and mapped
     :  FIRSTG,                ! the number of the first data array
                               ! read and mapped sucessfully
     :  TDIMS(ADI__MXDIM),     ! dimensions of input DATA_ARRAY
     :  DDIMS(ADI__MXDIM),     ! dimensions of temporary space
     :  INDIMS,                ! number of dimensions of input array
     :  IDIMS( ADI__MXDIM, MXFRAM ),! array of dimensions of input DATA_ARRAYs
     :  PNTRA,                 ! pointer to input DATA_ARRAY
     :  PNTRB,                 ! pointer to input VARIANCE
     :  PNTRC                  ! pointer to input QUALITY ARRAY
      INTEGER
     :  PNTRID( MXFRAM ),      ! pointers to input DATA_ARRAYs after rotation/
c                              ! rebinning to new frame
     :  PNTRIV( MXFRAM ),      ! pointers to input VARIANCE after rotation/
c                              ! rebinning to new frame
     :  PNTRIQ( MXFRAM ),      ! pointers to input QUALITY after rotation/
c                              ! rebinning to new frame
     :  XOFSET( MXFRAM ),      ! x offset of Nth frame from first
     :  YOFSET( MXFRAM ),      ! y   "     "  "    "     "    "
     :  PNTROD,                ! pointer to output DATA_ARRAY
     :  PNTROV,                ! pointer to output VARIANCE
     :  PNTROQ                 ! pointer to output QUALITY
      INTEGER
     :  ODIMS(ADI__MXDIM),     ! dimensions of output DATA_ARRAY
     :  PNTRT,                 ! pointer to data-array mask array
     :  MINX,                  ! minimum x offset from first frame
     :  MINY,                  !    "    y   "      "    "     "
     :  MAXX,                  ! maximum x   "      "    "     "
     :  MAXY,                  !    "    y   "      "    "     "
     :  I, J, K, L             ! counters

      CHARACTER*(PAR__SZNAM) 	PARM       		! Parameter name

      DOUBLE PRECISION		POINT(2)		! N'th dataset pointing
      DOUBLE PRECISION		POINT_1(2)		! 1st dataset pointing
      DOUBLE PRECISION		SPOS1(2)		! First i/p cel pos'n
      DOUBLE PRECISION		SPOSN(2)		! Nth i/p cel pos'n

      REAL			PCORN(2,2,2)		! Pixel posn fn (x,y)
      REAL			PPOS1(2)		! 1st i/p pix pos'n
      REAL			PPOSN(2)		! Nth i/p pix pos'n
      REAL			SPARR(2)		! Spaced array data

      INTEGER			IFID(MXFRAM)		! Input dataset ids
      INTEGER			IFILES			! USI input file info
      INTEGER                	IPTRD,IPTRV,IPTRQ  	! Loops over input slices
      INTEGER			IX, IY			! Loops over corners
      INTEGER			OFID			! Output dataset id
      INTEGER                	OPTRD,OPTRV,OPTRQ  	! Loops over output slices
      INTEGER			IDUM			! Dummy argument
      INTEGER                	INELM              	! No of input elements
      INTEGER                	LPARM      		! Used length of PARM
      INTEGER                	ISPNELM(MXFRAM)    	! # elements in spatial axes
      INTEGER                	NSLICE			! # 2-d slices per input
      INTEGER                	ONDIM              	! # output dimensions
      INTEGER                	ONELM              	! # output elements
      INTEGER                	ONSLICE			! # o/p 2-d slices
      INTEGER                	OSPNELM            	! # spatial pixels in output
      INTEGER			PIXID,PRJID,SYSID	! WCS info for dataset
      INTEGER			PIXID_1,PRJID_1,SYSID_1	! WCS info dataset 1
      INTEGER                	SL                 	! Loop over slices

      BYTE                   	BADBITS(MXFRAM)    	! Input quality masks

      LOGICAL			AVERGE			! Overlaps averaged?

      LOGICAL                  ! true if :
     :  GOODAT( MXFRAM ),      ! entry corresponding to given frame in
                               ! sequence is true if data and x,y
                               ! offsets were found ok
     :  JUMPOUT,               ! Leave the loop ?
     :  OK,                    ! Is data object present ?
     :  LVAR,                  ! This is set false permanently if any file
c                              ! has no variance array.
     :  LQUAL,                 ! Has this file got a quality array ?
     :  LRADEC,                ! Has the file sufficient astrometry info
c                              ! to enable offsets to be calculated ?
     :  LRADEC_1               ! Has the first file sufficient astrometry info ?

      REAL
     :  XPIX,YPIX,             ! Pixel size of a frame
     :  XPIX_1,YPIX_1,         ! Pixel size of the first frame
     :  XPIX_OUT,YPIX_OUT,     ! Pixel size of rebinned frame in units
c                              ! of input pixels
     :  XMIN,XMAX,             ! Range of X values in an input frame
     :  YMIN,YMAX              ! Range of Y values in an input frame

      REAL
     :  XBASE, YBASE,          ! Base value of the rotated/rebinned input
c                              ! axis in units of input pixels.
     :  XSTART, YSTART,        ! Base value of the output axes in input
c                              ! axis units
     :  THETA                  ! Angle between input frame and frame one
*
*    Version id :
*
      CHARACTER*30		VERSION
        PARAMETER  		( VERSION = 'IMOSAIC Version 2.1-0' )
*-

*    Show version number
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT()

*    Set the variance flag true
      LVAR = .TRUE.

*    Start by getting number of frames to be merged
      JUMPOUT=.FALSE.
      DO WHILE (.NOT. JUMPOUT)
        CALL USI_GET0I( 'NUMBER', NUMBER, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Test if within acceptable range
        IF ( (NUMBER .LT. 2) .OR. (NUMBER .GT. MXFRAM) ) THEN
          CALL MSG_SETI('MXFRAM', MXFRAM)
          CALL MSG_PRNT('Number of frames must be between 2 and '/
     :                                                /'^MXFRAM' )
          CALL USI_CANCL('NUMBER',STATUS)
        ELSE
          JUMPOUT=.TRUE.
        END IF
      END DO

*    Ask user if want to average the overlap region or not
      CALL USI_GET0L( 'AVERAGE', AVERGE, STATUS )

*    Check status before continuing
      IF ( STATUS .NE. SAI__OK ) THEN
        IF ( STATUS .NE. PAR__ABORT ) THEN
          CALL ERR_REP( 'ERR_IMOSAIC_PAR2',
     :        'AST_ERR: Error obtaining how to process '/
     :        /'overlapping data arrays', STATUS )
        END IF
        GOTO 99
      END IF

*    Initialise data ok flags
      CALL ARR_INIT1L( .FALSE., MXFRAM, GOODAT, STATUS )

*  Now get the required number of input files
      FIRSTG = 1
      ONSLICE = 1
      DO  I  =  1, NUMBER

*      Construct parameter name
        CALL MSG_SETI( 'N', I )
        CALL MSG_MAKE( 'INP^N', PARM, LPARM )

*      Tell user which number frame is required
        CALL MSG_SETI( 'FRAMENO', I )
        CALL MSG_PRNT( 'Input frame number ^FRAMENO' )

*      Get a locator to an IMAGE-type data structure then cancel parameter
        CALL USI_IASSOC( 'INP', I, 'BinDS|Array', 'READ',
     :                   IFID(I), STATUS )

*      Report error including frame number
        IF ( STATUS .NE. SAI__OK ) THEN
          IF ( STATUS .NE. PAR__ABORT ) THEN

*          Report error and set flag for bad data
            CALL MSG_SETI( 'FRAMENO', I )
            CALL ERR_REP( 'ERR_IMOSAIC_NOFRI',
     :          'Error occurred while trying to access '/
     :           /'input frame number ^FRAMENO.', STATUS )

            GOODAT(I) = .FALSE.

*          Increment number of the first good array if current value
*          is not to be included in the calculations
            IF ( I .EQ. FIRSTG ) FIRSTG = FIRSTG + 1

          ELSE

*          Store number of arrays read if an abort has been requested
            NUMREA = I - 1
            GOTO 99

          END IF

        ELSE

*      Map in its main data component
          CALL BDI_CHK( IFID(I), 'Data', OK, STATUS )
          CALL BDI_GETSHP( IFID(I), ADI__MXDIM, TDIMS, INDIMS, STATUS )
          ONDIM = INDIMS
          DO J = 3, ONDIM
            ODIMS(J) = TDIMS(J)
          END DO
          CALL ARR_SUMDIM( INDIMS, TDIMS, INELM )

*      Given warning if not at least 2-d
          IF ( INDIMS .LT. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error, input frames must be at least'/
     :                                     /' 2 dimensional', STATUS )

*      If greater than 2-d we process datasets as stacks of 2-d images
          ELSE IF ( INDIMS .GT. 2 ) THEN

*          First time through?
            IF ( I .EQ. FIRSTG  ) THEN

*            Give warning
              CALL MSG_PRNT( 'Input frame has more than 2 dimensions. '/
     :                       /'The spatial dimensions will be assumed' )
              CALL MSG_PRNT( 'to be numbers 1 and 2, the rest will be '/
     :                       /'copied unchanged' )

*            Number of slices in output dataset
              CALL ARR_SUMDIM( INDIMS-2, TDIMS(3), ONSLICE )

            ELSE

*            Number of slices in this dataset
              CALL ARR_SUMDIM( INDIMS-2, TDIMS(3), NSLICE )
              IF ( NSLICE .NE. ONSLICE ) THEN
                STATUS = SAI__ERROR
                CALL MSG_SETI( 'N', I )
                CALL ERR_REP( ' ', 'Mis-match in non-spatial '/
     :                      /'dimensions of frame ^N', STATUS )
              END IF

            END IF

          END IF

*      Number of elements in spatial dimensions
          ISPNELM(I) = TDIMS(1) * TDIMS(2)

*      Map the data
          CALL BDI_MAPR( IFID(I), 'Data', 'READ', PNTRA, STATUS )

*      Check for an error
          IF ( STATUS .NE. SAI__OK ) THEN

*        Tidy current frame
            CALL USI_CANCL( PARM(:LPARM), STATUS )

            IF ( STATUS .NE. PAR__ABORT ) THEN

*          Report error and set flag for bad data
              CALL MSG_SETI( 'FRAMENO', I )
              CALL ERR_REP( 'ERR_IMOSAIC_NOMPI',
     :              'AST_ERR: Error occurred whilst trying to map '/
     :               /'input frame number ^FRAMENO.', STATUS )

              GOODAT(I) = .FALSE.

*          Increment number of the first good array if current
*          value is not to be included in the calculations
              IF ( I .EQ. FIRSTG ) FIRSTG = FIRSTG + 1

            ELSE

*            Store number of arrays read if an abort has been requested
              NUMREA = I - 1
              GOTO 99
            END IF
          ELSE

*          Attempt to map the variance array if all files so far
*          have contained variances
            IF ( LVAR ) THEN

              CALL BDI_CHK( IFID(I), 'Variance', LVAR, STATUS )

*            Check that variance is ok and compatible with data array
              IF ( LVAR ) THEN

*              Map the input variance array
                CALL BDI_MAPR( IFID(I), 'Variance', 'READ', PNTRB,
     :                         STATUS )
                IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( ' ', 'Error mapping input variance',
     :                                                      STATUS )
                END IF

              ELSE
                CALL MSG_PRNT( 'Variance array will not be '/
     :                       /'produced in the output file ' )
              END IF
            END IF

*        If variances haven't been mapped then make the variance pointer
*        point to the data. This is done purely to ensure that the pointer
*        can be passed correctly to lower level code. The data is not
*        accessed through this pointer provided LVAR is FALSE.
            IF ( .NOT. LVAR ) PNTRB = PNTRA

*        Attempt to map the QUALITY array.
            CALL BDI_CHK( IFID(I), 'Quality', LQUAL, STATUS )

*        Check that quality is ok
            IF ( LQUAL ) THEN

*            Map the input quality array
              CALL BDI_MAPUB( IFID(I), 'Quality', 'READ', PNTRC, STATUS)
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL MSG_PRNT('Error mapping quality array')
                GOTO 99
              END IF

*          Get quality mask
              CALL BDI_GET0UB( IFID(I), 'QualityMask', BADBITS(I),
     :                         STATUS )

*          Otherwise map a temporary array
            ELSE

              CALL MSG_PRNT( 'Quality array not found - assuming'/
     :                                   /' all pixels are good' )

              CALL DYN_MAPB( INDIMS, TDIMS, PNTRC, STATUS )
              IF ( STATUS .NE. SAI__OK ) GOTO 99

*            Fill quality array with good values
              CALL ARR_INIT1B( QUAL__GOOD, INELM, %VAL(PNTRC), STATUS )

*            Set badbits value
              BADBITS(I) = QUAL__MASK

            END IF

*          For all but the first frame, get the offsets of the
*          current frame from the first, setting first offsets to
*          zero, and then cancel parameter
            IF ( I .NE. FIRSTG ) THEN

*            If the first file contained astrometry information test
*            if this file has enough to calculate offsets.
              IF ( LRADEC_1 ) THEN
                CALL IMOSAIC_GETAST( IFID(I), TDIMS, POINT, XPIX, YPIX,
     :                            LRADEC, PIXID, PRJID, SYSID, STATUS )
                IF (STATUS .NE. SAI__OK) GOTO 99
              END IF

*            If the file contained a full set of attitude info
*            calculate the relative pixel offsets from the first file
              IF ( LRADEC ) THEN

*              Rotate and rebin the image so that it has the same
*              orientation as the first frame and the same pixel size.

*              Find position in fiducial pixels of the 4 corners of
*              this frame. At the same accumulate the extreme values
                XMIN = VAL__MAXR
                XMAX = VAL__MINR
                YMIN = VAL__MAXR
                YMAX = VAL__MINR
                DO IX = 1, 2
                  PPOSN(1) = 0.5 + REAL(IX-1)*TDIMS(1)
                  DO IY = 1, 2
                    PPOSN(2) = 0.5 + REAL(IY-1)*TDIMS(2)

*                  Find world coord position in I'th frame
                    CALL WCI_CNP2S( PPOSN, PIXID, PRJID, SPOSN, STATUS )

*                  Convert to world coords in fiducial frame
                    CALL WCI_CNS2S( SYSID, SPOSN, SYSID_1, SPOS1,
     :                              STATUS )

*                  Then to pixels in that frame
                    CALL WCI_CNS2P( SPOS1, PIXID_1, PRJID_1,
     :                              PCORN(1,IX,IY), STATUS )

*                  Gather extrema
                    XMIN = MIN( XMIN, PCORN(1,IX,IY) )
                    XMAX = MAX( XMAX, PCORN(1,IX,IY) )
                    YMIN = MIN( YMIN, PCORN(2,IX,IY) )
                    YMAX = MAX( YMAX, PCORN(2,IX,IY) )

                  END DO
                END DO

*              Defines the angle of rotation between frames.
                THETA = - ATAN2( PCORN(2,2,1) - PCORN(2,1,1),
     :                           PCORN(1,2,1) - PCORN(1,1,1) )

*              Calculate position of lower left hand corner of rotated,
*              rebinned region in terms of the pixel positions of the
*              current input
                PPOS1(1) = XMIN
                PPOS1(2) = YMIN
                CALL WCI_CNP2S( PPOS1, PIXID_1, PRJID_1, SPOS1, STATUS )
                CALL WCI_CNS2S( SYSID_1, SPOS1, SYSID, SPOSN, STATUS )
                CALL WCI_CNS2P( SPOSN, PIXID, PRJID, PPOSN, STATUS )

*              Calculate the origin of the rotated frame w.r.t the N'th
*              frame, in pixels
                XBASE = PPOSN(1)
                YBASE = PPOSN(2)

*              Calculate the dimensions of the rebinned output array
*              in terms of the pixel size of the fiducial input
                IDIMS(1,I) = NINT( XMAX - XMIN ) + 1
                IDIMS(2,I) = NINT( YMAX - YMIN ) + 1
                DO J = 3, INDIMS
                  IDIMS(J,I) = TDIMS(J)
                END DO
                ISPNELM(I) = IDIMS(1,I) * IDIMS(2,I)

*              Now calculate the output pixel widths in units of
*              input pixels.
                XPIX_OUT = XPIX_1 / XPIX
                YPIX_OUT = YPIX_1 / YPIX

*              Create temporary arrays for the rotated data, variance
*              and quality arrays.
                CALL ARR_COP1I( INDIMS, IDIMS(1,I), DDIMS, STATUS )
                CALL DYN_MAPR( INDIMS, DDIMS, PNTRID(I), STATUS )
                CALL DYN_MAPR( INDIMS, DDIMS, PNTRIV(I), STATUS )
                CALL DYN_MAPB( INDIMS, DDIMS, PNTRIQ(I), STATUS )
                IF ( STATUS .NE. SAI__OK ) GOTO 99

*              Zero these arrays prior to use
                CALL ARR_INIT1R( 0.0, INELM, %VAL(PNTRID(I)), STATUS )
                CALL ARR_INIT1R( 0.0, INELM, %VAL(PNTRIV(I)), STATUS )
                CALL ARR_INIT1B( QUAL__GOOD, INELM,
     :                                      %VAL(PNTRIQ(I)), STATUS )

*              Do the rotation and rebinning over each slice of the inputs
                IPTRD = PNTRA
                IPTRV = PNTRB
                IPTRQ = PNTRC
                OPTRD = PNTRID(I)
                OPTRV = PNTRIV(I)
                OPTRQ = PNTRIQ(I)
                DO SL = 1, ONSLICE

*                Rebin this slice
                  CALL IMOSAIC_ROTREB( TDIMS(1), TDIMS(2), %VAL(IPTRD),
     :                    LVAR, %VAL(IPTRV), %VAL(IPTRQ), XBASE, YBASE,
     :                           XPIX_OUT, YPIX_OUT, THETA, IDIMS(1,I),
     :                            IDIMS(2,I), %VAL(OPTRD), %VAL(OPTRV),
     :                                            %VAL(OPTRQ) ,STATUS )

*                Advance pointers to next slice
                  IPTRD = IPTRD + TDIMS(1)*TDIMS(2)*VAL__NBR
                  IPTRV = IPTRV + TDIMS(1)*TDIMS(2)*VAL__NBR
                  IPTRQ = IPTRQ + TDIMS(1)*TDIMS(2)*VAL__NBB
                  OPTRD = OPTRD + ISPNELM(I)*VAL__NBR
                  OPTRV = OPTRV + ISPNELM(I)*VAL__NBR
                  OPTRQ = OPTRQ + ISPNELM(I)*VAL__NBB

                END DO

*            Unmap data arrays
                CALL BDI_UNMAP( IFID(I), 'Data', PNTRA, STATUS )
                IF ( LVAR ) THEN
                  CALL BDI_UNMAP( IFID(I), 'Variance', PNTRB, STATUS )
                END IF
                IF ( LQUAL ) THEN
                  CALL BDI_UNMAP( IFID(I), 'Quality', PNTRC, STATUS )
                END IF

*            Calculate the X,Y offsets of the rebinned rotated frame from
*            the first frame in units of integer output pixels.
*            XOFSET is here defined as positive pixels to the right.
                XOFSET(I) = XMIN
                YOFSET(I) = YMIN

*            The data array and offsets are all in order so set
*            the good data flag for this frame
                GOODAT(I) = .TRUE.

              ELSE

*              Ask for offsets
                CALL USI_GET0I( 'XOFFSET', XOFSET(I), STATUS )
                CALL USI_GET0I( 'YOFFSET', YOFSET(I), STATUS )

*              Check for an error
                IF ( STATUS .NE. SAI__OK ) THEN

                  IF ( STATUS .NE. PAR__ABORT ) THEN

*                  Report error and set flag for bad data
                    CALL MSG_SETI( 'FRAMENO', I )
                    CALL ERR_REP( 'ERR_IMOSAIC_OFFSET',
     :                     'AST_ERR: Error occurred whilst trying to '/
     :                     /'get offset for input frame number '/
     :                     /'^FRAMENO.', STATUS )

                    GOODAT(I) = .FALSE.

                  ELSE

*                  Store number of arrays read if an abort has been
*                  requested
                    NUMREA = I - 1
                    GOTO 99
                  END IF

                ELSE

*                The data array and offsets are all in order so set
*                the good data flag for this frame
                  GOODAT(I) = .TRUE.

*                Copy temp pointers into input frame pointer arrays
                  PNTRID(I) = PNTRA
                  PNTRIV(I) = PNTRB
                  PNTRIQ(I) = PNTRC

*                end of error-getting-offsets check

                END IF

*              Cancel parameters for next frame
                CALL USI_CANCL( 'XOFFSET', STATUS )
                CALL USI_CANCL( 'YOFFSET', STATUS )

              END IF

            ELSE

*            No offset for first frame
              XOFSET(1) = 0
              YOFSET(1) = 0

*            The data array and offsets are all in order so set
*            the good data flag for this frame
              GOODAT(I) = .TRUE.

*            Copy pointer into input frame pointer array
              PNTRID(I) = PNTRA
              PNTRIV(I) = PNTRB
              PNTRIQ(I) = PNTRC

*            Update array of input dimensions
              CALL ARR_COP1I( INDIMS, TDIMS, IDIMS(1,I), STATUS )

*            Obtain astrometry information from the first file
              CALL IMOSAIC_GETAST( IFID(I), TDIMS, POINT_1, XPIX_1,
     :                             YPIX_1, LRADEC_1, PIXID_1,
     :                             PRJID_1, SYSID_1, STATUS )
              IF (STATUS .NE. SAI__OK) GOTO 99

*          end of first-frame check
            END IF

*        end of if-error-mapping-input-array check
          END IF

*      end of if-error-after-getting-input-structure check
        END IF

      END DO

      NUMREA = NUMBER

*    Work out the size of the output frame to be created -
*    first sort out the maximum and minimum offsets.

*    Initialise the maxima and minima values
      MINX  =  0
      MINY  =  0
      MAXX  =  0
      MAXY  =  0

*    Loop round for each good input frame
      DO  J  =  1, NUMBER

        IF ( GOODAT(J) ) THEN

*        Compare the current x and y offsets with minima found so far
          MINX = MIN ( XOFSET(J), MINX )
          MINY = MIN ( YOFSET(J), MINY )

*        Just use MAX function for the maxima; maxima are found
*        from the offset plus frame size relative to first frame
          MAXX  =  MAX( MAXX, IDIMS(1,J) + XOFSET(J) )
          MAXY  =  MAX( MAXY, IDIMS(2,J) + YOFSET(J) )

        END IF

      END DO

*    Calculate size of output frame from the extrema in the
*    offset values
      ODIMS(1) = MAXX - MINX
      ODIMS(2) = MAXY - MINY
      OSPNELM = ODIMS(1) * ODIMS(2)
      CALL ARR_SUMDIM( ONDIM, ODIMS, ONELM )

*    Inform user of output array dimensions
      CALL MSG_SETI( 'NEWXDIM', ODIMS( 1 ) )
      CALL MSG_SETI( 'NEWYDIM', ODIMS( 2 ) )
      IF ( ONDIM .EQ. 2 ) THEN
        CALL MSG_PRNT( 'Output array size is ^NEWXDIM by ^NEWYDIM' )
      ELSE
        CALL MSG_PRNT( 'Output spatial dimensions are ^NEWXDIM '/
     :                                           /'by ^NEWYDIM' )
      END IF

*    Watch for zero dimensions, meaning there were no valid input
*    frames
      IF ( ODIMS(1) .EQ. 0 .OR. ODIMS(2) .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( 'ERR_IMOSAIC_OUTDIM',
     :     'AST_ERR: Output dimensions error. Check input frames',
     :     STATUS )
        GOTO 99
      END IF

*    Redefine offsets to be relative to the origin in the output array
*    so that the offsets will not be negative
      DO  K  =  1, NUMBER
        IF ( GOODAT(K) ) THEN
          XOFSET(K) = XOFSET(K) - MINX
          YOFSET(K) = YOFSET(K) - MINY
        END IF
      END DO

*  Now get the output file
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'BinDS', ONDIM, ODIMS, 'REAL', OFID, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*    Map the output data
        CALL BDI_MAPR( OFID, 'Data', 'WRITE/ZERO', PNTROD, STATUS )

*      Create and map the output variance array if required
        IF ( LVAR ) THEN

          CALL BDI_MAPR( OFID, 'Variance', 'WRITE/ZERO', PNTROV,
     :                   STATUS )

        ELSE

*        Set variance to point to data array. As long as LVAR is FALSE then
*        neither MOSCAD nor MOSCDV will try to access the variance data. We
*        only do this to avoid array dimension errors at run-time.
          PNTROV = PNTROD

        END IF

*    Map output quality array
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE', PNTROQ, STATUS )
        CALL ARR_INIT1B( QUAL__GOOD, ONELM, %VAL(PNTROQ), STATUS )

*    Test if any of these have failed
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Create some temporary workspace to hold the array mask. Only big
*      enough to hold first 2 dimensions
          CALL DYN_MAPR( 2, ODIMS, PNTRT, STATUS )

          IF ( STATUS .EQ. SAI__OK ) THEN

*          Initialise output slice pointers
            OPTRD = PNTROD
            OPTRV = PNTROV
            OPTRQ = PNTROQ

*          Loop over slices in output
            DO SL = 1, ONSLICE

*            Initialise array mask to be zero.
              CALL ARR_INIT1R( 0.0, OSPNELM, %VAL(PNTRT), STATUS )

*            Add each array into the (big) output array, updating
*            the array mask at the same time, using MOSCAD
              DO  L  =  1, NUMBER
                IF ( GOODAT(L) ) THEN
                  IPTRD = PNTRID(L) + (SL-1)*ISPNELM(L)*VAL__NBR
                  IPTRV = PNTRIV(L) + (SL-1)*ISPNELM(L)*VAL__NBR
                  IPTRQ = PNTRIQ(L) + (SL-1)*ISPNELM(L)*VAL__NBB
                  CALL IMOSAIC_MOSCAD( %VAL(IPTRD), LVAR, %VAL(IPTRV),
     :                    %VAL(IPTRQ), BADBITS(L), IDIMS(1,L),
     :                    IDIMS(2,L), XOFSET(L), YOFSET(L), ODIMS(1),
     :                    ODIMS(2), %VAL(OPTRD), %VAL(OPTRV),
     :                    %VAL(OPTRQ), %VAL(PNTRT), STATUS )
                END IF
              END DO

*            Now apply normalisation by dividing by the mask where
*            more than one input pixel has contributed to the
*            output pixel.
              IF ( STATUS .EQ. SAI__OK .AND. AVERGE ) THEN
                CALL IMOSAIC_MOSCDV( ODIMS(1), ODIMS(2), %VAL(PNTRT),
     :                               %VAL(OPTRD), LVAR, %VAL(OPTRV),
     :                               %VAL(OPTRQ), STATUS )
              END IF

*            Advance output data pointers to next slice
              OPTRD = OPTRD + OSPNELM*VAL__NBR
              OPTRV = OPTRV + OSPNELM*VAL__NBR
              OPTRQ = OPTRQ + OSPNELM*VAL__NBB

*          end-of-loop-over-output-slices
            END DO

*          Unmap the temporary workspace
            CALL DYN_UNMAP( PNTRT, STATUS )

          ELSE

            CALL ERR_REP( 'ERR_IMOSAIC_WSP',
     :           'AST_ERR: Unable to get workspace for array mask',
     :           STATUS )

*        end of creating-and-mapping-workspace check
          END IF

        ELSE
          CALL ERR_REP( 'ERR_IMOSAIC_NOMPO',
     :        'AST_ERR: Error occurred whilst trying to map output '/
     :        /'frame', STATUS )

*      end of if-no-error-after-mapping-output-data-array check
        END IF

      ELSE

        IF ( STATUS .NE. PAR__ABORT ) THEN
          CALL ERR_REP( 'ERR_IMOSAIC_NOFRO',
     :        'AST_ERR: Error occurred whilst trying to access output '/
     :        /'frame', STATUS )
        END IF

*    end of if-no-error-after-creating-output-structure check
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Copy ancillaries
      CALL UDI_COPANC( IFID(FIRSTG), 'grf', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Error copying MORE box into output file' )
        CALL ERR_ANNUL( STATUS )
      END IF

*  Write output axes if astrometry info was sufficient in first file
      IF ( LRADEC_1 ) THEN

*    Calculate base values of the output axes
        CALL BDI_AXGET1R( IFID(FIRSTG), 1, 'SpacedData', 2, SPARR,
     :                    IDUM, STATUS )
        XSTART = SPARR(1) + MINX * XPIX_1
        CALL BDI_AXGET1R( IFID(FIRSTG), 2, 'SpacedData', 2, SPARR,
     :                    IDUM, STATUS )
        YSTART = SPARR(1) + MINY * YPIX_1

*    Write values axes
        SPARR(1) = XSTART
        SPARR(2) = XPIX_1
        CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
        SPARR(1) = YSTART
        SPARR(2) = YPIX_1
        CALL BDI_AXPUT1R( OFID, 2, 'SpacedData', 2, SPARR, STATUS )

*    Copy axis text from 1st input to output
        CALL BDI_AXCOPY( IFID(FIRSTG), 1, 'Label,Units', OFID, 1,
     :                   STATUS )
        CALL BDI_AXCOPY( IFID(FIRSTG), 2, 'Label,Units', OFID, 2,
     :                   STATUS )

*    Extra axes just get copied
        IF ( ONDIM .GT. 2 ) THEN
          DO I = 3, ONDIM
            CALL BDI_AXCOPY( IFID(FIRSTG), I, ' ', OFID, I, STATUS )
          END DO
        END IF

      END IF

*  Copy history structure from first file into the output file
      CALL HSI_COPY( IFID(FIRSTG), OFID, STATUS )

*  Add standard record to new history structure
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Put names of input datafiles used into history
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  IMOSAIC_GETAST - Gets astrometry information from a datafile
      SUBROUTINE IMOSAIC_GETAST( FID, IDIMS, POINT, XPIX, YPIX, LRADEC,
     :                           PIXID, PRJID, SYSID, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton   (LTVAD::RDS)
*    History :
*     7 July 1989   original     (LTVAD::RDS)
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER			FID			! Input dataset id
      INTEGER IDIMS(2)                           !Dimensions of input data array
*
*    Export :
*
      DOUBLE PRECISION		POINT(2)		! Pointing direction
      REAL XPIX                                  !Size of pixel in first dim.
      REAL YPIX                                  !Size of pixel in second dim.
      LOGICAL LRADEC                             !Sufficient astrometry info ?
      INTEGER			PIXID,PRJID,SYSID	! Astrometry info
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL			SCALE(2)			! Bin sizes
      INTEGER			NACT			! Values read from obj
*-

      IF (STATUS .NE. SAI__OK) RETURN

*    Locate astrometry
      CALL WCI_GETIDS( FID, PIXID, PRJID, SYSID, STATUS )

*  Test if any objects were missing
      IF (STATUS .NE. SAI__OK) THEN

        CALL ERR_ANNUL(STATUS)
        CALL MSG_PRNT('Insufficient astrometry info. '/
     :                     /'in first datafile - using pixel offsets')

        LRADEC = .FALSE.

      ELSE
        LRADEC=.TRUE.

*    Defines bin width
        CALL ADI_CGET1R( PIXID, 'SCALE', 2, SCALE, NACT, STATUS )
        XPIX = SCALE(1)
        YPIX = SCALE(2)

      END IF

*  Extract pointing
      CALL ADI_CGET1D( PRJID, 'SPOINT', 2, POINT, NACT, STATUS )
      POINT(1) = POINT(1) * MATH__DDTOR
      POINT(2) = POINT(2) * MATH__DDTOR

*  Tidy up
 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'IMOSAIC_GETAST', STATUS )
      END IF

      END


*+  IMOSAIC_ROTREB - Sample an image array into a new grid
      SUBROUTINE IMOSAIC_ROTREB(IDIM1, IDIM2, INDATA, VAROK, INVAR,
     :              INQUAL, XBASE, YBASE, XPIX, YPIX, THETA, ODIM1,
     :                    ODIM2, OUTDATA, OUTVAR, OUTQUAL, STATUS )
*
*    Description :
*
*      Takes in an image array and rotates it and rebins it into a
*      new grid. The position of each pixel in the new image is found in
*      the coordinate system of the old image. The nearest pixel to this
*      position in the old image is then added into the new image. New pixels
*      outside the old image bounds are left as they are.
*      If XPIX,YPIX are < 1 then each old pixel may be repeated
*      more than once in the new image. If XPIX,YPIX are >1 then
*      some old pixels may be lost.
*      The algorithm insures that all the new pixels that cover the old image
*      are assigned a value. Repeated use of this routine should be avoided
*      because features in the old image may be displaced by upto half a pixel
*      in the new image or lost if the new sample size is too large.
*
*    Author :
*
*     Dick Willingale (LTVAD::RW)
*
*    History :
*
*     26 Oct 88 : Original (RW)
*     11 Jul 89 : Renamed MOSAIC_ROTREB and changed for Asterix (RDS)
*     24 Aug 89 : Normalise output value by area (RDS)
*     27 Aug 93 : Variance presence signalled by VAROK (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                IDIM1,IDIM2        ! Dimensions of input array
      REAL                   INDATA(IDIM1,IDIM2)! Input data array
      LOGICAL                VAROK              ! Variance present?
      REAL                   INVAR(IDIM1,IDIM2) ! Input VARIANCE
      BYTE                   INQUAL(IDIM1,IDIM2)! Input QUALITY array

      REAL XBASE,YBASE                 ! X,Y position of bottom left hand corner
*                                      !  of new image in input pixels.
      REAL XPIX,YPIX                   ! X,Y length of new pixels in units of
*                                      !  old pixels.
      REAL THETA                       ! Angle of new x-axis wrt old
*                                      !  x-axis (radians +ve anticlockwise)
      INTEGER ODIM1,ODIM2              ! Dimensions of output array
*    Export :
      REAL OUTDATA(ODIM1, ODIM2)       ! Output data array
      REAL OUTVAR(ODIM1, ODIM2)        ! Output VARIANCE

      BYTE OUTQUAL(ODIM1, ODIM2)       ! Output Quality array
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL CTH,STH                     ! Cos and sin of rotation angle
      REAL XP,YP                       ! X and Y offsets from the base position
      REAL XOLD,YOLD                   ! X,Y position in old reference frame of
*                                      !  new pixel.

      INTEGER IX,IY                    ! X and Y pixel positions in output array
      INTEGER J,K                      ! Loop variables
*-

*    Check status
      IF (STATUS.NE.SAI__OK) RETURN

*    Set sin and cos transformations
      CTH=COS(THETA)
      STH=SIN(THETA)

*    Loop over the output pixels
      DO J=1,ODIM2

*      Find Y value.
        YP = (REAL(J)-0.5)*YPIX
c        YP = YP - (YBASE + REAL(IDIM2) / 2.0)

	DO K=1,ODIM1
*
	  XP=(REAL(K)-0.5)*XPIX
c          XP = XP - (XBASE + REAL(IDIM1) / 2.0)
*
          XOLD = XP*CTH-YP*STH + XBASE
	  YOLD = XP*STH+YP*CTH + YBASE

	  IX=XOLD+1
*
	    IF (IX .GT. 0 .AND. IX .LE. IDIM1) THEN
*
	       IY=YOLD+1
*
* Set output pixel value to that of the nearest input pixel and normalise
* by area.
	       IF (IY .GT. 0 .AND. IY .LE. IDIM2) THEN
*
	          OUTDATA(K,J) = INDATA(IX,IY) * XPIX * YPIX
*
	          IF ( VAROK ) THEN
                    OUTVAR(K,J) = INVAR(IX,IY) * (XPIX*YPIX)*(XPIX*YPIX)
                  END IF
*
                  OUTQUAL(K,J) = INQUAL(IX,IY)
*
               ENDIF
*
	    ENDIF
*
         ENDDO
      ENDDO
*
      END


*+  IMOSAIC_MOSCAD - Adds a 2-d array to a new array and records the overlap
      SUBROUTINE IMOSAIC_MOSCAD ( INARR, VAROK, INVAR, INQUAL, BADBITS,
     :                      IDIM1, IDIM2, XOFSET, YOFSET, ODIM1, ODIM2,
     :                          OUTARR, OUTVAR, OUTQUAL, MASK, STATUS )
*
*    Description :
*
*     This routine adds a 2-d array into a (usually larger) output data
*     array, and is used to make a mosaic of arrays. The offset of the
*     small array relative to the large one is given. For each valid
*     pixel of the small array, the value of that pixel is added to
*     the input value of the corresponding pixel in the big array, but
*     in a weighted fashion according to the number of previous
*     additions to that pixel, as recorded in the mask array. If a
*     successful addition takes place for a particular pixel, the
*     corresponding pixel in the mask array is incremented by 1 to
*     record this fact.
*
*    Arguments :
*
*     INARR( IDIM1, IDIM2 ) = REAL( READ )
*         Input old image
*     IDIM1 & IDIM2 = INTEGER( READ )
*         Dimensions of input array
*     XOFSET = INTEGER( READ )
*         x offset of image from bottom left
*     YOFSET = INTEGER( READ )
*         y offset of image from bottom left
*     ODIM1 & ODIM2 = INTEGER( READ )
*         Dimensions of output image
*     OUTARR( ODIM1, ODIM2 ) = REAL( UPDATE )
*         Data array containing merged image
*     MASK( ODIM1, ODIM2 ) = REAL( UPDATE )
*         Array containing details of pixel contibutions
*     STATUS = INTEGER( READ )
*         Global status
*
*    Method :
*
*     Check for error on entry
*     If o.k. then
*     For each pixel
*        Find position of current pixel in output array
*        If pixel is valid then
*           If the mask value is zero then
*              Output pixel takes input-pixel value
*              Set mask value to one
*           Else
*              Add the input-pixel value to the corresponding output
*                pixel
*              Increment the mask by one
*        Elseif the mask is bad then
*           Set the output pixel to be bad
*        Endif
*     Endfor
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean (UoE,REVA::MJM)
*     Malcolm J. Currie (RAL,RAL::CUR)
*     Richard Saxton (Starlink,LTVAD::RDS)
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     16 Sep 85 : First implementation (REVA::MJM)
*     15 Aug 86 : Renamed from MOSAIC_ADD, arguments reordered (7th
*                 to 5th), completed the prologue, nearly conformed
*                 to Starlink programming standards (RL.STAR::CUR).
*      4 Sep 86 : Renamed parameters section to arguments, applied
*                 bad-pixel handling (RL.STAR::CUR).
*     31 May 89 : Removed normalisation code so that optional
*                 averaging may be performed by the calling
*                 application ( RAL::CUR )
*      1 Aug 89 : Changed name and input arguments (RDS)
*     19 Nov 89 : Fixed bug in quality manipulation (RDS)
*     27 Aug 93 : Added VAROK argument (DJA)
*
*    Type Definitions :
*
      IMPLICIT  NONE           ! No default typing allowed
*
*    Global constants :
*
      INCLUDE  'SAE_PAR'
      INCLUDE  'QUAL_PAR'
*
*    Import :
*
      INTEGER
     :  IDIM1, IDIM2,                       ! Dimensions of input arrays
     :  XOFSET,                             ! Offsets of this input array
     :  YOFSET,                             ! from the first array in pixels.
     :  ODIM1, ODIM2                        ! Dimensions of output arrays
      LOGICAL                VAROK              ! Variance present?

      REAL
     :  INARR( IDIM1, IDIM2 ),              ! Input data array
     :  INVAR( IDIM1, IDIM2 )               ! Input variance array

      BYTE
     :  INQUAL( IDIM1, IDIM2 ),             ! Input quality array
     :  BADBITS                             ! Mask for testing quality against.

*    Import - Export :

      REAL
     :  OUTARR( ODIM1, ODIM2 ),             ! Output data array
     :  OUTVAR( ODIM1, ODIM2 )              ! Output variance array

      REAL
     :  MASK( ODIM1, ODIM2 )                ! Contains the number of pixels
c                                           ! which have been summed in each
c                                           ! element in the output array.
      BYTE
     :  OUTQUAL( ODIM1, ODIM2 )             ! Output quality array

*    Status :

      INTEGER  STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local variables :
*
      INTEGER
     :  X,                     ! x position of pixel in output array
     :  Y,                     ! y    "      "   "    "    "     "
     :  I, J                   ! array counters

      LOGICAL
     :  LBAD                   ! Is this pixel bad ?
*-

*    Check status on entry - return if not ok
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Loop round each pixel in input array
         DO  J  =  1, IDIM2
            Y  =  J + YOFSET

            DO  I  =  1, IDIM1

*             Find position of current pixel in (bigger) output array
               X  =  I + XOFSET

*             If the current input pixel value is valid then
               LBAD = (BIT_ANDUB(INQUAL(I,J),BADBITS).NE.QUAL__GOOD)

               IF ( .NOT. LBAD ) THEN

*                Update array values and set quality good. This assumes
*                output arrays are initially zeroed.
                  OUTARR(X,Y) = OUTARR(X,Y) + INARR(I,J)
                  IF ( VAROK ) OUTVAR(X,Y) = OUTVAR(X,Y) + INVAR(I,J)

                  MASK(X,Y) = MASK(X,Y) + 1.0

                  OUTQUAL(X,Y) = QUAL__GOOD

*              Else if we have a bad pixel and the mask is still set to
*              0

               ELSE IF ( MASK( X, Y ) .EQ. 0.0 ) THEN

*                This is a valid input position (i.e. not overlapped),
*                but the input value is bad, and there has been no
*                previous data added to the point - thus set the quality
*                array value to the datum missing value. If good data is
*                subsequently available for this output position, the
*                code above will include it in the correct fashion,
*                deleting the bad value inserted here. Subsequent
*                bad pixels at the point will be ignored because the
*                mask will not be zero.
                  OUTQUAL( X, Y )  =  QUAL__MISSING

*             End of if-input-value-is-ok check
               END IF

*          End of loops for each pixel in the input array
            END DO
         END DO
      END IF

*    return and end
      END


*+  IMOSAIC_MOSCDV - normalise mosaic 2-d data array with respect to mask values
      SUBROUTINE IMOSAIC_MOSCDV( DIM1, DIM2, MASK, ARRAY,
     :                         VAROK, VAR, QUAL, STATUS )
*
*    Description :
*
*     This routine takes a 2-d data array containing a mosaic of data
*     arrays and normalises it with respect to a mask. The value of a
*     given pixel in the mask array gives the number of valid images
*     that contributed 'flux' to that pixel in the data array. Thus,
*     this routine divides each pixel by the corresponding mask-array
*     value to normalise.
*     It also sets the quality of the pixel bad if the mask value for
*     that element is zero.
*
*    Method :
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm J. Currie RAL ( RAL::CUR )
*
*    History :
*
*     20 Nov 86 : First implementation (HILO::MJM)
*     31 May 88 : Renamed from MOSAIC_DIV, and tidied to KAPPA
*                 conventions ( RAL::CUR )
*      1 Aug 89 : Renamed to MOSAIC_MOSCDV and input dimensions changed
*                 (LTVAD::RDS)
*     22 Aug 92 : Pixel quality set bad if mask element is zero (RDS)
*     27 Aug 93 : Variance processing made optional (DJA)
*
*    Type Definitions :
*
      IMPLICIT  NONE           ! no default typing allowed
*
*    Global constants :
*
      INCLUDE  'SAE_PAR'
      INCLUDE  'QUAL_PAR'
*
*    Import :
*
      INTEGER		     DIM1, DIM2         ! Dimensions of input arrays
      REAL                   MASK(DIM1,DIM2)	! mask array
      LOGICAL                VAROK              ! Variance present?
*
*    Import - Export :
*
      REAL		     ARRAY(DIM1,DIM2)	! Data array to be normalised
      REAL		     VAR(DIM1,DIM2)	! Data array to be normalised
      BYTE		     QUAL(DIM1,DIM2)	! Quality array
*
*    Status :
*
      INTEGER  		     STATUS 		! Global status parameter
*
*    Local variables :
*
      INTEGER                I, J 		! Array counters
*-

*    Check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop round each line of the input data array
      DO  J  =  1, DIM2

*      Loop round each pixel of the current line
        DO  I  =  1, DIM1

*        Is it worth dividing ?
          IF ( MASK(I,J) .GT. 1 ) THEN

*          Normalise the data-array pixel value and variance
            ARRAY(I,J)  =  ARRAY(I,J) / MASK(I,J)

*        Set pixel bad if mask contains zero
          ELSE IF ( MASK(I,J) .LT. 1 ) THEN

            ARRAY(I,J) = 0.0
            QUAL(I,J) = QUAL__MISSING

          END IF

*      End of loop round pixels in current line of input data array
        END DO

*    End of loop round all lines of input data array
      END DO

*    Variance present?
      IF ( VAROK ) THEN

*      Loop round each line of the input variance array
        DO  J  =  1, DIM2

*        Loop round each pixel of the current line
          DO  I  =  1, DIM1

*          Set variance for good pixels
            IF ( QUAL(I,J) .EQ. QUAL__GOOD ) THEN
              VAR(I,J)  =  VAR(I,J) / ( MASK(I,J) * MASK(I,J) )
            END IF

*        End of loop round pixels in current line of input variance array
          END DO

*      End of loop round all lines of input data array
        END DO

*    End of variance-present-test
      END IF

*    Return and end
      END
