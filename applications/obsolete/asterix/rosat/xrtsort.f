*+  XRTSORT - Sorts XRT data into ASTERIX output file(s)
      SUBROUTINE XRTSORT( STATUS )
*    Description :
*     Program to create an ASTERIX data set using
*     ROSAT XRT pointed phase data
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*    History :
*   Oct 1988 Original
*   Mar 1990 Real version
*   Apr 1991 Version 1.3-3     Includes radial spectra and time series.
*                              also supports elliptical boxes
*   Jun 1991 Version 1.3-4     Fixes a bug involving image axes
*   Jul 1991 Version 1.3-5     BASE_TAI redefined, it is now 10 seconds
*                              less than it was before
*   Oct 1991 Version 1.3-6     Fixed a bug giving an incorrect value for
*                              seconds of DEC in the pointing position.
*   Nov 1991 Version 1.5-1     Now sorts HRI data
*   Nov 1991 Version 1.5-2     Fixes bug in sorting elliptical boxes
*   Jan 1992 VERSION 1.5-3     Uses multiple time windows
*   Feb 1992 VERSION 1.5-4     Rounding error in start and stop times
*                              fixed.
*   Mar 1992 Version 1.5-5     Fixed a bug in background event sorting
*   May 1992 Version 1.5-6     Avoids duplicating an event at the edge
*                              of each small map.
*   Jan 1993 V1.6-1/2          Now sorts US FITS data.
*   MAY 1993 V1.6-4            Sets the default Corr. PHA channel to 256
*                              maximum. Uses dub. prec. for the time
*                              event lists
*   2 AUG 93 V1.6-5            Fix for US/HRI (RJV)
*  17 Sep 93 V1.6-6            Bug in FX_ fixed (RJV)
*  29 Nov 93 V1.6-7            Now sorts rational FITS files(JKA).
*  18 Feb 94 V1.6-8            Now uses EXTNAME convention (INC_RDF)
*   8 Apr 94 V1.6-9            Bug fix in image orientation for MPE-sdf
*  24 Apr 94 (v1.7-0) For new asterix release
*  25 May 94 (v1.7-1) PSF structure added in HDS output files
*  25 Aug 95 V1.8-0   Bug fix for event datasets (RJV)
*  11 Sep 95 V1.8-1   OMD support removed (RJV)
*  20 Dec 95 V2.0-0   ADI port (DJA)
*   5 Apr 98 V2.2-1   Structures removed (RJV)
*  24 Jan 99 V2.3-0   FITS file input. Bug in background file. (DGED)

*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
      INCLUDE 'XRTSRT_CMN'
*
*    Status :
      INTEGER                 STATUS
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local Constants :

      CHARACTER*80		ELISTS
        PARAMETER		( ELISTS = 'X_CORR,Y_CORR,X_DET,'/
     :           /'Y_DET,RAW_TIMETAG,PULSE_HEIGHT_CH,CORR_PH_CH' )
      CHARACTER*30            VERSION
         PARAMETER          ( VERSION = 'XRTSORT Version 2.3-0' )

*    Local variables :


      INTEGER                 SID               ! ID of source output dataset
      INTEGER                 BID               ! ID of bckgrnd output data
      INTEGER                 SDIM(7)           ! Dimensions of source array
      INTEGER                 BDIM(7)           ! Dimensions of bckgnd array
      INTEGER                 MDIM(2)		! Dimensions of spatial mask
      INTEGER                 NRBIN             ! Number of output radial bins
      INTEGER                 NAZBIN            ! Number of output azim. bins
      INTEGER                 SRCPTR            ! Pointer to mapped source array
      INTEGER                 BCKPTR            ! Pointer to background array
      INTEGER                 SQPTR             ! Pointer to source quality
      INTEGER                 BQPTR             ! Pointer to bckgnd quality
      INTEGER                 SMPTR             ! Pointer to src image mask
      INTEGER                 BMPTR             ! Pointer to bckgnd image mask
      INTEGER                 S2MPTR,B2MPTR     ! Secondary masks
      INTEGER                 WPNTR1,WPNTR2     ! Pointer to workspace arrays

      REAL                    MRES		! Resolution of spatial mask

      INTEGER                 BLOCK
      INTEGER                 IUNIT             ! Logical I/O unit
      INTEGER                 MAXRAW            ! Max value
        PARAMETER (MAXRAW = 500)
      INTEGER                 NFILES            ! Number of files
*
      CHARACTER*100           FILES(MAXRAW)     ! File name array
      CHARACTER*132           FITSDIR           ! Directory for FITS
      CHARACTER*132           FROOT             ! Root of FITS filename
      CHARACTER*5             ORIGIN            ! Origin of FITS file

*-
      CALL MSG_PRNT(VERSION)
      CALL MSG_PRNT( 'XRTSORT : For FITS/RDF files."' )
*
      CALL AST_INIT()
*
*  Get input file details
*  Get the current working directory
      CALL UTIL_GETCWD( FITSDIR, STATUS )
      CALL USI_DEF0C( 'RAWDIR', FITSDIR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      CALL USI_GET0C( 'RAWDIR', FITSDIR, STATUS )
*  Any FITS files?
      CALL UTIL_FINDFILE(FITSDIR, '*.fits', MAXRAW, FILES, NFILES,
     :                                                       STATUS)
*  If no files - exit
      IF (NFILES .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_PRNT ('XRTSORT : Error - No FITS file found')
         CALL MSG_PRNT ('XRTSORT : Uses RDF FITS files only.')
         CALL MSG_PRNT ('XRTSORT : Please use VERSION V2.2-1 for SDF'
     :                                                //'file input')
         GOTO 999
      END IF
*
*  Get root name of FITS file
      CALL USI_GET0C ('ROOTNAME', FROOT, STATUS )
*  Append extension of FITS extension containing header
      SRT_ROOTNAME = FROOT(1:CHR_LEN(FROOT)) // '_bas.fits'
*  Does file exist?
      CALL UTIL_FINDFILE(FITSDIR, SRT_ROOTNAME, MAXRAW, FILES, NFILES,
     :                                                       STATUS)
      IF (NFILES .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_PRNT ('XRTSORT : Error - Header file not found')
         GOTO 999
      END IF

      CALL MSG_PRNT('XRTSORT : Using FITS file : '// SRT_ROOTNAME)

*  Open the FITS file
      CALL FIO_GUNIT(IUNIT, STATUS)
      CALL FTOPEN(IUNIT, SRT_ROOTNAME, 0, BLOCK, STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
	 CALL MSG_SETC('FNAM',SRT_ROOTNAME)
         CALL MSG_PRNT('XRTSORT : Error - opening file ^FNAM **')
         GOTO 999
      ENDIF
*
      ORIGIN = 'RDF'
*  Read in FITS header
      CALL RAT_RDHEAD(IUNIT, ORIGIN, STATUS)
*
      IF ( STATUS .NE. SAI__OK ) GOTO 999
*
*  Close FITS files
      CALL FTCLOS(IUNIT, STATUS)
      CALL FIO_PUNIT(IUNIT, STATUS)
*
*  Establish DTYPE (required for range selection). Using 'BinDS' only
      SRT_DTYPE='BinDS'
*
*  Get the sort control information from the user
      CALL XRTSORT_RANGESELECT(SDIM,BDIM,NRBIN,NAZBIN,
     :                       MDIM,MRES,SMPTR,BMPTR,STATUS)
*
*  Create output files
      CALL USI_CREAT( 'OUT', ADI__NULLID, SID, STATUS )
      IF ( SRT_BCKGND ) THEN
        CALL USI_CREAT( 'BOUT', ADI__NULLID, BID, STATUS )
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GOTO 999
*
*  Create the output data set and get a pointer to mapped data array.
      CALL XRTSORT_CRE_BINNED(  1, SID, SDIM, NRBIN,
     :                          SRCPTR, SQPTR, S2MPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999
*
*  Create a background dataset if required
      IF ( SRT_BCKGND ) THEN
        CALL XRTSORT_CRE_BINNED(  2, BID, BDIM, 1,
     :                            BCKPTR, BQPTR, B2MPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 999

      ELSE
*
*  Dummy up a couple of arrays.
         CALL DYN_MAPR(7,BDIM,BCKPTR,STATUS)
         CALL DYN_MAPB(7,BDIM,BQPTR,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
           CALL MSG_PRNT('XRTSORT : Error - mapping dynamic arrays')
           GOTO 999
         ENDIF
       ENDIF
*
*     Map a couple of workspace arrays
        CALL DYN_MAPR(1, NRBIN, WPNTR1, STATUS)
        CALL DYN_MAPR(1, NRBIN, WPNTR2, STATUS)
        IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_PRNT('XRTSORT : Error - mapping dynamic arrays')
          GOTO 999
        ENDIF
*
*    Sort the data
      CALL XRTSORT_SORT_BIN(SDIM(1), SDIM(2),
     :        SDIM(3), SDIM(4), SDIM(5), SDIM(6), SDIM(7),
     :        BDIM(1), BDIM(2), BDIM(3), BDIM(4), BDIM(5), BDIM(6),
     :        BDIM(7), NRBIN, NAZBIN, MDIM(1), MDIM(2),
     :        MRES,%val(SMPTR),%val(BMPTR),%val(S2MPTR),%val(B2MPTR),
     :        %val(WPNTR1), %val(WPNTR2), %val(SRCPTR),
     :        %val(BCKPTR), %val(SQPTR), %val(BQPTR), STATUS)
*
*  Release workspace
      CALL DYN_UNMAP(WPNTR1,STATUS)
      CALL DYN_UNMAP(WPNTR2,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*  Put SORT box into output files
      CALL XRTSORT_WRISORT( SID, 1, STATUS )

*  Background sort box
      IF (SRT_BCKGND) THEN
         CALL XRTSORT_WRISORT(BID,  2, STATUS)
      ENDIF

*   History
      CALL HSI_NEW(SID, STATUS)
      CALL HSI_ADD(SID, VERSION, STATUS)

      IF (SRT_BCKGND) THEN
         CALL HSI_NEW(BID, STATUS)
         CALL HSI_ADD(BID, VERSION, STATUS)
      ENDIF

*  Tidy up
 999  CALL USI_ANNUL('OUT',STATUS)
      IF (SRT_BCKGND) THEN
         CALL USI_ANNUL('BOUT',STATUS)
      ENDIF
      CALL AST_CLOSE()

      END


*+ XRTSORT_AXES - Writes the axes info into the output datafile
	SUBROUTINE XRTSORT_AXES(FID,NELS,NRBIN,NAXES,AXES,HIGH,LOW,
     :                                      BASE,SCALE,UNITS,STATUS)
* Description :
*        This routine writes axes structures into an output datafile
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     6 Nov 88: original (LTVAD::RDS)            based on EXOLESORT_AXES
*    17 Apr 91  now copes with a radial axis    (LTVAD::RDS)
*    18 Dec 1995 : Use new BDI routines (DJA)
*
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Import :
      INTEGER			FID	! The output file

      INTEGER NELS(7)                   !Number of elements in each dimension
      INTEGER NRBIN                     !Number of radial bins
      INTEGER NAXES                     !Number of axes of output array
      INTEGER AXES(8)                   !Code for each axis (1-8)
      REAL HIGH(8),LOW(8)               !Extreme values for each dimension
      REAL BASE(8)                      ! Zero point for each axis
      REAL SCALE(8)                     ! Scale value for each axis
      CHARACTER*(*) UNITS(8)            ! Axes units
* Export :
* Status :
      INTEGER STATUS
* Local variables :
      REAL			SPARR(2)		! Spaced array data
      CHARACTER*6 VARIANT(8)               ! The type of data-array
      INTEGER DIMENSION(8)                 ! Length of each axis
      LOGICAL NORMALISED(8)                ! Data normalised to this axis ?
      CHARACTER*30 LABEL(8)                ! Axes labels
      INTEGER PNTR,LP,AXLP
      INTEGER SELS(8)                      ! An array of dimensions
*-

*  Check status:
      IF (STATUS .NE. SAI__OK) RETURN

*  Create new element array
      DO LP=1,7
        SELS(LP) = NELS(LP)
      ENDDO
      SELS(8) = NRBIN

*  Set up units and labels for axes
      UNITS(1)='degrees'
      LABEL(1)='X position'
      UNITS(2)='degrees'
      LABEL(2)='Y position'
      UNITS(3)='pixels'
      LABEL(3)='Detector X'
      UNITS(4)='pixels'
      LABEL(4)='Detector Y'
      UNITS(5)='seconds'
      LABEL(5)='Time'
      UNITS(6)='channel no.'
      LABEL(6)='PHA channel'
      UNITS(7)='channel no'
      LABEL(7)='Corrected PHA channel'
      UNITS(8)='degrees'
      LABEL(8)='Radius'
*
      DO LP=1,8
         VARIANT(LP)='SPACED'
      ENDDO

*  Set arrays for output later
      DO LP=1,NAXES
*
        PNTR=AXES(LP)
*
          DIMENSION(LP)=SELS(PNTR)
          SCALE(LP)=(HIGH(PNTR)-LOW(PNTR))/SELS(PNTR)
          BASE(LP)=LOW(PNTR)+0.5*SCALE(LP)
          LABEL(LP)=LABEL(PNTR)
          UNITS(LP)=UNITS(PNTR)
*
          NORMALISED(LP)=.FALSE.
*
      ENDDO

*  Put in components of each axis
      DO AXLP = 1, NAXES

*    Axis text
        CALL BDI_AXPUT0C( FID, AXLP, 'Label', LABEL(AXLP), STATUS )
        CALL BDI_AXPUT0C( FID, AXLP, 'Units', UNITS(AXLP), STATUS )

*    Normalisation
        CALL BDI_AXPUT0L( FID, AXLP, 'Normalised', NORMALISED(AXLP),
     :                    STATUS )

*    Write axis data as a spaced array
        SPARR(1) = BASE(AXLP)
        SPARR(2) = SCALE(AXLP)
        CALL BDI_AXPUT1R( FID, AXLP, 'SpacedData', 2, SPARR, STATUS )

      END DO

*  Tidy up
      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'XRTSORT_AXES', STATUS )
      END IF

      END



*+ XRTSORT_CRE_ASTERIX - Create an ASTERIX structure
      SUBROUTINE XRTSORT_CRE_ASTERIX(OUTFID, IDS, STATUS )
*    Description :
*       Writes information into the .MORE.ASTERIX box.
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*    Authors :
*     Richard Saxton   (LTVAD::RDS)      from SORT_CRE_ASTERIX   by M.Denby
*    History :
*     9-Nov-1988       original
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'WCI_PAR'
*    Structure definitions :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
      INTEGER			OUTFID
      INTEGER IDS
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables:
      CHARACTER*7		UNITS(2)		! Axis units

      REAL                    	EXPO_TIM            	! Exposure time (s)
      REAL                    	OBSLEN              	! Observation length (s)time

      DOUBLE PRECISION		DPOINT(2)		! WCS pointing dir'n
      DOUBLE PRECISION	        STAI			! TAI at obs start

      INTEGER			DETID			! Mission strings
      INTEGER                 	LOOP,LOOP2          	! Loop counter
      INTEGER			PIXID, PRJID, SYSID	! WCS data
      INTEGER			TIMID			! Timing info

*  Local Data:
      DATA			UNITS/'degree','degree'/
*-

*  Check status - return if bad
      IF (STATUS .NE. SAI__OK) RETURN

*  Set corrections flags
      CALL XRT_CREPROC( OUTFID, .FALSE., .FALSE., .FALSE., STATUS )

*  Mission strings describing the observation
      CALL DCI_NEW( 'ROSAT', 'XRT', HEAD_DETECTOR, HEAD_FILTER,
     :              HEAD_TARGET, HEAD_OBSERVER, DETID, STATUS )
      CALL DCI_IPUT0R( DETID, 'PIXEL_SIZE', HEAD_PIXEL, STATUS )
      CALL DCI_IPUT0D( DETID, 'SC_BASE', HEAD_BASE_SCTIME, STATUS )
      CALL DCI_IPUT0D( DETID, 'SC_CONV', HEAD_SCCONV, STATUS )
      CALL DCI_IPUT0C( DETID, 'RAWDATA', HEAD_ORIGIN, STATUS )
      CALL DCI_IPUT0C( DETID, 'SASS_VERSION', HEAD_SASS_DATE, STATUS )

*  World coordinates
*   Pixellation
      IF (HEAD_ROLLCI .LT. -180.D0) HEAD_ROLLCI = HEAD_ROLLCI + 360.D0
      IF (HEAD_ROLLCI .GT.  180.D0) HEAD_ROLLCI = HEAD_ROLLCI - 360.D0
      CALL WCI_NEWPX( 0, 0.0, 0.0, UNITS, -HEAD_ROLLCI, PIXID, STATUS )
*   Projection
      DPOINT(1) = HEAD_AXIS_RA
      DPOINT(2) = HEAD_AXIS_DEC
      CALL WCI_NEWPRJ( 'TAN', 0, 0.0, DPOINT, 180.0D0, PRJID, STATUS )
      DPOINT(1) = SRT_FIELD_RA(IDS)
      DPOINT(2) = SRT_FIELD_DEC(IDS)
      CALL ADI_CPUT1D( PRJID, 'NPOINT', 2, DPOINT, STATUS )
*   Coordinate system
      CALL WCI_NEWSYS( 'FK5', 2000.0, WCI__FLAG, SYSID, STATUS )

*  Timing information. Time frame is spacecraft local frame
      CALL TCI_NEW( 'LOCAL', TIMID, STATUS )

*  Observation start
      CALL ADI_CPUT0D( TIMID, 'MJDObs', HEAD_BASE_MJD, STATUS )

*  Convert to atomic time
      CALL TCI_MJD2TAI( HEAD_BASE_MJD, STAI )
      CALL ADI_CPUT0D( TIMID, 'TAIObs', STAI, STATUS )

*  Calculate observation length
      OBSLEN = 0.0
      DO LOOP = 1, SRT_NTIME(IDS)
        OBSLEN = OBSLEN + SRT_MAX_T(LOOP,IDS) - SRT_MIN_T(LOOP,IDS)
      END DO
      CALL ADI_CPUT0R( TIMID, 'ObsLength', OBSLEN, STATUS )

*  Calculate exposure time and write to header
      EXPO_TIM=0.0
      DO LOOP=1,HEAD_NTRANGE
        DO LOOP2=1,SRT_NTIME(IDS)
          IF ( SRT_MIN_T(LOOP2,IDS) .LT. HEAD_TEND(LOOP) .AND.
     :          SRT_MAX_T(LOOP2,IDS) .GT. HEAD_TSTART(LOOP)) THEN
            EXPO_TIM = EXPO_TIM +
     :           ( MIN( HEAD_TEND(LOOP), SRT_MAX_T(LOOP2,IDS) ) -
     :             MAX( HEAD_TSTART(LOOP), SRT_MIN_T(LOOP2,IDS) ) )
          ENDIF
        ENDDO
      ENDDO
      CALL ADI_CPUT0R( TIMID, 'Exposure', EXPO_TIM, STATUS )

*  Create elements in the livetime extension
c      CALL ADI_CPUT1D( TIMID, 'LiveOn', SRT.NWINS, SRT.WST, STATUS )
c      CALL ADI_CPUT1D( TIMID, 'LiveOff', SRT.NWINS, SRT.WET, STATUS )

*  Write stuff to file
      CALL TCI_PUTID( OUTFID, TIMID, STATUS )
      CALL WCI_PUTIDS( OUTFID, PIXID, PRJID, SYSID, STATUS )
      CALL DCI_PUTID( OUTFID, DETID, STATUS )

*  Create elements in the PSF extension
c      CALL HDX_PUTC( PSF, 'LIBRARY_NAME', 1, 'PSFLIB', STATUS)
c      IF (HEAD_DETECTOR .EQ. 'HRI') THEN
c         CALL HDX_PUTC( PSF, 'ROUTINE_NAME', 1, 'XRT_HRI', STATUS)
c      ELSE
c         CALL HDX_PUTC( PSF, 'ROUTINE_NAME', 1, 'XRT_PSPC', STATUS)
c      ENDIF

*  Check status
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_REXIT( 'XRTSORT_CRE_ASTERIX', STATUS )
      END IF

      END


*+  XRTSORT_CRE_BINNED - Create output binned dataset
      SUBROUTINE XRTSORT_CRE_BINNED(IDS, OUTFID, SDIM, NRBIN,
     :                                    ARRPTR, QPTR, MPTR, STATUS)
*    Description :
*    Environment parameters :
*       OUTPUT             UNIV             Name of binned output file
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     R.D.Saxton      (LTVAD::RDS)
*    History :
*     7-Nov-1988    original
*     8-Apr-1994    parameter LMPE removed, now checks HEAD_YSTART (JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PAR_ERR'
*    Structure definitions :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
      INTEGER IDS                               ! 1=source 2=background
      INTEGER			OUTFID			! Output file id
      INTEGER SDIM(7)                           ! Dimensions of binned axes
      INTEGER NRBIN                             ! Number of radial bins
*    Import-Export :
*    Export :
      INTEGER ARRPTR                            ! Pointer to mapped array
      INTEGER QPTR                              ! Pointer to quality array
      INTEGER MPTR				! Pointer to mask array
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*15  TYPE
      CHARACTER*40 UNITS(8)
      INTEGER IDIMS(7)                          ! Dimensions of output array
      REAL LOW(8),HIGH(8)                       ! Range of each axis
      REAL BASE(8),SCALE(8)
      REAL PTOD                                 ! Pixels to degrees conversion
      INTEGER TOTELS,LP
*-

*  Check status - return if bad
      IF (STATUS .NE. SAI__OK) RETURN

*  Work out dataset type
      CALL XRT_GETTYPE(SRT_NAXES(IDS),SRT_BINAXIS(1,IDS),TYPE)

*  Find dimensions of output array from the SORT structure
      TOTELS = 1
      DO LP = 1, SRT_NAXES(IDS)
        IF (SRT_BINAXIS(LP,IDS) .NE. 8) THEN
          IDIMS(LP)=SDIM(SRT_BINAXIS(LP,IDS))
        ELSE
          IDIMS(LP)=NRBIN
        ENDIF
        TOTELS=TOTELS*IDIMS(LP)
      ENDDO

*  Link to interface object
      CALL BDI_LINK( 'BinDS', SRT_NAXES(IDS), IDIMS, 'REAL', OUTFID,
     :                                                       STATUS )
      CALL BDI_SETDST( OUTFID, TYPE, STATUS )

*  Write in units and title
      CALL BDI_PUT0C( OUTFID, 'Title', HEAD_TITLE, STATUS )
      CALL BDI_PUT0C( OUTFID, 'Units', 'counts', STATUS )

*  Create and map the data array
      CALL BDI_MAPR( OUTFID, 'Data', 'WRITE/ZERO', ARRPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error creating output data array')
        GOTO 99
      END IF

*  Create and map data quality
      CALL BDI_MAPUB( OUTFID, 'Quality', 'WRITE/QGOOD', QPTR, STATUS )
      CALL BDI_PUT0UB( OUTFID, 'QualityMask', QUAL__MASK, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Error creating output quality array' )
        GOTO 99
      ENDIF

*  Calculate maximum and minimum axis values
      PTOD = HEAD_PIXEL / 3600.0

*  Assume centre pixel is 0,0
      HIGH(1)= - (SRT_MAX_X(IDS) - HEAD_SKYCX) * PTOD
      LOW(1) = - (SRT_MIN_X(IDS) - HEAD_SKYCX) * PTOD
      IF (HEAD_YSTART.LT.0) THEN
        HIGH(2)= - (SRT_MIN_Y(IDS) - HEAD_SKYCY) * PTOD
        LOW(2) = - (SRT_MAX_Y(IDS) - HEAD_SKYCY) * PTOD
      ELSE
        LOW(2)= (SRT_MIN_Y(IDS) - HEAD_SKYCY) * PTOD
        HIGH(2) = (SRT_MAX_Y(IDS) - HEAD_SKYCY) * PTOD
      ENDIF
*
      HIGH(3)=REAL(SRT_MAX_XD(IDS)) + 0.5
      LOW(3) =REAL(SRT_MIN_XD(IDS)) - 0.5
      HIGH(4)=REAL(SRT_MAX_YD(IDS)) + 0.5
      LOW(4) =REAL(SRT_MIN_YD(IDS)) - 0.5
      HIGH(5)=SRT_MAX_T(SRT_NTIME(IDS),IDS)
      LOW(5) =SRT_MIN_T(1,IDS)
      HIGH(6)=REAL(SRT_MAX_PH(IDS)) + 0.5
      LOW(6) =REAL(SRT_MIN_PH(IDS)) - 0.5
      HIGH(7)=REAL(SRT_MAX_EN(IDS)) + 0.5
      LOW(7) =REAL(SRT_MIN_EN(IDS)) - 0.5
      HIGH(8)=REAL(SRT_ELAMAX(IDS)) * SRT_PTOD
      LOW(8) =REAL(SRT_ELAMIN(IDS)) * SRT_PTOD

*  Create and fill axis data
      CALL XRTSORT_AXES(OUTFID, SDIM,NRBIN,SRT_NAXES(IDS),
     :       SRT_BINAXIS(1,IDS),HIGH, LOW, BASE, SCALE, UNITS, STATUS)

*  Create sort mask for images
      IF ( SRT_IMAGE(IDS) ) THEN
        CALL DYN_MAPI( 2, SDIM, MPTR, STATUS )
        CALL ARX_MASK(SRT_ARDID(IDS),SDIM,BASE,SCALE,UNITS,%VAL(MPTR),
     :                                                     STATUS)
      END IF

*  Now create the ASTERIX box
      CALL XRTSORT_CRE_ASTERIX( OUTFID, IDS,  STATUS )

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'XRTSORT_CRE_BINNED', STATUS )
      END IF

      END


*+ XRTSORT_CRE_EVENT - Create & map the output event dataset
      SUBROUTINE XRTSORT_CRE_EVENT(IDS, MAPLIM,
     :                              OUTFID, DPTR, STATUS )
*    Description :
*     Creates & maps output event dataset
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton  (LTVAD::RDS)
*    History :
*     27/5/88:  Original (PLA)
*     6-MAY-1993  Uses double precision time lists (RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structures :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'

*    Import :
      INTEGER IDS
      INTEGER  MAPLIM                          ! Current mapping extent of lists
      INTEGER			OUTFID
*    Export :
      INTEGER                 DPTR(7)       ! Pointers to mapped lists
*    Status :
      INTEGER                 STATUS

*    Local variables :
      REAL MINX,MAXX,MINY,MAXY                 ! Min. and max. X values (arcmin)

      INTEGER			DLID(7)			! List identifiers
      INTEGER			I			! Loop over lists
*-

*  Check status
      IF ( STATUS .NE. SAI__OK) RETURN

*  Create interface object
      CALL EDI_LINK( 'EventDS', MAPLIM, HEAD_TITLE, OUTFID, STATUS )

*  Create min and max values in arcmins - note the reversal which seems
*  to be necessary to give correct value of FIELD_MIN/MAX in output
      MAXX = -(SRT_MIN_X(IDS) - HEAD_SKYCX) * SRT_PTOD * 60.0
      MINX = -(SRT_MAX_X(IDS) - HEAD_SKYCX) * SRT_PTOD * 60.0
      IF (HEAD_YSTART .LT. 0) THEN
        MAXY = -(SRT_MIN_Y(IDS) - HEAD_SKYCY) * SRT_PTOD * 60.0
        MINY = -(SRT_MAX_Y(IDS) - HEAD_SKYCY) * SRT_PTOD * 60.0
      ELSE
        MINY = (SRT_MIN_Y(IDS) - HEAD_SKYCY) * SRT_PTOD * 60.0
        MAXY = (SRT_MAX_Y(IDS) - HEAD_SKYCY) * SRT_PTOD * 60.0
      ENDIF

*  Define lists
      CALL EDI_CREL0R( OUTFID, 'X_CORR', .TRUE., MINX, MAXX, 0.0,
     :                 'arcmin', DLID(1), STATUS )
      CALL EDI_CREL0R( OUTFID, 'Y_CORR', .FALSE., MINY, MAXY, 0.0,
     :                 'arcmin', DLID(2), STATUS )
      CALL EDI_CREL0I( OUTFID, 'X_DET', .FALSE., SRT_MIN_XD(IDS),
     :       SRT_MAX_XD(IDS),1, 'pixels', DLID(3), STATUS )
      CALL EDI_CREL0I( OUTFID, 'Y_DET', .FALSE., SRT_MIN_YD(IDS),
     :       SRT_MAX_YD(IDS),1, 'pixels', DLID(4), STATUS )
      CALL EDI_CREL0D( OUTFID, 'RAW_TIMETAG', .FALSE.,
     :      DBLE(SRT_MIN_T(1,IDS)), DBLE(SRT_MAX_T(SRT_NTIME(IDS),IDS)),
     :                 0.0D0, 'seconds', DLID(5), STATUS )
      CALL EDI_CREL0I( OUTFID, 'PULSE_HEIGHT_CH', .FALSE.,
     :               SRT_MIN_PH(IDS), SRT_MAX_PH(IDS), 1, 'channel no.',
     :                 DLID(6), STATUS )
      CALL EDI_CREL0I( OUTFID, 'CORR_PH_CH', .FALSE.,
     :               SRT_MIN_EN(IDS), SRT_MAX_EN(IDS), 1, 'channel_no',
     :                 DLID(7), STATUS )

*  Create lists
      DO I = 1, 7
        CALL EDI_CREAT( OUTFID, DLID(I), STATUS )
      END DO

*  Map them
      CALL EDI_MAPR( OUTFID, 'X_CORR,Y_CORR', 'WRITE', 0, 0, DPTR(1),
     :               STATUS )
      CALL EDI_MAPI( OUTFID, 'X_DET,Y_DET', 'WRITE', 0, 0, DPTR(3),
     :               STATUS )
      CALL EDI_MAPD( OUTFID, 'RAW_TIMETAG', 'WRITE', 0, 0, DPTR(5),
     :               STATUS )
      CALL EDI_MAPI( OUTFID, 'PULSE_HEIGHT_CH,CORR_PH_CH', 'WRITE',
     :               0, 0, DPTR(6), STATUS )

*  Initialise lists
      CALL ARR_INIT1R( 0.0, MAPLIM, %val(DPTR(1)),STATUS )
      CALL ARR_INIT1R( 0.0, MAPLIM, %val(DPTR(2)),STATUS )
      CALL ARR_INIT1I( 0, MAPLIM, %val(DPTR(3)),STATUS )
      CALL ARR_INIT1I( 0, MAPLIM, %val(DPTR(4)),STATUS )
      CALL ARR_INIT1D( 0.0D0, MAPLIM, %val(DPTR(5)), STATUS )
      CALL ARR_INIT1I( 0, MAPLIM, %val(DPTR(6)),STATUS )
      CALL ARR_INIT1I( 0, MAPLIM, %val(DPTR(7)),STATUS )

*  Create ASTERIX structure
      CALL XRTSORT_CRE_ASTERIX( OUTFID, IDS,  STATUS )

*  Tidy up
 99   IF (STATUS .NE. SAI__OK) THEN
	CALL AST_REXIT( 'XRTSORT_CRE_EVENT', STATUS )
      END IF

      END


*+ XRTSORT_FILESELECT - Select XRT observations to sort
	SUBROUTINE XRTSORT_FILESELECT(STATUS)
* Description :
*         This routine allows the user to select a list of files from
*   which data can be extracted.
*
* Environment parameters :
*      RAWDIR   char.       Directory for raw files
*      ROOTNAME char.       Rootname of files
*      LISTDIR  logical     LIst files on directory ?
*      INDEX    char.       Index file into events
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*    20-APR-1990         Original
*     9-Mar-1993         Uses portable C routine to fine current directory
*     8-Dec-1993	 Looks for <rtn>_hdr file. sets HEAD_ORIGIN = 'OMD'
*                        if old-style .OMD files found
*    18-Jan-1994         uses INC_RDF to determin filenaming conventions
*
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'AST_SYS_PAR'
* Structure definitions :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
* Import :
* Import-Export :
* Export :
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
* Local constants :
      INTEGER MAXRAW,MAXRANGE
      PARAMETER (MAXRAW=500,MAXRANGE=10)
* Local variables :
        CHARACTER*80 RAWDIR                     ! Directory for Raw files
        CHARACTER*30 RTNAME                     ! Rootname for files
        CHARACTER*100 FILE(MAXRAW)              ! Names of files in dir.
        INTEGER NFILES                          ! No of files in dir.
        LOGICAL CURR				! use current directory
        INTEGER K
        INTEGER LP
*-

* Check status :
        IF (STATUS .NE. SAI__OK) RETURN

* Find the name of the present directory and set as a default
        CALL UTIL_GETCWD(RAWDIR, STATUS)

* Annul the status variable if the current directory couldn't be found
        IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)

* Get directory name
        CALL USI_GET0L('CURR',CURR,STATUS)
        IF (.NOT.CURR) THEN
          CALL MSG_BLNK()
          CALL USI_DEF0C('RAWDIR', RAWDIR, STATUS)
          CALL USI_GET0C('RAWDIR', RAWDIR, STATUS)
        ENDIF
        CALL MSG_BLNK()

	IF (STATUS .NE. SAI__OK) GOTO 999

* Look for the header file
        CALL UTIL_FINDFILE(RAWDIR, '*_hdr.sdf', MAXRAW, FILE,
     &                                         NFILES, STATUS)

* Test if file is found
        IF (NFILES.LE.0) THEN
          STATUS=SAI__ERROR
          CALL MSG_PRNT('AST_ERR:No header files in this directory')
          GOTO 999

        ELSE


          CALL MSG_PRNT('The following header files are present:')
          DO LP=1,NFILES
            CALL STR_ROOT(FILE(LP),RTNAME)
            CALL MSG_SETC('FILE', RTNAME)
            CALL MSG_PRNT(' ^FILE')
          ENDDO
          CALL MSG_BLNK()

*  Get root-name
          CALL STR_ROOT(FILE(1), RTNAME)
          K = INDEX(RTNAME, '_hdr.')
          CALL USI_DEF0C('ROOTNAME', RTNAME(1:K-1), STATUS)
          CALL USI_GET0C('ROOTNAME', RTNAME, STATUS)
*
          IF (STATUS .NE. SAI__OK) GOTO 999
*
* Create rootname + directory string
          SRT_ROOTNAME = RAWDIR(1:CHR_LEN(RAWDIR))//FIL_SEP_CH//RTNAME
*
* Check if any files are present
          CALL UTIL_FINDFILE(RAWDIR,RTNAME(1:CHR_LEN(RTNAME))//'*',
     &                                   MAXRAW,FILE,NFILES,STATUS)
          IF (NFILES .EQ. 0) THEN
             CALL MSG_PRNT(
     &             'AST_ERR: no files found with that root-name')
             STATUS=SAI__ERROR
             GOTO 999
          ENDIF

        ENDIF

*  Tidy up
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT('XRTSORT_FILESELECT' ,STATUS)
      END IF

      END

*+XRTSORT_CRE_MASK - Create spatial mask
      SUBROUTINE XRTSORT_CRE_MASK(ARDID,RESFACT,MDIM,MRES,MPTR,
     :                                                     STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
      INTEGER ARDID
      INTEGER RESFACT
*    Import-Export :
*    Export :
      INTEGER MDIM(2)				! Dimensions of mask
      REAL MRES					! Resolution of mask
      INTEGER MPTR				! Pointers to mask
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*40 UNITS(2)
      REAL BASE(2),SCALE(2)
*-
      IF (STATUS .EQ. SAI__OK) THEN

*  set resolution to factorof 8 raw pixels to avoid gigantic array size
        MRES=REAL(RESFACT)*8.0
        IF (HEAD_ORIGIN.EQ.'MPE') THEN
          MDIM(1)=INT(ABS(HEAD_XEND-HEAD_XSTART)/MRES)
          MDIM(2)=INT(ABS(HEAD_YEND-HEAD_YSTART)/MRES)
        ELSE
          MDIM(1)=HEAD_XEND/MRES
          MDIM(2)=HEAD_YEND/MRES
        ENDIF
        SCALE(1)=-HEAD_PIXEL*MRES/3600.0
        SCALE(2)=HEAD_PIXEL*MRES/3600.0
        BASE(1)=-MDIM(1)/2*SCALE(1)+SCALE(1)/2.0
        BASE(2)=-MDIM(2)/2*SCALE(2)+SCALE(2)/2.0
        UNITS(1)='degrees'
        UNITS(2)='degrees'

        CALL DYN_MAPI(2,MDIM,MPTR,STATUS)
        CALL ARX_MASK(ARDID,MDIM,BASE,SCALE,UNITS,%val(MPTR),STATUS)

        IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ',' from XRTSORT_CRE_MASK',STATUS)
        ENDIF

      ENDIF

      END


*+XRTSORT_SCAN_MASK - Create spatial mask
      SUBROUTINE XRTSORT_SCAN_MASK(MDIM1,MDIM2,MASK,MRES,XC,YC,
     :                                   XMIN,XMAX,YMIN,YMAX,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'XRTHEAD_CMN'
*    Structure definitions :
*    Import :
      INTEGER MDIM1,MDIM2			! Dimensions of mask
      INTEGER MASK(MDIM1,MDIM2)
      REAL MRES
*    Import-Export :
*    Export :
      REAL XC,YC
      REAL XMIN,XMAX
      REAL YMIN,YMAX
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL BASE(2)
      REAL SCALE(2)
      REAL XTOT,YTOT
      REAL OFFSET
      REAL HX,HY
      INTEGER I,J
      INTEGER IXMIN,IXMAX
      INTEGER IYMIN,IYMAX
      INTEGER NPIX
*-
*   Check status - return if bad
      IF (STATUS .NE. SAI__OK) RETURN

      SCALE(1)=-HEAD_PIXEL*MRES/3600.0
      SCALE(2)=HEAD_PIXEL*MRES/3600.0
      BASE(1)=-MDIM1/2*SCALE(1)+SCALE(1)/2.0
      BASE(2)=-MDIM2/2*SCALE(2)+SCALE(2)/2.0

      IXMIN=MDIM1
      IXMAX=1
      IYMIN=MDIM2
      IYMAX=1
      XTOT=0.0
      YTOT=0.0
      NPIX=0

      DO J=1,MDIM2
        DO I=1,MDIM1

          IF (MASK(I,J).NE.0) THEN

            IXMIN=MIN(IXMIN,I)
            IXMAX=MAX(IXMAX,I)
            IYMIN=MIN(IYMIN,J)
            IYMAX=MAX(IYMAX,J)

            XTOT=XTOT+BASE(1)+REAL(I-1)*SCALE(1)
            YTOT=YTOT+BASE(2)+REAL(J-1)*SCALE(2)

            NPIX=NPIX+1

          ENDIF

        ENDDO
      ENDDO

      XC=XTOT/REAL(NPIX)
      YC=YTOT/REAL(NPIX)
      HX=0.5*SCALE(1)
      HY=0.5*SCALE(2)
      OFFSET=REAL(IXMIN-1)*SCALE(1)
      XMIN=BASE(1) +OFFSET -HX
      OFFSET=REAL(IXMAX-1)*SCALE(1)
      XMAX=BASE(1)+OFFSET+HX
      OFFSET=REAL(IYMIN-1)*SCALE(2)
      YMIN=BASE(2)+ OFFSET -HY
      OFFSET=REAL(IYMAX-1)*SCALE(2)
      YMAX=BASE(2)+ OFFSET +HY

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ',' from XRTSORT_SCAN_MASK',STATUS)
      ENDIF
*
      END



*+XRTSORT_PHOTONCNT   - Sorts XRT raw data into a binned data array
      SUBROUTINE XRTSORT_PHOTONCNT( MAXLIM, STATUS)
*    Description :
*        Finds the maximum number of photons which could be contained
*        in any event list given the sort ranges chosen. Note this number
*        is subsequently used to create the event lists and is a gross
*        over-estimation of the actual size needed.
*    History :
*     2-May-1990   original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Structures :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
*
*
*    Import-Export :
*    Export :
      INTEGER MAXLIM                              ! Maximum size of event lists
*    Local constants :
*    Local variables :
      INTEGER END_XMAP,END_YMAP                   ! Last pixel of current map
      INTEGER MAP                                 ! Map number
      INTEGER NINMAP
*-
*
* Zero the photon event counter
      MAXLIM=0
*
*   Loop over maps
      DO MAP=1,HEAD_ISMTNU
*
         END_XMAP = HEAD_XSMAP(MAP) + HEAD_IFDSZX/REAL(HEAD_ISMNUX)
         END_YMAP = HEAD_YSMAP(MAP) + HEAD_IFDSZY/REAL(HEAD_ISMNUY)
*
*     Check if the map is within either sort box
         IF ( (SRT_MIN_X(1) .LE. END_XMAP .AND. SRT_MAX_X(1) .GE.
     &      HEAD_XSMAP(MAP) .AND. SRT_MIN_Y(1) .LE. END_YMAP .AND.
     &      SRT_MAX_Y(1) .GE. HEAD_YSMAP(MAP)) .OR. (SRT_BCKGND
     &     .AND. SRT_MIN_X(2) .LE. END_XMAP .AND. SRT_MAX_X(2) .GE.
     &      HEAD_XSMAP(MAP) .AND. SRT_MIN_Y(2) .LE. END_YMAP .AND.
     &      SRT_MAX_Y(2) .GE. HEAD_YSMAP(MAP)) ) THEN
*
*     Add the number of photons in this map to the total
              IF (MAP .EQ. HEAD_ISMTNU) THEN
                 NINMAP = HEAD_IEVTNU - (HEAD_EVSTART(HEAD_ISMTNU)-1)
              ELSE
                 NINMAP = HEAD_EVSTART(MAP+1)-HEAD_EVSTART(MAP)
              ENDIF
*
              MAXLIM = MAXLIM + NINMAP
*
C              WRITE(*,*)MAP,NINMAP,MAXLIM
         ENDIF
*
      ENDDO           ! Loop over all maps
*
      END

*+XRTSORT_RANGESELECT	Obtains sorting options from the user
      SUBROUTINE XRTSORT_RANGESELECT(SDIM,BDIM,
     &              NRBIN,NAZBIN,MDIM,MRES,SMPTR,BMPTR,STATUS)
*
* Description :
*       Gets all sorting criteria from the user.
*
* Environment parameters :
*  DATASET              Output files required Event or Binned
*  RANGES               Property ranges to change
*                                   (x=1,y=2,xdet=3,ydet=4,t=5,s=6,e=7)
*  XSTART               Minimum value of X in degrees
*  XSTOP                Maximum value of X in degrees
*  YSTART               Minimum value of Y in degrees
*  YSTOP                Maximum value of Y in degrees
*  TSTART               Minimum value of T in seconds
*  TSTOP                Maximum value of T in seconds
*  MINPH                Minimum value of PHA channel
*  MAXPH                Maximum value of PHA channel
*  MINEN                Minimum value of energy
*  MAXEN                Maximum value of energy
*  BIN_AXES             Axes to use in the binned data array
*  SHAPE                Shape of the source and bckgnd areas
*  GRP_RA               RA of the centre of the source box
*  GRP_DEC              DEC of the centre of the source box
*  GRP_SWIDTH           Width of the source box
*  GRP_SHEIGHT          Height of the source box
*  RADIUS               Radius of source box if circular shape
*  NXBIN                X bin width for source and background (pix)
*  NYBIN                Y bin widths for source and bckgnd
*  TIMBIN               The width of the time bin
*  ENBIN                The width of the energy bin
*  PHBIN                The width of the Pha channel
* Method :
*                      Coordinate systems
*
*       MPE raw pixels         US/RDF raw pixels          axis coords
*
* -ve ------------------  +ve ------------------   +ve -------------------
*     |                |      |                |       |                 |
*     |                |      |                |       |                 |
*     |                |      |                |       |                 |
*  0  -                -      |                |   0.0 -                 -
*     |                |      |                |       |                 |
*     |                |      |                |       |                 |
*     |                |      |                |       |                 |
* +ve --------|---------    1 ------------------   -ve ---------|---------
*    -ve      0       +ve     1              +ve      +ve      0.0      -ve
*
*
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
*     Robert Vallance
* History :
*     Closely based on EXOLESORT_RANGESELECT   7 Dec 87: original (LTVAD::RDS)
*     Original 7-Nov 1988      (LTVAD::RDS)
*     Real one 10-Apr-1990      (LTVAD::RDS)
*     Allows a background annulus to be selected 10-Sep-1990      (LTVAD::RDS)
*     25 Aug 93: traps PHA channel=0 (RJV)
*      4 Apr 95: allows ARD input (RJV)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
* Structures:
      INCLUDE 'XRTSRT_CMN'                      ! Sorting struc. incl. MXTIME
      INCLUDE 'XRTHEAD_CMN'
* Import :
* Export :
      INTEGER SDIM(8)                           ! Dimensions of source array
      INTEGER BDIM(8)                           ! Dimensions of bckgnd array
      INTEGER NRBIN                             ! Number of radial bins
      INTEGER NAZBIN                            ! Number of azimuthal bins
      INTEGER MDIM(2)				! Dimensions of spatial masks
      REAL MRES					! Mask resolution
      INTEGER SMPTR,BMPTR			! Pointers to spatial masks
* Status :
      INTEGER STATUS
* Local constants :
        DOUBLE PRECISION DTOR,RTOD
	    PARAMETER (DTOR=3.14159265/180.,RTOD=1.0/DTOR)
        INTEGER MAXRAW
            PARAMETER (MAXRAW=500)
        INTEGER MXRNG
            PARAMETER (MXRNG=250)
        REAL TOL
            PARAMETER (TOL=1.0/3600)  ! Set tolerance to one arcsecond
* Local variables :
        CHARACTER*132 TIMSTRING       ! Time ranges
        CHARACTER*40 RAS,DECS	      ! RA/DEC in any format
        CHARACTER*30 C1,C2            ! Default time range
        CHARACTER CENTRE	      ! Option for region centre
        CHARACTER*10 RANGES,BIN_AXES  ! Axes codes for choosing range and data
        DOUBLE PRECISION RARAD        ! RA pointing direction in radians
        DOUBLE PRECISION DECRAD       ! DEC pointing direction in radians
        DOUBLE PRECISION DROLL        ! Roll angle of spacecraft (radians)
        DOUBLE PRECISION TMAT(3,3)    ! image axis to RA/DEC conversion matrix
        REAL SWIDTH,SHEIGHT   	      ! Size of source box
        REAL BWIDTH,BHEIGHT   	      ! Size of bckgnd box
        REAL SRAD1                    ! Inner radius of source box (pixels)
        REAL SRAD2                    ! Outer radius of source box (pixels)
        REAL BRADIN                   ! Inner radius of bckgnd annulus (degs)
        REAL BRADOUT                  ! Outer radius of bckgnd annulus (degs)
        REAL ELAMIN,ELAMAX            ! Elliptic 'X' axis min and max
        REAL ELBMAX                   ! Elliptic 'Y' axis max
        REAL EXMAX,EYMAX              ! Max. axis lengths for an ellipse
        REAL SMAXT                    ! Max. time
        REAL CBORDX,CBORDY            ! Default distance for box size
        REAL BBORDX,BBORDY            ! Border distance for bckgnd box
        REAL TIMBIN                   ! Bin width for time axis
        REAL PTOD                     ! Conversion of pixels to degrees
        REAL PTOR                     ! Conversion of pixels to radians
        REAL XW,YW		      ! Position in world coords
        REAL XW1,XW2,YW1,YW2
        REAL BXW,BYW		      ! Position in world coords
        REAL XWIDW,YWIDW,RADW	      ! Other parameters in world coords.
        REAL BXWIDW,BYWIDW      ! Other parameters in world coords.
        REAL IRADW,ORADW	      ! Inner and outer radii in world coords.
        REAL BIRADW,BORADW	      ! Inner and outer radii in world coords.
        REAL ANGLE,BANGLE	      ! Orientation  angle of ellipse
        INTEGER K1,K2                 !
        INTEGER BX_HWIDTH,BY_HWIDTH   ! Width of box in pixels
        INTEGER BCKGND_X,BCKGND_Y     ! Background box centre
        INTEGER X_HWIDTH,Y_HWIDTH     ! Widths of source box in pixels
        INTEGER SOURCE_X,SOURCE_Y     ! Centre of source box in pixels
        INTEGER NREBINX,NREBINY
        INTEGER NXBIN,NYBIN           ! No of X and Y pixel bins
        INTEGER NREBXD,NREBYD
        INTEGER NXDBIN,NYDBIN         ! No of X and Y detector bins
        INTEGER LP
        INTEGER ENBIN                 ! Bin width for energy axis
        INTEGER PHBIN                 ! Bin width for PHA channel axis
        INTEGER MFACT                 ! Y orientation factor (-1 or +1)
        INTEGER RES		      ! Resolution factor
        LOGICAL ANNULAR		      ! Background is annular
        LOGICAL JUMPOUT
*
* Statement functions :
        INTEGER NPIX
        REAL D
*
*       IPIX(D)=1024+NINT(D/PTOD)
*
        NPIX(D)=NINT(0.5*D/PTOD-0.5)
*
* Local data :
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN


*  get ID for ARD description of sort regions
      CALL ARX_OPEN('WRITE',SRT_ARDID(1),STATUS)
      CALL ARX_OPEN('WRITE',SRT_ARDID(2),STATUS)

* Set a factor for dealing with different orientations of y-axes in raw data
      IF (HEAD_ORIGIN.EQ.'MPE') THEN
         MFACT = -1
      ELSE
         MFACT = 1
      ENDIF
*
* Calculate pixel size in degrees
      PTOD = HEAD_PIXEL/3600.0
*
* Store pixel conversion factor
      SRT_PTOD = PTOD
*
* Calculate conversion factor from pixels to radians
      PTOR=PTOD*DTOR
*
* Generate the attitude matrix of the spacecraft
      RARAD =  HEAD_AXIS_RA * DTOR
      DECRAD = HEAD_AXIS_DEC * DTOR
*
*   Convert the roll angle to radians.
      DROLL = (HEAD_ROLLCI) * DTOR
*
      CALL CONV_GENDMAT(RARAD, DECRAD, DROLL, TMAT)
*
*
* Ask which properties will be used as binning axes in the output file.
      IF (SRT_DTYPE.EQ.'BinDS')THEN
*
        SRT_NAXES(1)=0
*
        CALL MSG_BLNK()
        CALL MSG_PRNT('Binned dataset selection')
        CALL MSG_PRNT('--------------------------')
        CALL MSG_PRNT('1 - XPIX')
        CALL MSG_PRNT('2 - YPIX')
        CALL MSG_PRNT('3 - XDET')
        CALL MSG_PRNT('4 - YDET')
        CALL MSG_PRNT('5 - Time')
        CALL MSG_PRNT('6 - PHA channel')
        CALL MSG_PRNT('7 - Corrected PHA channel')
        CALL MSG_PRNT('8 - Radial')
*
        CALL USI_GET0C('AXES', BIN_AXES, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
      ELSE
*
        BIN_AXES='          '
*
      ENDIF
*
*   Check that the axes chosen are consistent
      IF ( INDEX( BIN_AXES, '8') .NE. 0 .AND.
     &          (INDEX( BIN_AXES, '1') .NE. 0 .AND.
     &                        INDEX( BIN_AXES, '2') .NE. 0) ) THEN
*
        CALL MSG_PRNT(
     :         'AST_ERR: You cant have a radial and spatial axes')
        STATUS=SAI__ERROR
        GOTO 999
      ENDIF

* Get shape of sort area
      CALL USI_GET0C('SHAPE', SRT_SHAPE(1), STATUS)
      CALL CHR_UCASE(SRT_SHAPE(1))
      IF (INDEX('RCAEI', SRT_SHAPE(1)) .EQ. 0) THEN
        CALL MSG_PRNT('AST_ERR: unknown shape')
        STATUS=SAI__ERROR
        GOTO 999
      ENDIF

*  ARD description
      IF (SRT_SHAPE(1).EQ.'I') THEN

*  get ARD file
        CALL ARX_READ('ARD',SRT_ARDID(1),STATUS)

*  Get resolution factor
        CALL USI_GET0I('RES',RES,STATUS)
        IF (RES.LT.1.OR.RES.GT.3) THEN
          CALL MSG_PRNT('** Invalid resolution factor - using 2')
          RES=2
        ENDIF

*  Create spatial mask
        CALL MSG_PRNT('Creating spatial mask...')
        CALL XRTSORT_CRE_MASK(SRT_ARDID(1),RES,MDIM,MRES,SMPTR,STATUS)
        CALL MSG_PRNT('Done!')

        CALL XRTSORT_SCAN_MASK(MDIM(1),MDIM(2),%val(SMPTR),MRES,
     :                                  XW,YW,XW1,XW2,YW1,YW2,STATUS)


*   Get nominal centre of sort region in RA and DEC degrees
        CALL MSG_PRNT('Specify nominal centre of region:-')
        CALL MSG_PRNT('  1 Geometric centre')
        CALL MSG_PRNT('  2 Median position')
        CALL MSG_PRNT('  3 Optical axis')
        CALL MSG_PRNT('  4 Other')
        CALL USI_GET0C('CENTRE',CENTRE,STATUS)

        IF (CENTRE.EQ.'1') THEN
          CENTRE='MEAN'
          CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             SRT_FIELD_RA(1),SRT_FIELD_DEC(1),STATUS)
        ELSEIF (CENTRE.EQ.'2') THEN
          CENTRE='MEDIAN'
          XW=(XW1+XW2)/2.0
          YW=(YW1+YW2)/2.0
          CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             SRT_FIELD_RA(1),SRT_FIELD_DEC(1),STATUS)
        ELSEIF (CENTRE.EQ.'3') THEN
          CENTRE='AXIS'
          SRT_FIELD_RA(1)=HEAD_AXIS_RA
          SRT_FIELD_DEC(1)=HEAD_AXIS_DEC
          CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(1),SRT_FIELD_DEC(1),TMAT,
     :                                                     XW,YW,STATUS)
        ELSEIF (CENTRE.EQ.'4') THEN
          CENTRE='USER'
          CALL USI_GET0C('RA', RAS, STATUS)
          CALL USI_GET0C('DEC', DECS, STATUS)
          CALL CONV_RADEC(RAS,DECS,SRT_FIELD_RA(1),SRT_FIELD_DEC(1),
     :                                                        STATUS)
          CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(1),SRT_FIELD_DEC(1),
     :                                            TMAT,XW,YW,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid mode')
          STATUS=SAI__ERROR
          GOTO 999
        ENDIF


        X_HWIDTH=NPIX(ABS(XW2-XW1))
        Y_HWIDTH=NPIX(ABS(YW2-YW1))
C        SRT.PHI=0.0
C        SRT.ELAMIN=0.0
C        SRT.ELBMIN=0.0
c        SRT.ELAMAX=X_HWIDTH
c        SRT.ELBMAX=Y_HWIDTH


      ELSE

*   Set centre of the field as default
        WRITE(RAS,*) HEAD_AXIS_RA
        WRITE(DECS,*) HEAD_AXIS_DEC
        CALL USI_DEF0C('RA', RAS, STATUS)
        CALL USI_DEF0C('DEC', DECS, STATUS)

        CALL USI_GET0C('RA', RAS, STATUS)
        CALL USI_GET0C('DEC', DECS, STATUS)
        CALL CONV_RADEC(RAS,DECS,SRT_FIELD_RA(1),SRT_FIELD_DEC(1),
     :                                                       STATUS)
        CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(1),SRT_FIELD_DEC(1),TMAT,
     :                                              XW,YW,STATUS)

      ENDIF

      IF (STATUS .NE. SAI__OK) GOTO 999

*  Calculate the source position in raw pixels

      SOURCE_X = NINT( -XW / PTOD)
      SOURCE_Y = NINT( YW / PTOD) * MFACT

*  Add the sky pixel centre to the source centre
      SOURCE_X = SOURCE_X + HEAD_SKYCX
      SOURCE_Y = SOURCE_Y + HEAD_SKYCY

*  Find the closest distance to each border
      CBORDX=MIN(ABS(SOURCE_X-HEAD_XSTART),ABS(SOURCE_X-HEAD_XEND))
     &                                   * PTOD * 2.0
      CBORDY=MIN(ABS(SOURCE_Y-HEAD_YSTART),ABS(SOURCE_Y-HEAD_YEND))
     &                                   * PTOD * 2.0

*  Rectangle
      IF (SRT_SHAPE(1) .EQ. 'R') THEN

	 CALL USI_DEF0R('SWIDTH', CBORDX, STATUS)
	 CALL USI_DEF0R('SHEIGHT', CBORDY, STATUS)
	 CALL USI_GET0R('SWIDTH', SWIDTH, STATUS)
	 CALL USI_GET0R('SHEIGHT', SHEIGHT, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Convert to nearest whole pixels
         X_HWIDTH = NPIX(SWIDTH)
	 Y_HWIDTH = NPIX(SHEIGHT)

*   and back to world coords.
         XWIDW=2.0*X_HWIDTH*PTOD
         YWIDW=2.0*Y_HWIDTH*PTOD
*
*   Set sort ranges
         SRT_PHI(1)=0.0
         SRT_ELAMIN(1)=0.0
         SRT_ELBMIN(1)=0.0
         SRT_ELAMAX(1)=X_HWIDTH
         SRT_ELBMAX(1)=Y_HWIDTH

*  store ARD description
         CALL ARX_BOX(SRT_ARDID(1),0,'ADD',.FALSE.,XW,YW,XWIDW,YWIDW,
     :                                                         STATUS)
*
*   Circular area
      ELSEIF (SRT_SHAPE(1) .EQ. 'C') THEN
*
	 CALL USI_DEF0R('RAD', MIN(CBORDX,CBORDY)/2.0, STATUS)
	 CALL USI_GET0R('RAD', SRAD2, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Make sure the user hasn't requested the whole universe
         SRAD2 = MIN(SRAD2,MIN(CBORDX,CBORDY)/2.0)
*
*     Convert this radius into raw pixels, in elliptical coordinates
*     and set orientation to zero
	 SRT_ELAMIN(1) = 0.0
	 SRT_ELAMAX(1) = REAL(NINT(SRAD2 / PTOD - 0.5))
         SRT_ELBMIN(1) = 0.0
         SRT_ELBMAX(1) = SRT_ELAMAX(1)
         SRT_PHI(1) = 0.0
*
	 X_HWIDTH = SRT_ELAMAX(1)
	 Y_HWIDTH = SRT_ELAMAX(1)
*
*   back to world coords
         RADW=SRT_ELAMAX(1)*PTOD

*   store ARD description
         CALL ARX_CIRCLE(SRT_ARDID(1),0,'ADD',.FALSE.,XW,YW,RADW,STATUS)

*   Get box inner and outer radii for an annular box
      ELSEIF (SRT_SHAPE(1) .EQ. 'A') THEN
*
	 CALL USI_GET0R('IRAD', SRAD1, STATUS)
	 CALL USI_DEF0R('ORAD', MIN(CBORDX,CBORDY)/2.0, STATUS)
	 CALL USI_GET0R('ORAD', SRAD2, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Make sure the user hasn't requested the whole universe
         SRAD2 = MIN(SRAD2,MIN(CBORDX,CBORDY)/2.0)
*
*     Convert radial inputs into whole raw pixels in elliptic coords
         SRT_ELAMIN(1) = REAL(NINT(SRAD1 / PTOD))
         SRT_ELAMAX(1) = REAL(NINT(SRAD2 / PTOD))
         SRT_ELBMIN(1) = SRT_ELAMIN(1)
         SRT_ELBMAX(1) = SRT_ELAMAX(1)
         SRT_PHI(1)=0.0
*
*   and back to world coords
         IRADW=SRT_ELAMIN(1)*PTOD
         ORADW=SRT_ELAMAX(1)*PTOD

*   store ARD description
         CALL ARX_ANNULUS(SRT_ARDID(1),0,'ADD',.FALSE.,XW,YW,
     :                                    IRADW,ORADW,STATUS)

	 X_HWIDTH = SRT_ELAMAX(1)
	 Y_HWIDTH = SRT_ELAMAX(1)
*
*   Get ellipse parameters for an elliptical box
      ELSEIF (SRT_SHAPE(1) .EQ. 'E') THEN
*
*      Get orientation. +ve is anticlockwise from east
	 CALL USI_GET0R('ANGLE', ANGLE, STATUS)
*
*      Convert orientation angle to radians
         SRT_PHI(1) = ANGLE * DTOR
*
*      Find cos and sine of orientation
         SRT_COSPHI(1) = COS(SRT_PHI(1))
         SRT_SINPHI(1) = SIN(SRT_PHI(1))
*
*      Get inner value for X axis (semi-major) of ellipse
	 CALL USI_GET0R('EXINN', ELAMIN, STATUS)
*
*      Get outer radii - jumpout when they are not too big
         JUMPOUT = .FALSE.
         DO WHILE (.NOT. JUMPOUT)
*
            CALL USI_GET0R('EXOUT', ELAMAX, STATUS)
*
            CALL USI_GET0R('EYOUT', ELBMAX, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Check user hasn't set the outer radii too large
*      Get outer value for X axis (semi-major) of ellipse
            EXMAX=SQRT((ELAMAX*SRT_COSPHI(1))**2 +
     :                            (ELBMAX*SRT_SINPHI(1))**2)
            EYMAX=SQRT((ELAMAX*SRT_SINPHI(1))**2 +
     :                            (ELBMAX*SRT_COSPHI(1))**2)
*
*     Check user hasn't set the outer radii too large
            IF (EXMAX .GT. CBORDX/2.0 .OR. EYMAX .GT. CBORDY/2.0) THEN
               CALL MSG_PRNT('Outer axes extend beyond the edge of '/
     &                    /'the field - please reduce them')
               CALL USI_CANCL('EXOUT', STATUS)
               CALL USI_CANCL('EYOUT', STATUS)
            ELSE
               JUMPOUT=.TRUE.
            ENDIF
         ENDDO
*
*      Convert to pixels
         SRT_ELAMIN(1) = REAL(NINT(ELAMIN / PTOD))
         SRT_ELAMAX(1) = REAL(NINT(ELAMAX / PTOD))
         SRT_ELBMAX(1) = REAL(NINT(ELBMAX / PTOD))
*
*      Calculate the inner Y axis
         SRT_ELBMIN(1) = SRT_ELAMIN(1) *
     :                 SRT_ELBMAX(1) / SRT_ELAMAX(1)

*   convert back to world coords and store ARD description
         XWIDW=SRT_ELAMAX(1)*PTOD
         YWIDW=SRT_ELBMAX(1)*PTOD
         CALL ARX_ELLIPSE(SRT_ARDID(1),0,'ADD',.FALSE.,XW,YW,
     :                             XWIDW,YWIDW,ANGLE,STATUS)
         IF ((SRT_ELAMIN(1)).GT.0.0.AND.(SRT_ELBMIN(1)).GT.0.0) THEN
           XWIDW=SRT_ELAMIN(1)*PTOD
           YWIDW=SRT_ELBMIN(1)*PTOD
           CALL ARX_ELLIPSE(SRT_ARDID(1),0,'AND',.TRUE.,XW,YW,
     :                               XWIDW,YWIDW,ANGLE,STATUS)
         ENDIF
*
*      Calculate maximum X and Y half widths for this ellipse
	 X_HWIDTH = SQRT( (SRT_ELAMAX(1) * SRT_COSPHI(1)) ** 2 +
     &                           (SRT_ELBMAX(1) * SRT_SINPHI(1)) ** 2 )
	 Y_HWIDTH = SQRT( (SRT_ELAMAX(1) * SRT_SINPHI(1)) ** 2 +
     &                           (SRT_ELBMAX(1) * SRT_COSPHI(1)) ** 2 )
*
      ENDIF
*
*   Put in a modification which restricts the PH range of the corrected
*   amplitude data to be >=1 and <=256, if a spectrum is being produced.
      IF ( INDEX(BIN_AXES,'7') .NE. 0) THEN

        IF ( HEAD_CEND .GT. 256 ) THEN
          CALL MSG_PRNT(' ')
          CALL MSG_SETI('CMAX',HEAD_CEND)
          CALL MSG_PRNT('** WARNING: This observation contains data '/
     &      /'up to PHA=^CMAX. ONLY the first 256 '/
     &      /'channels can be analysed in a spectrum **')
          CALL MSG_PRNT(' ')
          HEAD_CEND = MIN(HEAD_CEND, 256)
        ENDIF
        IF (HEAD_CSTART .LE. 0) THEN
          CALL MSG_PRNT(' ')
          CALL MSG_SETI('CMIN',HEAD_CSTART)
          CALL MSG_PRNT('** WARNING: Illegal lower channel number - '/
     &       /'^CMIN - substituting 1')
        ENDIF
      ENDIF
*
* Display data ranges
      WRITE (*,*) '   Data array axes'
      WRITE (*,*) ' *******************'
      WRITE (*,1010)3,' XDET:',HEAD_XDSTART,HEAD_XDEND,' (pixels)'
      WRITE (*,1010)4,' YDET:',HEAD_YDSTART,HEAD_YDEND,' (pixels)'
      WRITE (*,1000)5,' Time:',HEAD_TSTART(1),HEAD_TEND(HEAD_NTRANGE),
     &                                           ' (seconds)'
      WRITE (*,1010)6,' PHA channel:',HEAD_ASTART,HEAD_AEND,' (chn.)'
      WRITE (*,1010)7,' Corr. PHA chan.',HEAD_CSTART,HEAD_CEND,' (chn.)'
*
1000  FORMAT(5X,I1,')',A,F12.3,' to ',F12.3,A)
1010  FORMAT(5X,I1,')',A,I7,' to ',I7,A)
*
      CALL USI_DEF0I('XDSTART', HEAD_XDSTART, STATUS)
      CALL USI_DEF0I('XDEND', HEAD_XDEND, STATUS)
      CALL USI_DEF0I('YDSTART', HEAD_YDSTART, STATUS)
      CALL USI_DEF0I('YDEND', HEAD_YDEND, STATUS)
*
      CALL CHR_DTOC(HEAD_TSTART(1), C1, K1)
      CALL CHR_DTOC(HEAD_TEND(HEAD_NTRANGE), C2, K2)
      TIMSTRING = C1(1:K1) // ':' // C2(1:K2)
*
      CALL USI_DEF0C('TIMRANGE', TIMSTRING, STATUS)
      CALL USI_DEF0I('MINPH', HEAD_ASTART, STATUS)
      CALL USI_DEF0I('MAXPH', HEAD_AEND, STATUS)
      CALL USI_DEF0I('MINEN', HEAD_CSTART, STATUS)
      CALL USI_DEF0I('MAXEN', HEAD_CEND, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Select ranges to change (3-7)
      CALL USI_GET0C('RANGES',RANGES,STATUS)
*
*   Use default ranges if zero or a null (!) has been entered
      IF (RANGES.EQ.'0'.OR.STATUS.EQ.PAR__NULL) CALL ERR_ANNUL(STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Detector X
      IF (INDEX(RANGES,'3') .NE. 0) THEN
         CALL USI_GET0I('XDSTART',SRT_MIN_XD(1),STATUS)
         CALL USI_GET0I('XDEND', SRT_MAX_XD(1), STATUS)
      ELSE
         SRT_MIN_XD(1) = HEAD_XDSTART
         SRT_MAX_XD(1) = HEAD_XDEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Detector Y
      IF (INDEX(RANGES,'4') .NE. 0) THEN
         CALL USI_GET0I('YDSTART',SRT_MIN_YD(1),STATUS)
         CALL USI_GET0I('YDEND', SRT_MAX_YD(1), STATUS)
      ELSE
         SRT_MIN_YD(1) = HEAD_YDSTART
         SRT_MAX_YD(1) = HEAD_YDEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Time:
      IF (INDEX(RANGES,'5') .NE. 0) THEN
*
*      Time range may be input as either a string of start and stop
*      times or a text file of times. The times may be expressed as offsets
*      from time zero or as MJDs in which case they are prefixed wih an 'M'.
         CALL USI_GET0C('TIMRANGE',TIMSTRING,STATUS)
*
*      Decode the timestring into a sequence of start and stop times
         CALL UTIL_TDECODE(TIMSTRING, HEAD_BASE_MJD, MXTIME,
     &             SRT_NTIME(1), SRT_MIN_T(1,1), SRT_MAX_T(1,1),
     :                                                   STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*
      ELSE
         SRT_NTIME(1)=1
         SRT_MIN_T(1,1) = HEAD_TSTART(1)
         SRT_MAX_T(1,1) = HEAD_TEND(HEAD_NTRANGE)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   PH:
      IF (INDEX(RANGES,'6') .NE. 0) THEN
         CALL USI_GET0I('MINPH', SRT_MIN_PH(1), STATUS)
	 CALL USI_GET0I('MAXPH', SRT_MAX_PH(1), STATUS)
      ELSE
         SRT_MIN_PH(1)=HEAD_ASTART
         SRT_MAX_PH(1)=HEAD_AEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*
*  Energy:
      IF (INDEX(RANGES,'7') .NE. 0) THEN
         CALL USI_GET0I('MINEN', SRT_MIN_EN(1), STATUS)
         CALL USI_GET0I('MAXEN', SRT_MAX_EN(1), STATUS)
      ELSE
         SRT_MIN_EN(1)=HEAD_CSTART
         SRT_MAX_EN(1)=HEAD_CEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Select bin widths for axes properties
* Calculate X and Y extremes
      IF ( INDEX(BIN_AXES,'1') .NE. 0) THEN
*
* Tell user how many raw pixels in axis
         CALL MSG_SETI('XPIX',2*X_HWIDTH)
         CALL MSG_OUT(' ',' There are ^XPIX raw sky pixels within'//
     &                                 ' the X range selected',STATUS)
*
         CALL USI_GET0I('NXBIN', NXBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBINX=NINT(2.0*REAL(X_HWIDTH)/REAL(NXBIN))
*
         SRT_MIN_X(1) = SOURCE_X-INT(REAL(NREBINX)*REAL(NXBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT_MAX_X(1) =SOURCE_X+NINT(REAL(NREBINX)*REAL(NXBIN)/2.0 +
     &                                0.4) - 1
*
* Make sure you haven't gone over the edge
         SRT_MIN_X(1) = MAX(SRT_MIN_X(1), HEAD_XSTART)
         SRT_MAX_X(1) = MIN(SRT_MAX_X(1), HEAD_XEND)
*
         CALL MSG_SETI('NXBIN',NXBIN)
         CALL MSG_SETI('NREB',NREBINX)
         CALL MSG_SETI('SPIX',SRT_MIN_X(1))
         CALL MSG_PRNT('There will be ^NXBIN X bins, each of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(1)=NXBIN
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=1
*
      ELSE
*
         SDIM(1)=1
         NREBINX=1
         SRT_MIN_X(1) = MAX(SOURCE_X-X_HWIDTH, HEAD_XSTART)
         SRT_MAX_X(1) = MIN(SOURCE_X+X_HWIDTH, HEAD_XEND)
*
      ENDIF
*
      IF ( INDEX(BIN_AXES,'2') .NE. 0) THEN
*
         CALL MSG_SETI('YPIX',2*Y_HWIDTH)
         CALL MSG_OUT(' ',' There are ^YPIX raw sky pixels within'//
     &                                 ' the Y range selected',STATUS)
*
         CALL USI_GET0I('NYBIN', NYBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBINY=NINT(2.0*REAL(Y_HWIDTH)/REAL(NYBIN))
*
         SRT_MIN_Y(1)=SOURCE_Y-INT(REAL(NREBINY)*REAL(NYBIN)/2.0)
         SRT_MAX_Y(1)=SOURCE_Y+NINT(REAL(NREBINY)*REAL(NYBIN)/2.0 +
     &                                        0.4) - 1
*
* Make sure you haven't gone over the edge
         SRT_MIN_Y(1) = MAX(SRT_MIN_Y(1), HEAD_YSTART)
         SRT_MAX_Y(1) = MIN(SRT_MAX_Y(1), HEAD_YEND)

         CALL MSG_SETI('NYBIN',NYBIN)
         CALL MSG_SETI('NREB',NREBINY)
         CALL MSG_SETI('SPIX',SRT_MIN_Y(1))
         CALL MSG_PRNT('There will be ^NYBIN Y bins, each of '//
     &                         '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(2)=NYBIN
*
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=2
*
      ELSE
*
         SDIM(2)=1
         NREBINY=1
         SRT_MIN_Y(1) = MAX(SOURCE_Y-Y_HWIDTH, HEAD_YSTART)
         SRT_MAX_Y(1) = MIN(SOURCE_Y+Y_HWIDTH, HEAD_YEND)
*
      ENDIF
*
      IF ( INDEX(BIN_AXES,'8') .NE. 0) THEN
*
*     Get number of radial bins
         CALL USI_GET0I('NRBIN', NRBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
C         RBIN = (SRT_RADIN(1) - SRT_RADOUT(1)) / REAL(NRBIN)
*
C         CALL MSG_SETI('NRBIN',NRBIN)
C         CALL MSG_SETI('RBIN',RBIN)
C         CALL MSG_SETI('SPIX',SRT_RADIN(1))
C         CALL MSG_PRNT('There will be ^NRBIN radial bins, each of '//
C     &                         '^RBIN degrees starting from ^SPIX')
*
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=8
*
*   Set the first dimension to be the number of radial bins.
         SDIM(1)=NRBIN
*
      ELSE
         NRBIN=1
      ENDIF
*
* Detector pixels
      IF ( INDEX(BIN_AXES,'3') .NE. 0) THEN
*
* Tell user how many raw pixels in axis
         CALL MSG_SETI('XDET',SRT_MAX_XD(1)-SRT_MIN_XD(1)+1)
         CALL MSG_OUT(' ',' There are ^XDET raw pixels within'//
     &                      ' the X detector range selected',STATUS)
*
         CALL USI_GET0I('NXDBIN', NXDBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBXD=NINT((SRT_MAX_XD(1) - SRT_MIN_XD(1) + 1) / REAL(NXDBIN))
*
         SRT_MIN_XD(1) = (SRT_MAX_XD(1) + SRT_MIN_XD(1))/2.0 -
     &                    INT(REAL(NREBXD)*REAL(NXDBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT_MAX_XD(1) = (SRT_MAX_XD(1) + SRT_MIN_XD(1))/2.0 +
     &                  NINT(REAL(NREBXD)*REAL(NXDBIN)/2.0 + 0.4) - 1
         CALL MSG_SETI('NXDBIN',NXDBIN)
         CALL MSG_SETI('NREB',NREBXD)
         CALL MSG_SETI('SPIX',SRT_MIN_XD(1))
         CALL MSG_PRNT('There will be ^NXDBIN X detector bins, of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(3)=NXDBIN
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=3
*
      ELSE
*
         SDIM(3)=1
         NREBXD=1
*
      ENDIF
*
* Y detector:
      IF ( INDEX(BIN_AXES,'4') .NE. 0) THEN
*
* Tell user how many raw pixels in axis
         CALL MSG_SETI('YDET',SRT_MAX_YD(1)-SRT_MIN_YD(1)+1)
         CALL MSG_OUT(' ',' There are ^YDET raw pixels within'//
     &                      ' the Y detector range selected',STATUS)
*
         CALL USI_GET0I('NYDBIN', NYDBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBYD=NINT((SRT_MAX_YD(1) - SRT_MIN_YD(1) + 1) / REAL(NYDBIN))
*
         SRT_MIN_YD(1) = (SRT_MAX_YD(1) + SRT_MIN_YD(1))/2.0 -
     &                    INT(REAL(NREBYD)*REAL(NYDBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT_MAX_YD(1) = (SRT_MAX_YD(1) + SRT_MIN_YD(1))/2.0 +
     &                  NINT(REAL(NREBYD)*REAL(NYDBIN)/2.0 + 0.4) - 1
         CALL MSG_SETI('NYDBIN',NYDBIN)
         CALL MSG_SETI('NREB',NREBYD)
         CALL MSG_SETI('SPIX',SRT_MIN_YD(1))
         CALL MSG_PRNT('There will be ^NYDBIN Y detector bins, of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(4)=NYDBIN
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=4
*
      ELSE
*
         SDIM(4)=1
         NREBYD=1
*
      ENDIF
*
      IF ( INDEX(BIN_AXES,'5') .NE. 0) THEN
*
         CALL USI_GET0R('TIMBIN', TIMBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
         SDIM(5)=INT((SRT_MAX_T(SRT_NTIME(1),1) -
     :                          SRT_MIN_T(1,1))/TIMBIN)
*
* Need to restrict the allowed time ranges to the new max. value.
         SMAXT=SRT_MIN_T(1,1) + INT(REAL(SDIM(5))*TIMBIN)
*
*   Check which srt range is greater than this
         DO LP=1,SRT_NTIME(1)
*
            IF (SRT_MAX_T(LP,1) .GT. SMAXT) THEN
               IF (SRT_MIN_T(LP,1) .LE. SMAXT) THEN
                  SRT_MAX_T(LP,1) = SMAXT
                  SRT_NTIME(1) = LP
                  GOTO 100
               ELSE
                  IF (LP.GT.1) SRT_NTIME(1) = LP-1
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO
*
100      CONTINUE
*
         CALL MSG_SETI('NTBIN',SDIM(5))
         CALL MSG_SETR('TIMBIN',TIMBIN)
         CALL MSG_SETR('STIM',REAL(SRT_MIN_T(1,1)))
         CALL MSG_PRNT('There will be ^NTBIN Time bins, each '//
     &                       'of ^TIMBIN seconds, starting from ^STIM')
*
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=5
*
      ELSE
         SDIM(5)=1
      ENDIF
*
      IF ( INDEX(BIN_AXES,'6') .NE. 0) THEN
*
         CALL USI_GET0I('PHBIN', PHBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Make sure PH binsize makes sense
         PHBIN = MAX(1,PHBIN)
         PHBIN = MIN( (SRT_MAX_PH(1)-SRT_MIN_PH(1)+1), PHBIN )
*
*   Calculate number of PH bins in output file and recalculate maximum
         SDIM(6)=INT((SRT_MAX_PH(1)-SRT_MIN_PH(1)+1)/PHBIN)
         SRT_MAX_PH(1)=PHBIN*SDIM(6)+SRT_MIN_PH(1)-1
*
         CALL MSG_SETI('NPBIN',SDIM(6))
         CALL MSG_SETI('PHBIN',PHBIN)
         CALL MSG_SETI('SPH',SRT_MIN_PH(1))
         CALL MSG_PRNT('There will be ^NPBIN PH bins, each '//
     &          'of ^PHBIN raw channels, starting from ^SPH')
*
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=6
*
      ELSE
         SDIM(6)=1
      ENDIF
*
      IF ( INDEX(BIN_AXES,'7') .NE. 0) THEN
*
         CALL USI_GET0I('ENBIN', ENBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Make sure energy binsize makes sense
         ENBIN = MAX(1,ENBIN)
         ENBIN = MIN( (SRT_MAX_EN(1)-SRT_MIN_EN(1)+1), ENBIN )
*
*   Calculate number of PH bins in output file and recalculate maximum
         SDIM(7)=INT((SRT_MAX_EN(1)-SRT_MIN_EN(1)+1)/ENBIN)
         SRT_MAX_EN(1)=ENBIN*SDIM(7)+SRT_MIN_EN(1)-1
*
         CALL MSG_SETI('NEBIN',SDIM(7))
         CALL MSG_SETI('ENBIN',ENBIN)
         CALL MSG_SETI('SEN',SRT_MIN_EN(1))
         CALL MSG_PRNT('There will be ^NEBIN energy bins, each '//
     &          'of ^ENBIN raw channels, starting from ^SEN')
*
         SRT_NAXES(1)=SRT_NAXES(1)+1
         SRT_BINAXIS(SRT_NAXES(1),1)=7
*
      ELSE
         SDIM(7)=1
      ENDIF
*
* Set number of azimuthal bins to 1 for now
      NAZBIN=1
*
* Calculate the centre X and Y value in degrees
      SRT_XCENT(1) =
     :   - ((SRT_MIN_X(1) + SRT_MAX_X(1)) / 2.0 - HEAD_SKYCX) * PTOD
      SRT_YCENT(1) = MFACT*((SRT_MIN_Y(1)+SRT_MAX_Y(1))/2.0-HEAD_SKYCY)
     &                                      * PTOD
*
* The outer X,Y values may have changed due to rebinning. If so
* recalculate them.
      IF (SDIM(1) .GT. 1 .OR. SDIM(2) .GT. 1 ) THEN
         IF (SRT_SHAPE(1) .NE. 'E') THEN
            SRT_ELAMAX(1) =
     :           ABS(SRT_MAX_X(1)-(SRT_MIN_X(1) + SRT_MAX_X(1))/2.0)
            SRT_ELBMAX(1) = ABS(SRT_MAX_Y(1) -
     :             (SRT_MIN_Y(1) + SRT_MAX_Y(1))/2.0)
         ELSE
C????            SRT.ELAMAX = SRT.ELAMAX * SRT.MAX_X / X_HWIDTH
C????            SRT.ELBMAX = SRT.ELBMAX * SRT.MAX_X / X_HWIDTH
         ENDIF
      ENDIF
*
* Calculate cos and sine of the orientation
      SRT_COSPHI(1) = COS(SRT_PHI(1))
      SRT_SINPHI(1) = SIN(SRT_PHI(1))

* Calculate tot. no of elements
      SRT_NDATA(1)=SDIM(1)*SDIM(2)*SDIM(3)*SDIM(4)*SDIM(5)*
     :        SDIM(6)*SDIM(7)*NRBIN*NAZBIN
*
      SRT_IMAGE(1)=(SRT_DTYPE.EQ.'BinDS'.AND.
     &              SDIM(1).GT.1.AND.SDIM(2).GT.1.AND.NRBIN.EQ.1)

* Is a background file wanted ? Not possible if source file contains
* radial bins.
      IF ( INDEX(BIN_AXES,'8') .EQ. 0) THEN
         CALL USI_GET0L('BACK', SRT_BCKGND, STATUS)
      ELSE
         SRT_BCKGND = .FALSE.
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999

* Get RA and DEC of background box in degrees.
      IF (SRT_BCKGND) THEN


*  ARD description
         IF (SRT_SHAPE(2).EQ.'I') THEN

*  get ARD file
            CALL ARX_READ('BARD',SRT_ARDID(2),STATUS)


*  Create spatial mask
            CALL MSG_PRNT('Creating background spatial mask...')
            CALL XRTSORT_CRE_MASK(SRT_ARDID(2),RES,MDIM,MRES,BMPTR,
     :                                                      STATUS)
            CALL MSG_PRNT('Done!')

            CALL XRTSORT_SCAN_MASK(MDIM(1),MDIM(2),%val(BMPTR),
     :                           MRES,BXW,BYW,XW1,XW2,YW1,YW2,STATUS)

            IF (CENTRE.EQ.'MEAN') THEN
               CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             SRT_FIELD_RA(2),SRT_FIELD_DEC(2),STATUS)
            ELSEIF (CENTRE.EQ.'MEDIAN') THEN
               BXW=(XW1+XW2)/2.0
               BYW=(YW1+YW2)/2.0
               CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             SRT_FIELD_RA(2),SRT_FIELD_DEC(2),STATUS)
            ELSEIF (CENTRE.EQ.'AXIS') THEN
                SRT_FIELD_RA(2)=HEAD_AXIS_RA
                SRT_FIELD_DEC(2)=HEAD_AXIS_DEC
                CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(2),
     :                    SRT_FIELD_DEC(2),TMAT,XW,YW,STATUS)
            ELSEIF (CENTRE.EQ.'USER') THEN
                CALL USI_GET0C('RAB', RAS, STATUS)
                CALL USI_GET0C('DECB', DECS, STATUS)
                CALL CONV_RADEC(RAS,DECS,SRT_FIELD_RA(2),
     :                                SRT_FIELD_DEC(2),STATUS)
                CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(2),
     :                 SRT_FIELD_DEC(2),TMAT,XW,YW,STATUS)
            ENDIF

            BX_HWIDTH=NPIX(ABS(XW2-XW1))
            BY_HWIDTH=NPIX(ABS(YW2-YW1))


        ELSE

*   Set centre of the field as default
           WRITE(RAS,*) HEAD_AXIS_RA
           WRITE(DECS,*) HEAD_AXIS_DEC
           CALL USI_DEF0C('RAB', RAS, STATUS)
           CALL USI_DEF0C('DECB', DECS, STATUS)

           CALL USI_GET0C('RAB', RAS, STATUS)
           CALL USI_GET0C('DECB',DECS, STATUS)
           CALL CONV_RADEC(RAS,DECS,SRT_FIELD_RA(2),SRT_FIELD_DEC(2),
     :                                                     STATUS)
           CALL XRTSORT_RADEC2AXIS(SRT_FIELD_RA(2),SRT_FIELD_DEC(2),
     :                                             TMAT,XW,YW,STATUS)

        ENDIF

        IF (STATUS .NE. SAI__OK) GOTO 999

*  Calculate the source position in raw pixels

        BCKGND_X = NINT( -XW / PTOD)
        BCKGND_Y = NINT( YW / PTOD) * MFACT

*  Add the sky pixel centre to the source centre
        BCKGND_X = BCKGND_X + HEAD_SKYCX
        BCKGND_Y = BCKGND_Y + HEAD_SKYCY

*  Find the closest distance to each border
        BBORDX=MIN(ABS(BCKGND_X-HEAD_XSTART),ABS(BCKGND_X-HEAD_XEND))
     &                                                   * PTOD * 2.0
        BBORDY=MIN(ABS(BCKGND_Y-HEAD_YSTART),ABS(BCKGND_Y-HEAD_YEND))
     &                                                   * PTOD * 2.0





	 IF (SRT_SHAPE(1) .EQ. 'R') THEN
	    CALL USI_GET0R('BWIDTH', BWIDTH, STATUS)
	    CALL USI_GET0R('BHEIGHT', BHEIGHT, STATUS)
*
*  Convert to nearest whole pixels
	    BX_HWIDTH = NPIX(BWIDTH)
	    BY_HWIDTH = NPIX(BHEIGHT)

*  And back to world coords.
            BXWIDW=2.0*BX_HWIDTH*PTOD
            BYWIDW=2.0*BY_HWIDTH*PTOD
*
*  Check if this is a rectangular annulus
            IF (ABS(SRT_FIELD_RA(2)-SRT_FIELD_RA(1)) .LT. 0.001 .AND.
     &          ABS(SRT_FIELD_DEC(2)-SRT_FIELD_DEC(1)) .LT. 0.001) THEN
*
               CALL ARX_BOX(SRT_ARDID(2),0,'ADD',.FALSE.,BXW,BYW,
     :                                     BXWIDW,BYWIDW,STATUS)
               CALL ARX_BOX(SRT_ARDID(2),0,'AND',.TRUE.,XW,YW,
     :                                     XWIDW,YWIDW,STATUS)

*     Set sort parameters
               SRT_ELAMIN(2) = X_HWIDTH
               SRT_ELBMIN(2) = Y_HWIDTH
               SRT_ELAMAX(2) = BX_HWIDTH
               SRT_ELBMAX(2) = BY_HWIDTH
*
            ELSE
*
               CALL ARX_BOX(SRT_ARDID(2),0,'ADD',.FALSE.,BXW,BYW,
     :                                     BXWIDW,BYWIDW,STATUS)

               SRT_ELAMIN(2) = 0.0
               SRT_ELBMIN(2) = 0.0
               SRT_ELAMAX(2) = BX_HWIDTH
               SRT_ELBMAX(2) = BY_HWIDTH
*
            ENDIF
*
	 ELSEIF (SRT_SHAPE(1) .EQ. 'C' .OR. SRT_SHAPE(1) .EQ. 'A') THEN
*
*     If an annulus is wanted (i.e. the centre of the background box is
*     the same as that of the source box) then set the default for the
*     inner radius of the background annulus to the outer radius of the
*     source box.
            IF (ABS(SRT_FIELD_RA(2)-SRT_FIELD_RA(1)) .LT. 0.001 .AND.
     &          ABS(SRT_FIELD_DEC(2)-SRT_FIELD_DEC(1)) .LT. 0.001) THEN

	       CALL USI_DEF0R('BIRAD', SRAD2, STATUS)
               SRT_SHAPE(2) = 'A'
            ELSE
	       CALL  USI_DEF0R('BIRAD', 0.0, STATUS)
*
*  Calculate distance of background box centre from the edge
            ENDIF
*
            CALL USI_DEF0R('BIRAD', MIN(BBORDX,BBORDY)/2.0, STATUS)
*
	    CALL USI_GET0R('BIRAD', BRADIN, STATUS)
	    CALL USI_GET0R('BORAD', BRADOUT, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Check user hasn't set the outer radius too large
            IF (BRADOUT .GT. MIN(BBORDX,BBORDY)/2.0) THEN
               BRADOUT = MIN(BBORDX,BBORDY)/2.0
*
               CALL MSG_SETR('OUT',BRADOUT)
               CALL MSG_PRNT('Reset outer background radius to ^OUT')
            ENDIF
*

	    SRT_ELAMAX(2) = REAL(NINT(BRADOUT / PTOD))
	    SRT_ELAMIN(2) = REAL(NINT(BRADIN / PTOD))
            SRT_ELBMIN(2) = SRT_ELAMIN(2)
            SRT_ELBMAX(2) = SRT_ELAMAX(2)
	    BX_HWIDTH = SRT_ELAMAX(2)
	    BY_HWIDTH = SRT_ELAMAX(2)
*
            BIRADW=SRT_ELAMIN(2)*PTOD
            BORADW=SRT_ELAMAX(2)*PTOD

            CALL ARX_ANNULUS(SRT_ARDID(2),0,'ADD',.FALSE.,BXW,BYW,
     :                                      BIRADW,BORADW,STATUS)

         ELSEIF (SRT_SHAPE(1) .EQ. 'E') THEN
*
            ANNULAR=(ABS(SRT_FIELD_RA(2)-SRT_FIELD_RA(1)) .LT. 0.001
     :         .AND.
     :          ABS(SRT_FIELD_DEC(2)-SRT_FIELD_DEC(1)) .LT. 0.001)

            IF (ANNULAR) THEN

*     Set some parameters if background box is an annular ellipse
*
	       CALL USI_DEF0R('BEXINN', ELAMAX, STATUS)
*
*        Background orientation equals source orientation
               SRT_PHI(2) = SRT_PHI(1)
               BANGLE=ANGLE

            ELSE
*
*       Get orientation. +ve is anticlockwise from east
	       CALL USI_GET0R('BANGLE', SRT_PHI(2), STATUS)
*
	       CALL USI_DEF0R('BEXINN', 0.0, STATUS)
*
*       Convert the angle to radians
               SRT_PHI(2) = SRT_PHI(2) * DTOR
*
            ENDIF
*
*      Find cos and sine of orientation
            SRT_COSPHI(2) = COS(SRT_PHI(2))
            SRT_SINPHI(2) = SIN(SRT_PHI(2))
*
*      Get inner value for X axis (semi-major) of ellipse
	    CALL USI_GET0R('BEXINN', ELAMIN, STATUS)
*
*      Get outer radii - jumpout when they are not too big
            JUMPOUT = .FALSE.
            DO WHILE (.NOT. JUMPOUT)
*
               CALL USI_GET0R('BEXOUT', ELAMAX, STATUS)
*
*      Set default if poss.
               IF (SRT_ELAMAX(1) .GT. 0) THEN
                  ELBMAX = SRT_ELBMAX(1) * ELAMAX / SRT_ELAMAX(1)
	          CALL USI_DEF0R('BEYOUT', ELBMAX, STATUS)
               ENDIF
*
               CALL USI_GET0R('BEYOUT', ELBMAX, STATUS)
*
               IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Check user hasn't set the outer radii too large
*      Get outer value for X axis (semi-major) of ellipse
               EXMAX=SQRT((ELAMAX*SRT_COSPHI(2))**2 +
     &                              (ELBMAX*SRT_SINPHI(2))**2)
               EYMAX=SQRT((ELAMAX*SRT_SINPHI(2))**2 +
     &                                  (ELBMAX*SRT_COSPHI(2))**2)
*
*     Check user hasn't set the outer radii too large
               IF (EXMAX .GT. BBORDX/2.0 .OR. EYMAX .GT. BBORDY/2.0)THEN
                  CALL MSG_PRNT('Outer axes extend beyond the edge of '/
     &                    /'the field - please reduce them')
                  CALL USI_CANCL('BEXOUT', STATUS)
                  CALL USI_CANCL('BEYOUT', STATUS)
               ELSE
                  JUMPOUT=.TRUE.
               ENDIF
            ENDDO
*
*      Convert to pixels
            SRT_ELAMIN(2) = REAL(NINT(ELAMIN / PTOD))
            SRT_ELAMAX(2) = REAL(NINT(ELAMAX / PTOD))
            SRT_ELBMAX(2) = REAL(NINT(ELBMAX / PTOD))
*
*      Calculate the inner Y axis
            SRT_ELBMIN(2) = SRT_ELAMIN(2) * SRT_ELBMAX(2)/SRT_ELAMAX(2)
*
*      Calculate maximum X and Y half widths for this ellipse
	    BX_HWIDTH = SQRT( (SRT_ELAMAX(2)*SRT_COSPHI(2)) ** 2 +
     &                             (SRT_ELBMAX(2)*SRT_SINPHI(2)) ** 2 )
	    BY_HWIDTH = SQRT( (SRT_ELAMAX(2)*SRT_SINPHI(2)) ** 2 +
     &                             (SRT_ELBMAX(2)*SRT_COSPHI(2)) ** 2 )
*
            BXWIDW=SRT_ELAMAX(2)*PTOD
            BYWIDW=SRT_ELBMAX(2)*PTOD

            CALL ARX_ELLIPSE(SRT_ARDID(2),0,'ADD',.FALSE.,BXW,BYW,
     :                               BXWIDW,BYWIDW,BANGLE,STATUS)
            IF (ANNULAR) THEN
              CALL ARX_ELLIPSE(SRT_ARDID(2),0,'AND',.TRUE.,XW,YW,
     :                                 XWIDW,YWIDW,ANGLE,STATUS)
            ENDIF
	 END IF
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* If image axes have been binned calculate background box. Binning factor is
* the same as for the source box
*
         IF (INDEX(BIN_AXES,'1') .NE. 0) THEN
*
             BDIM(1)=INT(BX_HWIDTH*2/NREBINX)
             SRT_MIN_X(2)=BCKGND_X-(NREBINX*BDIM(1))/2
             SRT_MAX_X(2)=BCKGND_X+NINT(REAL(NREBINX)*REAL(BDIM(1))/2.0
     &                                 + 0.4)
         ELSE
             BDIM(1)=1
             SRT_MIN_X(2)=BCKGND_X-BX_HWIDTH
             SRT_MAX_X(2)=BCKGND_X+BX_HWIDTH
*
         ENDIF
*
* Make sure you haven't gone over the edge
         SRT_MIN_X(2) = MAX(SRT_MIN_X(2), HEAD_XSTART)
         SRT_MAX_X(2) = MIN(SRT_MAX_X(2), HEAD_XEND)

         IF (INDEX(BIN_AXES,'2') .NE. 0) THEN
*
             BDIM(2)=INT(BY_HWIDTH*2/NREBINY)
             SRT_MIN_Y(2)=BCKGND_Y-INT(REAL(NREBINY)*REAL(BDIM(2))/2.0)
             SRT_MAX_Y(2)=BCKGND_Y+NINT(REAL(NREBINY)*REAL(BDIM(2))/2.0
     &                                    + 0.4)
*
         ELSE
             BDIM(2)=1
             SRT_MIN_Y(2)=BCKGND_Y-BY_HWIDTH
             SRT_MAX_Y(2)=BCKGND_Y+BY_HWIDTH

         ENDIF
*
* Make sure you haven't gone over the edge
         SRT_MIN_Y(2) = MAX(SRT_MIN_Y(2), HEAD_YSTART)
         SRT_MAX_Y(1) = MIN(SRT_MAX_Y(1), HEAD_YEND)

* Set time and sumsig for background same as source.
         SRT_MIN_XD(2)=SRT_MIN_XD(1)
         SRT_MIN_YD(2)=SRT_MIN_YD(1)
         SRT_MIN_PH(2)=SRT_MIN_PH(1)
         SRT_MIN_EN(2)=SRT_MIN_EN(1)
*
         SRT_MAX_XD(2)=SRT_MAX_XD(1)
         SRT_MAX_YD(2)=SRT_MAX_YD(1)
         SRT_MAX_PH(2)=SRT_MAX_PH(1)
         SRT_MAX_EN(2)=SRT_MAX_EN(1)
*
         SRT_NTIME(2) = SRT_NTIME(1)
*
         DO LP=1,SRT_NTIME(2)
            SRT_MIN_T(LP,2)=SRT_MIN_T(LP,1)
            SRT_MAX_T(LP,2)=SRT_MAX_T(LP,1)
         ENDDO
*
         BDIM(3)=SDIM(3)
         BDIM(4)=SDIM(4)
         BDIM(5)=SDIM(5)
         BDIM(6)=SDIM(6)
         BDIM(7)=SDIM(7)
*
*  Set axes components the same as for source
         SRT_NAXES(2)=SRT_NAXES(1)
*
         DO LP=1,8
            SRT_BINAXIS(LP,2)=SRT_BINAXIS(LP,1)
         ENDDO
*
* Calculate the centre X and Y value in degrees
         SRT_XCENT(2) = - ((SRT_MIN_X(2)+SRT_MAX_X(2)) /
     :                  2.0 - HEAD_SKYCX)* PTOD
         SRT_YCENT(2) = ((SRT_MIN_Y(2)+SRT_MAX_Y(2)) /
     :                  2.0 - HEAD_SKYCY)* PTOD * MFACT
*
* The outer X,Y values may have changed due to rebinning. If so
* recalculate them
         IF (SDIM(1) .GT. 1 .OR. SDIM(2) .GT. 1) THEN
            IF (SRT_SHAPE(2) .NE. 'E') THEN
               SRT_ELAMAX(2) = ABS(SRT_MAX_X(2) - (SRT_MIN_X(2) +
     &                                          SRT_MAX_X(2))/2.0)
               SRT_ELBMAX(2) = ABS(SRT_MAX_Y(2) - (SRT_MIN_Y(2) +
     &                                          SRT_MAX_Y(2))/2.0)
            ELSE
               SRT_ELAMAX(2) = SRT_ELAMAX(2) * ABS(SRT_MAX_X(2) -
     &             (SRT_MIN_X(2) + SRT_MAX_X(2)) / 2.0 ) / BX_HWIDTH
               SRT_ELBMAX(2) = SRT_ELBMAX(2) * ABS(SRT_MAX_X(2) -
     &             (SRT_MIN_X(2) + SRT_MAX_X(2)) / 2.0 ) / BX_HWIDTH
            ENDIF
         ENDIF
*
* Calculate cos and sine of the orientation
         SRT_COSPHI(2) = COS(SRT_PHI(2))
         SRT_SINPHI(2) = SIN(SRT_PHI(2))
*
      ELSE
*
*  Set background dimensions to one
         DO LP=1,7
            BDIM(LP)=1
         ENDDO
      ENDIF

      SRT_IMAGE(2)=(SRT_DTYPE.EQ.'BinDS'.AND.
     &              BDIM(1).GT.1.AND.BDIM(2).GT.1)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_RANGESELECT',STATUS)
      ENDIF
*
      END



*+XRTSORT_SORT_BIN- Sorts XRT raw data into a binned data array
      SUBROUTINE XRTSORT_SORT_BIN(SDIM1, SDIM2,
     &           SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDIM1,
     &           BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &           NRBIN, NAZBIN, MDIM1,MDIM2,MRES,SMASK, BMASK,
     &           S2MASK,B2MASK,ELIPA2,ELIPB2,SDATA,BDATA,SQUAL,BQUAL,
     &                                                         STATUS)
*    Description :
*        Sorts events from an XRT hds event datafile into a temporary
*       array of 7 dimensions.
*    History :
*     2-Nov-1988   original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Status :
      INTEGER STATUS
*    Structures :
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
*
      INTEGER SDIM1,SDIM2,SDIM3,SDIM4,SDIM5       ! Dimensions of source array
      INTEGER SDIM6,SDIM7                         ! Dimensions of source array
      INTEGER BDIM1,BDIM2,BDIM3,BDIM4,BDIM5       ! Dimensions of bckgnd array
      INTEGER BDIM6,BDIM7                         ! Dimensions of bckgnd array
      INTEGER NRBIN                               ! Number of radial bins
      INTEGER NAZBIN                              ! Number of azimuthal bins
      INTEGER MDIM1,MDIM2			  ! Dimensions of spatial mask
      REAL MRES					  ! Resolution of mask
      INTEGER SMASK(MDIM1,MDIM2)                  ! Source spatial mask
      INTEGER BMASK(MDIM1,MDIM2)                  ! Bckgnd spatial mask
      INTEGER S2MASK(SDIM1,SDIM2)
      INTEGER B2MASK(BDIM1,BDIM2)
      INTEGER IUNIT
*
*    Import-Export :
      REAL ELIPA2(NRBIN)                          ! Square of elliptical
                                                  ! X axes (for each rad. bin)
      REAL ELIPB2(NRBIN)                          ! Square of elliptical Y axes
      REAL SDATA(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)  ! S array
      REAL BDATA(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)  ! b array
      BYTE SQUAL(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)  ! S quality
      BYTE BQUAL(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)  ! B quality
*    Export :
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXBAD
         PARAMETER (MAXBAD=1000)                  ! Max no. of bad time periods
*    Local variables :
      REAL STBAD(MAXBAD), ENBAD(MAXBAD)           ! Bad time periods
      REAL XWIDTH,YWIDTH,EWIDTH,PWIDTH            ! Binwidth of each axis
      DOUBLE PRECISION TWIDTH
      REAL BXWIDTH,BYWIDTH                        ! Binwidth in backgnd box
      REAL XDWID,YDWID                            ! Width of detector axes
      REAL ELAWID,ELBWID                          ! Binwidth of elliptic axes
      DOUBLE PRECISION T1,T2                      ! Lower and upper limits of
*                                                 ! a time bin
      INTEGER NBAD                                ! Number of bad time ranges
      INTEGER TLP,LP1,LP2,LP3,LP4,LP6,LP7,INLP,LP
      INTEGER BADEV                               ! No of events in hotspots
      LOGICAL QVAL
      INTEGER PTRA(7)            ! Pointer to mapped arrays and item count
      INTEGER LOWER              ! number of indexes and ranges
*
      INTEGER ANYF               ! Notes undefined array elements
      INTEGER COL                                  ! Fits table, column no
      INTEGER FBEG                                 ! Fits table, start
      INTEGER FEOF                                 ! Flag EOF
      INTEGER HTYPE                                ! Fits header, style
      INTEGER MXCOL                                ! Max number of columns
        PARAMETER (MXCOL = 512)
      INTEGER NROWS                                ! Fits table, no of rows
      INTEGER NHDU                                 ! Fits header, unit
      INTEGER VARIDAT                              ! Fitsio variable
      INTEGER TFIELDS            ! Fits header, no fields per rows
      INTEGER BLOCK, N1
      INTEGER PHACOL

      CHARACTER*20  EXTNAME                         ! File extension name
      CHARACTER*100 INSTRUMENT,COMMENT              ! Instrument name
      CHARACTER*12  TTYPE(MXCOL)                    ! Fits header, col name
      CHARACTER*40  TFORM(MXCOL)                    ! Fits header, var type
      CHARACTER*40  TUNIT(MXCOL)  ! Fits header, unit of measurement

      LOGICAL       PI                              ! Flags the PI column
*-
      IF (STATUS.NE.SAI__OK) RETURN

* Set events in hotspots counter to zero.
      BADEV = 0
*
* Calculate bin widths for each axis. NB: SDIM1 can refer to the no. of
* radial bins.
      IF (NRBIN .EQ. 1) THEN
         XWIDTH = (SRT_MAX_X(1) - SRT_MIN_X(1) + 1) / REAL(SDIM1)
         YWIDTH = (SRT_MAX_Y(1) - SRT_MIN_Y(1) + 1) / REAL(SDIM2)
      ELSE
         XWIDTH = (SRT_MAX_X(1) - SRT_MIN_X(1) + 1) / 1.0
         YWIDTH = (SRT_MAX_Y(1) - SRT_MIN_Y(1) + 1) / 1.0
      ENDIF
*
      IF (SDIM1 .EQ. 1 .AND. SDIM2 .EQ. 1) THEN
         BXWIDTH = (SRT_MAX_X(2) - SRT_MIN_X(2) + 1) / REAL(BDIM1)
         BYWIDTH = (SRT_MAX_Y(2) - SRT_MIN_Y(2) + 1) / REAL(BDIM2)
      ELSE
         BXWIDTH=XWIDTH
         BYWIDTH=YWIDTH
      ENDIF
*
      XDWID = (SRT_MAX_XD(1) - SRT_MIN_XD(1) + 1) / REAL(SDIM3)
      YDWID = (SRT_MAX_YD(1) - SRT_MIN_YD(1) + 1) / REAL(SDIM4)
      TWIDTH = (SRT_MAX_T(SRT_NTIME(1),1) - SRT_MIN_T(1,1))
     :                                          / REAL(SDIM5)
      PWIDTH = (SRT_MAX_PH(1) - SRT_MIN_PH(1) + 1) / REAL(SDIM6)
      EWIDTH = (SRT_MAX_EN(1) - SRT_MIN_EN(1) + 1) / REAL(SDIM7)
*
* Generate the squares of the elliptical minor and major axes
* for each radial bin (Max values).
      IF (NRBIN .GT. 1) THEN
*   Calculate binwidths for elliptical minor and major axes
         ELAWID = (SRT_ELAMAX(1) - SRT_ELAMIN(1)) / REAL(NRBIN)
         ELBWID = (SRT_ELBMAX(1) - SRT_ELBMIN(1)) / REAL(NRBIN)
*
         DO LP=1,NRBIN
            ELIPA2(LP) = (SRT_ELAMIN(1) + (LP) * ELAWID) **2
            ELIPB2(LP) = (SRT_ELBMIN(1) + (LP) * ELBWID) **2
         ENDDO
      ENDIF
*
*  Open the FITS fIle
      CALL FIO_GUNIT(IUNIT,STATUS)
      CALL FTOPEN(IUNIT,SRT_ROOTNAME,0,BLOCK,STATUS)
      IF (STATUS.NE.0) THEN
	 CALL MSG_SETC('FNAM',SRT_ROOTNAME)
         CALL MSG_PRNT('XRTSORT : Error - opening file ^FNAM **')
         GOTO 999
      ENDIF
*
*  Locate event data
*  Move to FITS header.
      NHDU = 0
      CALL FTMAHD(IUNIT, 1, HTYPE, STATUS)
*     Locate STDEVT table in FITS file.
      DO WHILE (EXTNAME .NE. 'STDEVT')
*        Flag EOF
         FEOF = NHDU
*        Get the current hdu values
         CALL FTGHDN(IUNIT,NHDU)
*        Move to the next data unit.
         CALL FTMRHD(IUNIT,1,HTYPE,STATUS)
*        If type is binary table get table details
         IF (HTYPE .EQ. 2) THEN
            CALL FTGBNH(IUNIT, NROWS, TFIELDS, TTYPE, TFORM,
     :      TUNIT, EXTNAME, VARIDAT, STATUS)
         END IF
         IF ( NHDU .EQ. FEOF) THEN
            CALL MSG_PRNT('XRTSORT : Error - finding STDEVT extension'/
     :      /' in FITS file.')
            STATUS = SAI__ERROR
            GOTO 999
         END IF
      ENDDO
      IF (STATUS.NE.SAI__OK) GOTO 999

*  Create dynamic arrays and read event data
      PI    = .FALSE.
      FBEG  = 1
      DO N1 = 1,7
         COL = N1
         IF (TTYPE(N1)(1:4) .EQ. 'TIME') THEN
            CALL DYN_MAPD(1,NROWS,PTRA(1),STATUS)
            CALL FTGCVD(IUNIT,COL,FBEG,1,NROWS,0.D0,%VAL(PTRA(1)),
     :      ANYF,STATUS)
         ELSE IF (TTYPE(N1)(1:1) .EQ. 'X') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(2),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(2)),
     :      ANYF,STATUS)
         ELSE IF (TTYPE(N1)(1:1) .EQ. 'Y') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(3),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(3)),
     :      ANYF,STATUS)
*        RAWX for HSI or DETX for PSPS
         ELSE IF (TTYPE(N1)(1:4) .EQ. 'RAWX' .OR. TTYPE(N1)(1:4)
     :      .EQ. 'DETX') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(4),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(4)),
     :      ANYF,STATUS)
*        RAWX for HSI or DETX for PSPS
         ELSE IF (TTYPE(N1)(1:4) .EQ. 'RAWY'.OR. TTYPE(N1)(1:4)
     :      .EQ. 'DETY') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(5),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(5)),
     :      ANYF,STATUS)
         ELSE IF (TTYPE(N1)(1:3) .EQ. 'PHA') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(6),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(6)),
     :      ANYF,STATUS)
            PHACOL=COL
         ELSE IF (TTYPE(N1)(1:2) .EQ. 'PI') THEN
            CALL DYN_MAPI(1,NROWS,PTRA(7),STATUS)
            CALL FTGCVJ(IUNIT,COL,FBEG,1,NROWS,0,%VAL(PTRA(7)),
     :      ANYF,STATUS)
            PI = .TRUE.
         ENDIF
        IF (STATUS.NE.SAI__OK) GOTO 999
      ENDDO

*     A special case for HRI. If it can't find a PI channel then it
*     maps to the PHA channel.
      IF (.NOT. PI ) THEN
          CALL DYN_MAPI(1,NROWS,PTRA(7),STATUS)
          CALL FTGCVJ(IUNIT,PHACOL,FBEG,1,NROWS,0,%VAL(PTRA(7)),
     :    ANYF,STATUS)
      END IF
*
      LOWER = 1
*
      CALL XRTSORT_DOIT_BIN(%val(PTRA(1)),
     &   %val(PTRA(2)),%val(PTRA(3)), %val(PTRA(4)),
     &   %val(PTRA(5)), %val(PTRA(6)),%val(PTRA(7)),
     &   NROWS, SDATA, SDIM1, SDIM2,
     &   SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &   BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &   MDIM1,MDIM2,MRES,SMASK,BMASK,
     &   SRT_QUAL_MORE, MAXBAD, NBAD, STBAD, ENBAD, XWIDTH, YWIDTH,
     &   XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &   BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)
*
      CALL DYN_UNMAP(PTRA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
*
      CALL FTMAHD(IUNIT, 1, HTYPE, STATUS)
      CALL FTGKYS(IUNIT,'INSTRUMENT',INSTRUMENT,COMMENT,STATUS)
      IF(INSTRUMENT .EQ. 'HRI') THEN
         CALL MSG_SETI('BAD', BADEV)
         CALL MSG_PRNT('XRTSORT : Rejected ^BAD events found in '
     &   //'hotspots/deadspots')
      ENDIF
*
*     Loop over each time bin
      DO TLP=1,SDIM5
*        Calculate lower and upper times of this bin
         T1=SRT_MIN_T(1,1) + TWIDTH * (TLP-1.0)
         T2=SRT_MIN_T(1,1) + TWIDTH * TLP

*        Test if this bin was within any of the on times of the instrument
*        Set the quality value to 0 if the bin is good or 1 if it is bad.
         QVAL=.FALSE.
         DO INLP=1,HEAD_NTRANGE
            IF (HEAD_TSTART(INLP).LT.T2.AND.
     &                         HEAD_TEND(INLP).GT.T1) THEN
               QVAL = .TRUE.
               GOTO 100
            ENDIF
         ENDDO

100      CONTINUE

*        If the time is within the pre-selection windows - check if it
*       is within the windows selected in XSORT
         IF (QVAL) THEN
*
            QVAL=.FALSE.
            DO INLP=1,SRT_NTIME(1)
               IF ( SRT_MIN_T(INLP,1) .LT. T2 .AND.
     &                         SRT_MAX_T(INLP,1) .GT. T1 ) THEN
                  QVAL = .TRUE.
                  GOTO 110
               ENDIF
            ENDDO
         ENDIF
*
110      CONTINUE
*
*     Loop over the remaining dimensions of the source and background arrays
         DO LP7=1,SDIM7
           DO LP6=1,SDIM6
            DO LP4=1,SDIM4
             DO LP3=1,SDIM3

*       If an image then use spatial mask as well
               IF (SRT_IMAGE(1)) THEN
                 DO LP2=1,SDIM2
                    DO LP1=1,SDIM1
                       IF (.NOT. QVAL .OR. S2MASK(LP1,LP2).EQ.0) THEN
                         SQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=
     :                                                  QUAL__MISSING
                       ENDIF
                    ENDDO
                 ENDDO
               ELSE
                 DO LP2=1,SDIM2
                    DO LP1=1,SDIM1
                       IF (.NOT. QVAL) THEN
                         SQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=
     :                                                  QUAL__MISSING
                       ENDIF
                    ENDDO
                 ENDDO
               ENDIF
*              Background quality
               IF (SRT_BCKGND) THEN
                 IF (BDIM1.GT.1.AND.BDIM2.GT.1.AND.NRBIN.EQ.1) THEN
                   DO LP2=1,BDIM2
                     DO LP1=1,BDIM1
                       IF (.NOT. QVAL .OR. B2MASK(LP1,LP2).EQ.0) THEN
                         BQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=
     :                                                QUAL__MISSING
                       ENDIF
                     ENDDO
                   ENDDO
                 ELSE
                   DO LP2=1,BDIM2
                     DO LP1=1,BDIM1
                       IF (.NOT. QVAL) THEN
                         BQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=
     :                                                QUAL__MISSING
                       ENDIF
                     ENDDO
                   ENDDO
                 ENDIF
               ENDIF

            ENDDO
           ENDDO
          ENDDO
         ENDDO
      ENDDO

* Close FITS files
      CALL FTCLOS(IUNIT, STATUS)
      CALL FIO_PUNIT(IUNIT, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_SORT_BIN',STATUS)
      ENDIF
*
      END

*+XRTSORT_DOIT_BIN     checks mapped events against sort parameters
      SUBROUTINE XRTSORT_DOIT_BIN( TIME, XPIX, YPIX,
     &              XDET,
     &              YDET, AMPL, CAMPL, NELEMS, SDATA, SDIM1, SDIM2,
     &              SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &              BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &              MDIM1,MDIM2,MRES,SMASK,BMASK,
     &              QCHECK, MAXBAD, NBAD, STBAD, ENBAD, XWIDTH, YWIDTH,
     &              XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &              BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*    Import :
      INTEGER NELEMS			! length of data arrays
      DOUBLE PRECISION TIME(NELEMS)     ! Event times
      INTEGER XPIX(NELEMS), YPIX(NELEMS)  ! Array of coordinates
      INTEGER XDET(NELEMS), YDET(NELEMS)  ! Array of detector coords
      INTEGER AMPL(NELEMS), CAMPL(NELEMS) ! Array of photon events
      INTEGER SDIM1, SDIM2, SDIM3, SDIM4, SDIM5, SDIM6, SDIM7
      INTEGER BDIM1, BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7
      LOGICAL QCHECK                    ! Check quality of each event
      INTEGER MAXBAD,NBAD               ! Dimension & NUMBER of bad arrays
      REAL STBAD(MAXBAD),ENBAD(MAXBAD)  ! Start & End times of bad data
      REAL XWIDTH,YWIDTH,PWIDTH,EWIDTH     ! Bin widths of axes
      DOUBLE PRECISION TWIDTH
      REAL XDWID,YDWID                  ! Bin widths of detector axes
      REAL BXWIDTH, BYWIDTH             ! Width of bckgnd im pixels
      INTEGER NRBIN                     ! Number of output radial bins
      INTEGER NAZBIN                    ! Number of output azim. bins
      REAL ELIPA2(NRBIN),ELIPB2(NRBIN)  ! Squares of elliptic axes
      INTEGER MDIM1,MDIM2		! Dimensions of spatial mask
      REAL MRES				! Resolution of mask
      INTEGER SMASK(MDIM1,MDIM2)	! Source mask
      INTEGER BMASK(MDIM1,MDIM2)	! Background mask
*    Import-Export :
      REAL SDATA(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)   ! Source array
      REAL BDATA(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)   ! Source array
      INTEGER BADEV                               ! No of events in hotspots
*    Functions :
      LOGICAL XRT_HSPOT
        EXTERNAL XRT_HSPOT
*    Local constants :
*    Local variables :
      INTEGER BLP,TLP
      INTEGER XEV,YEV,XDEV,YDEV,AEV,CEV
      DOUBLE PRECISION TEV,BSCTIM
      INTEGER EL1,EL2,EL3,EL4,EL5,EL6,EL7
      INTEGER MEL1,MEL2
      INTEGER SCEN_X,SCEN_Y                  ! Centre pixel position of src box
      INTEGER BCEN_X,BCEN_Y                  ! Centre pixel position of bck box
      INTEGER SAVE_YMIN,SAVE_YMAX
*
      REAL SAMIN2,SAMAX2,SBMIN2,SBMAX2       ! Squares of the min. and max.
*                                            ! values for the source ellip axes.
      REAL BAMIN2,BAMAX2,BBMIN2,BBMAX2       !    Same for the background.
      REAL SA2B2I,SA2B2O                     ! Product of the squares of the
*                                            ! inner and outer source axes
      REAL BA2B2I,BA2B2O                     ! Product of the squares of the
*                                            ! inner and outer background axes
      REAL SDIFFX,SDIFFY                     ! X,Y offset from box centre (pix)
      REAL BDIFFX,BDIFFY                     !    Same for the background
      REAL SELPX,SELPY                       ! X,Y position in elliptical cords
      REAL BELPX,BELPY                       !   same for the background box
      REAL SELPX2,SELPY2                     ! Square of the photon X,Y pos.
*                                            ! in source box elliptical coords
      REAL BELPX2,BELPY2                     ! Square of the photon X,Y pos.
*                                            ! in bckgnd box elliptical coords
      REAL SCPHI,SSPHI                       ! Cos and Sine of src angle
      REAL BCPHI,BSPHI                       ! Cos and Sine of bck angle
*
      LOGICAL LHRI                           ! Is it an HRI file ?
      LOGICAL SOK,BOK
      LOGICAL OK
      INTEGER IX,YMAX                        ! Counter

*-
      SAVE_YMIN = SRT_MIN_Y(1)
      SAVE_YMAX = SRT_MAX_Y(1)

      YMAX = HEAD_YEND

***   Test if this is an HRI file
      LHRI = (INDEX(HEAD_DETECTOR, 'HRI') .NE. 0)


***   Calculate the pixel centres of each box
      SCEN_X = (SRT_MIN_X(1) + SRT_MAX_X(1)) / 2.0
      SCEN_Y = (SRT_MIN_Y(1) + SRT_MAX_Y(1)) / 2.0
      BCEN_X = (SRT_MIN_X(2) + SRT_MAX_X(2)) / 2.0
      BCEN_Y = (SRT_MIN_Y(2) + SRT_MAX_Y(2)) / 2.0

      BSCTIM = HEAD_BASE_SCTIME

***   Calculate the squares of the elliptical axis - if any
      IF (SRT_SHAPE(1) .EQ. 'C' .OR. SRT_SHAPE(1) .EQ. 'A' .OR.
     &                              SRT_SHAPE(1) .EQ. 'E') THEN
        SAMIN2 = SRT_ELAMIN(1) **2
        SAMAX2 = SRT_ELAMAX(1) **2
        SBMIN2 = SRT_ELBMIN(1) **2
        SBMAX2 = SRT_ELBMAX(1) **2
        BAMIN2 = SRT_ELAMIN(2) **2
        BAMAX2 = SRT_ELAMAX(2) **2
        BBMIN2 = SRT_ELBMIN(2) **2
        BBMAX2 = SRT_ELBMAX(2) **2

***      Calculate the product of the squares of the two elliptical axes
        SA2B2I = SAMIN2 * SBMIN2
        SA2B2O = SAMAX2 * SBMAX2
        BA2B2I = BAMIN2 * BBMIN2
        BA2B2O = BAMAX2 * BBMAX2

***      Set a local cos and sin of the orientation angle for speed
        SCPHI = SRT_COSPHI(1)
        SSPHI = SRT_SINPHI(1)
        BCPHI = SRT_COSPHI(2)
        BSPHI = SRT_SINPHI(2)
      ENDIF

***   Loop over each element in arrays
      DO IX = 1,NELEMS
***      Copy event to simpler variables
        TEV = TIME(IX) - BSCTIM
        XEV = XPIX(IX)
        YEV = YPIX(IX)
        XDEV = XDET(IX)
        YDEV = YDET(IX)
        AEV = AMPL(IX)
        CEV = CAMPL(IX)
***      Fix for HRI no corrected events. set to value '1'
        IF (LHRI) CEV = 1

*  Check if this event is from an HRI hotspot or deadspot
        IF (LHRI .AND..NOT. XRT_HSPOT( XEV, YEV)) THEN

          BADEV = BADEV + 1

        ELSE

*  If the source box is circular, annular or elliptical, calculate
***         various numbers.
          IF (SRT_SHAPE(1) .EQ. 'C' .OR. SRT_SHAPE(1) .EQ. 'A' .OR.
     &                              SRT_SHAPE(1) .EQ. 'E') THEN

***             calculate the offset in X and Y celestial pixels from the
***             source box centre
            SDIFFX = XEV - SCEN_X
            SDIFFY = YEV - SCEN_Y

***             calculate the position in elliptical coordinates - source box
***             NB: This also handles circles
            SELPX = SDIFFX * SCPHI + SDIFFY * SSPHI
            SELPY = - SDIFFX * SSPHI + SDIFFY * SCPHI
            SELPX2 = SELPX * SELPX
            SELPY2 = SELPY * SELPY

***             calculate the position in elliptical coordinates - backgnd box
            IF (SRT_BCKGND) THEN

***                calculate the offset in X and Y celestial pixels from the
***                background box centre
              BDIFFX = XEV - BCEN_X
              BDIFFY = YEV - BCEN_Y

              BELPX = BDIFFX * BCPHI + BDIFFY * BSPHI
              BELPY = - BDIFFX * BSPHI + BDIFFY * BCPHI
              BELPX2 = BELPX * BELPX
              BELPY2 = BELPY * BELPY

            ENDIF
          ENDIF

*  Check if each event is within the selected time range
          OK = .FALSE.
          TLP=1
          DO WHILE (.NOT.OK.AND.TLP.LE.SRT_NTIME(1))
            OK= (SRT_MIN_T(TLP,1) .LE. TEV .AND. SRT_MAX_T(TLP,1)
     &                                                .GE. TEV)
            TLP=TLP+1
          ENDDO

*  If quality limits have been made more strict then check quality
          IF (QCHECK) THEN

*  See if this time is within one of the bad times
            BLP=1
            DO WHILE (OK.AND.BLP.LE.NBAD)
              IF (TEV.GE.STBAD(BLP).AND.TEV.LE.ENBAD(BLP)) THEN
                OK=.FALSE.
              ENDIF
            ENDDO

          ENDIF

*  if timing Ok then check event falls within various other limits
          IF (OK) THEN
*
            OK= (SRT_MIN_PH(1).LE. AEV.AND.SRT_MAX_PH(1).GE. AEV).AND.
     :          (SRT_MIN_EN(1).LE. CEV.AND.SRT_MAX_EN(1).GE. CEV).AND.
     :          (SRT_MIN_XD(1).LE.XDEV.AND.SRT_MAX_XD(1).GE.XDEV).AND.
     :          (SRT_MIN_YD(1).LE.YDEV.AND.SRT_MAX_YD(1).GE.YDEV)
*
          ENDIF

*  if event has survived this far check spatial selection
          SOK=OK
          BOK=OK
          IF (OK) THEN

*  rectangle
            IF ( SRT_SHAPE(1) .EQ. 'R') THEN

              SOK=((SRT_MIN_X(1) .LE. XEV .AND. SRT_MAX_X(1) .GE. XEV)
     &                               .AND.
     &             (SRT_MIN_Y(1) .LE. YEV .AND. SRT_MAX_Y(1) .GE. YEV))
              IF (SRT_BCKGND) THEN
                BOK=((SRT_MIN_X(2) .LE.XEV.AND.SRT_MAX_X(2).GE.XEV)
     &                            .AND.
     &                 (SRT_MIN_Y(2).LE.YEV.AND.SRT_MAX_Y(2).GE.YEV))
              ENDIF

*  circle, annulus or ellipse (all treated as ellipse)
            ELSEIF (INDEX('CAE',SRT_SHAPE(1)).NE.0) THEN

              SOK=((SELPX2*SBMIN2 + SELPY2*SAMIN2) .GE. SA2B2I
     &                               .AND.
     &             (SELPX2*SBMAX2 + SELPY2*SAMAX2) .LE. SA2B2O)
              IF (SRT_BCKGND) THEN
                BOK=((BELPX2*BBMIN2 + BELPY2*BAMIN2) .GE. BA2B2I
     &                              .AND.
     &                (BELPX2*BBMAX2 + BELPY2*BAMAX2) .LE. BA2B2O)
              ENDIF

*  ARD description
            ELSEIF (SRT_SHAPE(1).EQ.'I') THEN

*  find position within spatial mask
              MEL1=INT((XEV-HEAD_XSTART)/MRES)+1
              IF (HEAD_ORIGIN.EQ.'MPE') THEN
                MEL2=INT((-YEV-HEAD_YSTART)/MRES)+1
              ELSE
                MEL2=INT((YEV-HEAD_YSTART)/MRES)+1
              ENDIF
              SOK=(SMASK(MEL1,MEL2).NE.0)
              IF (SRT_BCKGND) THEN
                BOK=(BMASK(MEL1,MEL2).NE.0)
              ENDIF
            ENDIF

          ENDIF


          IF (SOK) THEN

*  Calculate position of data in array
*  The first two dimensions of the array can be either
*  X and Y pixel or RADIAL and AZIMUTHAL bin, depending on
*  the user selection.
            IF (SRT_IMAGE(1)) THEN
*
*  X,Y bins:
              EL1=INT((XEV-SRT_MIN_X(1))/XWIDTH) + 1
*  Calculate Y element according to orientation of raw pixels
              IF (HEAD_ORIGIN.EQ.'MPE') THEN
                EL2=SDIM2 - INT((YEV-SRT_MIN_Y(1))/YWIDTH)
              ELSE
                EL2=INT((YEV-SRT_MIN_Y(1))/YWIDTH) + 1
              ENDIF


            ELSE

*  No spatial axes
              IF (SDIM1.EQ.1.AND.SDIM2.EQ.1) THEN

                EL1=1
                EL2=1

*  Radial distribution
              ELSE

*  Calculate the radial (and azimuthal) bin by checking
*  each bin individually (how else ??)
                DO EL1=1,NRBIN
*
                  IF (( SELPX2 / ELIPA2(EL1) +
     &                 SELPY2 / ELIPB2(EL1) ) .LE. 1.0) GOTO 110
                ENDDO
*
*  This error message should never be activated
                CALL MSG_PRNT('Error calculating elliptical - '/
     &                             /'bin refer to author')
*
110             CONTINUE
*
*                 Set azimuthal bin to zero for now
                EL2=1
*
              ENDIF
            ENDIF
*
*  Calculate the other elements
            EL3=INT((XDEV-SRT_MIN_XD(1))/XDWID) + 1
            EL4=INT((YDEV-SRT_MIN_YD(1))/YDWID) + 1
            EL5=MIN((INT((TEV-SRT_MIN_T(1,1))/TWIDTH) + 1), SDIM5)
            EL6=INT((AEV-SRT_MIN_PH(1))/PWIDTH) + 1
            EL7=INT((CEV-SRT_MIN_EN(1))/EWIDTH) + 1


            SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &         SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0


          ENDIF

          IF ( SRT_BCKGND.AND.BOK) THEN

*  Calculate position of data in array
*  NB: no polar bins in background
            IF (BDIM1.GT.1.AND.BDIM2.GT.1) THEN
              EL1=INT((XEV-SRT_MIN_X(2))/BXWIDTH) + 1
*  Calculate Y element depending on orientation of raw pixels
              IF (HEAD_ORIGIN.EQ.'MPE') THEN
                EL2=BDIM2 - INT((YEV-SRT_MIN_Y(2))/BYWIDTH)
              ELSE
                EL2=INT((YEV-SRT_MIN_Y(2))/BYWIDTH) + 1
              ENDIF
*
            ELSE

              EL1=1
              EL2=1

            ENDIF

            EL3=INT((XDEV-SRT_MIN_XD(2))/XDWID) + 1
            EL4=INT((YDEV-SRT_MIN_YD(2))/YDWID) + 1
            EL5=INT((TEV-SRT_MIN_T(1,2))/TWIDTH) + 1
            EL6=INT((AEV-SRT_MIN_PH(2))/PWIDTH) + 1
            EL7=INT((CEV-SRT_MIN_EN(2))/EWIDTH) + 1
*

            BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &           BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0

          ENDIF

        ENDIF

      ENDDO
*

999   CONTINUE
*
      SRT_MIN_Y(1) = SAVE_YMIN
      SRT_MAX_Y(1) = SAVE_YMAX
*
      END

*+XRTSORT_WRISORT    Writes the sorting conditions into a SORT box.
      SUBROUTINE XRTSORT_WRISORT( FID, IDS, STATUS)
*    Description :
*      Writes the sorting conditions used to produce the binned dataset into
*     a .SORT box which it creates.
*    Deficiencies :
*    Bugs :
*    Authors :
*     R.D.Saxton   (LTVAD::RDS)
*    History :
*     7-Jul-1990           original  (LTVAD::RDS)
*    17-Apr-1991           changed to handle just one sort box  (LTVAD::RDS)
*    26-Apr-1991           spatial sort completely revised
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'XRTSRT_CMN'
*    Import :
      INTEGER			FID			! Output file id
      INTEGER                   IDS                     ! 1=src 2=bgnd
*    Status :
      INTEGER STATUS
*    Local constants :
      REAL RTOD
            PARAMETER (RTOD = 180.0 / 3.1415493)
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC,LOCS,LOCSS,LOCSC
      CHARACTER*(DAT__SZLOC) LOCX,LOCXC,LOCY,LOCYC
      CHARACTER*(DAT__SZLOC) LOCT,LOCTC,LOCP,LOCPC
*-
      IF (STATUS .NE. SAI__OK) RETURN


* Create SORT extension in file
      CALL ADI1_LOCINSTR( FID, .TRUE., ILOC, STATUS)
      CALL DAT_NEW(ILOC,'SORT','EXTN',0,0,STATUS)
      CALL DAT_FIND(ILOC,'SORT',LOCS,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error creating SORT extension',STATUS)
         GOTO 999
      ENDIF
*
* Create spatial box
      CALL DAT_NEW(LOCS,'SPACE','SPACE',1,1,STATUS)
      CALL DAT_FIND(LOCS,'SPACE',LOCSS,STATUS)
      CALL DAT_CELL(LOCSS,1,1,LOCSC,STATUS)
*
* Write shape element
      CALL HDX_PUTC(LOCSC,'SHAPE',1,SRT_SHAPE(IDS),STATUS)
*
* Write orientation
      CALL HDX_PUTR(LOCSC,'PHI',1,SRT_PHI(IDS)*RTOD,STATUS)
*
* Write X and Y centre
      CALL HDX_PUTR(LOCSC,'XCENT',1,SRT_XCENT(IDS),STATUS)
      CALL HDX_PUTR(LOCSC,'YCENT',1,SRT_YCENT(IDS),STATUS)
*
* Write X and Y axis min and max radii. NB: This applies to every shape in
* our limited book, i.e. rectangles,circles,annuli and ellipses
      CALL HDX_PUTR(LOCSC,'XINNER',1,
     :           SRT_ELAMIN(IDS) * SRT_PTOD,STATUS)
      CALL HDX_PUTR(LOCSC,'XOUTER',1,
     :           SRT_ELAMAX(IDS) * SRT_PTOD,STATUS)
      CALL HDX_PUTR(LOCSC,'YINNER',1,
     :           SRT_ELBMIN(IDS) * SRT_PTOD,STATUS)
      CALL HDX_PUTR(LOCSC,'YOUTER',1,
     :           SRT_ELBMAX(IDS) * SRT_PTOD,STATUS)
*
      CALL DAT_ANNUL(LOCSC, STATUS)
      CALL DAT_ANNUL(LOCSS, STATUS)
*
* Check spatial region was written ok.
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error writing X and Y sort ranges',STATUS)
         GOTO 999
      ENDIF

* Create X detector element and write it
      CALL DAT_NEW(LOCS,'XDET','XDET',1,1,STATUS)
      CALL DAT_FIND(LOCS,'XDET',LOCX,STATUS)

      CALL DAT_CELL(LOCX,1,1,LOCXC,STATUS)
      CALL HDX_PUTI(LOCXC,'START',1,SRT_MIN_XD(IDS),STATUS)
      CALL HDX_PUTI(LOCXC,'STOP',1,SRT_MAX_XD(IDS),STATUS)
*
      CALL DAT_ANNUL(LOCXC, STATUS)
      CALL DAT_ANNUL(LOCX, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error writing X det. sort range',STATUS)
         GOTO 999
      ENDIF

* Create Y element and write it
      CALL DAT_NEW(LOCS,'YDET','YDET',1,1,STATUS)
      CALL DAT_FIND(LOCS,'YDET',LOCY,STATUS)

      CALL DAT_CELL(LOCY,1,1,LOCYC,STATUS)
      CALL HDX_PUTI(LOCYC,'START',1,SRT_MIN_YD(IDS),STATUS)
      CALL HDX_PUTI(LOCYC,'STOP',1,SRT_MAX_YD(IDS),STATUS)
*
      CALL DAT_ANNUL(LOCYC, STATUS)
      CALL DAT_ANNUL(LOCY, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error writing Y det. sort range',STATUS)
         GOTO 999
      ENDIF

* Create Time element and write it
      CALL DAT_NEW(LOCS,'TIME','TIME',1,1,STATUS)
      CALL DAT_FIND(LOCS,'TIME',LOCT,STATUS)
*
      CALL DAT_CELL(LOCT,1,1,LOCTC,STATUS)
      CALL HDX_PUTD(LOCTC,'START',SRT_NTIME(IDS),
     :                            SRT_MIN_T(1,IDS),STATUS)
      CALL HDX_PUTD(LOCTC,'STOP',SRT_NTIME(IDS),
     :                            SRT_MAX_T(1,IDS),STATUS)
*
      CALL DAT_ANNUL(LOCTC, STATUS)
      CALL DAT_ANNUL(LOCT, STATUS)
*
* Create Sumsig element and write it
      CALL DAT_NEW(LOCS,'PH_CHANNEL','PH_CHANNEL',1,1,STATUS)
      CALL DAT_FIND(LOCS,'PH_CHANNEL',LOCP,STATUS)

      CALL DAT_CELL(LOCP,1,1,LOCPC,STATUS)
      CALL HDX_PUTI(LOCPC,'START',1,SRT_MIN_PH(IDS),STATUS)
      CALL HDX_PUTI(LOCPC,'STOP',1,SRT_MAX_PH(IDS),STATUS)
*
      CALL DAT_ANNUL(LOCPC, STATUS)
      CALL DAT_ANNUL(LOCP, STATUS)
*
*   Create corrected PH chanel element and write it
      CALL DAT_NEW(LOCS,'ENERGY','ENERGY',1,1,STATUS)
      CALL DAT_FIND(LOCS,'ENERGY',LOCP,STATUS)

      CALL DAT_CELL(LOCP,1,1,LOCPC,STATUS)
      CALL HDX_PUTI(LOCPC,'START',1,SRT_MIN_EN(IDS),STATUS)
      CALL HDX_PUTI(LOCPC,'STOP',1,SRT_MAX_EN(IDS),STATUS)
*
      CALL DAT_ANNUL(LOCPC, STATUS)
      CALL DAT_ANNUL(LOCP, STATUS)
*
      CALL DAT_ANNUL(LOCS, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing SORT structure in '/
     &                   /'background file')
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_OUT(' ','from XRTSORT_WRISORT',STATUS)
      ENDIF
      END






*+XRTSORT_WRISORT_ARD    Writes the sorting conditions into selection structure
      SUBROUTINE XRTSORT_WRISORT_ARD(ID, VERSION, IDS, STATUS)
*    Description :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'XRTSRT_CMN'
*    Import :
      INTEGER ID		       ! ID of file
      INTEGER IDS
      CHARACTER*(*) VERSION
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      INTEGER SID
*-
      IF (STATUS .NE. SAI__OK) RETURN

      CALL SLN_NEWREC(VERSION,SID,STATUS)

*  write spatial selection
      CALL SLN_PUTARD(SID,'SPACE',SRT_ARDID(IDS),STATUS)

*  write detector coordinate selection
      CALL SLN_PUTRNGI(SID,'XDET',1,SRT_MIN_XD(IDS),SRT_MAX_XD(IDS),
     :                                                       STATUS)
      CALL SLN_PUTRNGI(SID,'YDET',1,SRT_MIN_YD(IDS),SRT_MAX_YD(IDS),
     :                                                       STATUS)

*  write time ranges
      CALL SLN_PUTRNGI(SID,'TIME',SRT_NTIME(IDS),
     :             SRT_MIN_T(1,IDS),SRT_MAX_T(SRT_NTIME(IDS),IDS),
     :                                                     STATUS)

*  write PH channel selection
      CALL SLN_PUTRNGI(SID,'PH_CHANNEL',1,SRT_MIN_PH(IDS),
     :                                 SRT_MAX_PH(IDS),STATUS)

*  write corrected PH channel selection
      CALL SLN_PUTRNGI(SID,'ENERGY',1,SRT_MIN_EN(IDS),
     :                             SRT_MAX_EN(IDS),STATUS)

      CALL SLN_PUTREC(ID,SID,STATUS)

*
      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_OUT(' ','from XRTSORT_WRISORT',STATUS)
      ENDIF
*
      END

*+XRTSORT_RADEC2AXIS - converts an RA/DEC to image axis coordinates
      SUBROUTINE XRTSORT_RADEC2AXIS(RA,DEC,TMAT,X,Y,STATUS)
*    Description :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      DOUBLE PRECISION RA,DEC
      DOUBLE PRECISION TMAT(3,3)
*    Export :
      REAL X,Y
*    Status :
      INTEGER STATUS
*    Local constants :
      DOUBLE PRECISION DTOR,RTOD
      PARAMETER (DTOR=3.14159265/180.,RTOD=1.0/DTOR)
*    Local variables :
      REAL U(3),V(3)
      INTEGER J
*-
      IF (STATUS .NE. SAI__OK) RETURN

*  Calculate azimuth and elevation for the source box in raw pixels
      CALL CONV_CONA2V(REAL(RA)*REAL(DTOR),
     &                           REAL(DEC)*REAL(DTOR),U)

      DO J = 1,3
        V(J) = U(1)*TMAT(1,J)+U(2)*TMAT(2,J)+U(3)*TMAT(3,J)
      ENDDO

      X=ATAN2(V(2),V(1))*REAL(RTOD)
      Y=ASIN(V(3))*REAL(RTOD)

      IF (STATUS .NE. SAI__OK) THEN
          CALL MSG_OUT(' ','from XRTSORT_RADEC2AXIS',STATUS)
      ENDIF

      END


*+XRTSORT_AXIS2RADEC - converts  image axis coordinates to an RA/DEC
      SUBROUTINE XRTSORT_AXIS2RADEC(X,Y,TMAT,RA,DEC,STATUS)
*    Description :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL X,Y
      DOUBLE PRECISION TMAT(3,3)
*    Export :
      DOUBLE PRECISION RA,DEC
*    Status :
      INTEGER STATUS
*    Local constants :
      DOUBLE PRECISION DTOR,RTOD
      PARAMETER (DTOR=3.14159265/180.,RTOD=1.0/DTOR)
*    Local variables :
      REAL U(3),V(3)
      INTEGER J
*-
      IF (STATUS .NE. SAI__OK) RETURN

*  Calculate azimuth and elevation for the source box in raw pixels
      CALL CONV_CONA2V(X*REAL(DTOR),Y*REAL(DTOR),U)

      DO J = 1,3
        V(J) = U(1)*TMAT(J,1)+U(2)*TMAT(J,2)+U(3)*TMAT(J,3)
      ENDDO

      RA=ATAN2(V(2),V(1))*RTOD
      IF (RA.LT.0.0D0) THEN
        RA=RA+180.0D0
      ENDIF
      DEC=ASIN(V(3))*RTOD

      END

