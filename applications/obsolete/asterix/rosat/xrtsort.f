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
*  24 Apr 94 (v1.7-0) for new asterix release
*  25 May 94 (v1.7-1) PSF structure added in HDS output files
*  25 Aug 95 V1.8-0   Bug fix for event datasets (RJV)
*  11 Sep 95 V1.8-1   OMD support removed (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*
*    Global variables :
      INCLUDE 'INC_XRTHEAD'
      INCLUDE 'INC_XRTSRT'
*
*    Status :
      INTEGER                 STATUS
*    Functions :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local Constants :
      CHARACTER*30            VERSION
         PARAMETER          ( VERSION = 'XSORT Version 1.8-1' )

*    Local variables :
      RECORD /XRT_HEAD/ HEAD                    ! Observation header information
      RECORD /XRT_SCFDEF/ SRT, BSRT             ! Sorting parameters for source
*                                               ! and background files
*
      CHARACTER*(DAT__SZLOC)  LOCS		! Locator to Binned file
      CHARACTER*(DAT__SZLOC)  LOCB		! Locator to Binned bckgnd file
      CHARACTER*(DAT__SZLOC)  ELOCS(7)		! Locators to EVDS source lists
      CHARACTER*(DAT__SZLOC)  ELOCB(7)		! Locators to EVDS bckgnd lists
      CHARACTER*30 VERS       	                ! SASS version date

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
      INTEGER                 SEVPTR(7)         ! Pointers to source lists
      INTEGER                 BEVPTR(7)         ! Pointers to bckgnd lists
      INTEGER                 WPNTR1,WPNTR2     ! Pointer to workspace arrays
      INTEGER		      MAPLIM		! Current mapping extent of EVDS lists

      INTEGER                 LP                ! Loop variable
      INTEGER                 TOTEV_SRC         ! Number of events in source box
      INTEGER                 TOTEV_BCK         ! Number of events in bckgnd box

      REAL                    MRES		! Resolution of spatial mask
*
*-

      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*  Get directory name from user and display the available observations.
*  Get rootname of file wanted.
      CALL XRTSORT_FILESELECT(SRT, HEAD, STATUS)
*

*  Read header info from the corresponding _HDR.SDF file
      CALL RAT_GETXRTHEAD(SRT.ROOTNAME, HEAD, STATUS)

*
* Get type of output dataset - binned or event?
      CALL USI_GET0C('TYPE', SRT.DTYPE, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*
* Check if response made sense
      CALL CHR_UCASE(SRT.DTYPE)
      IF (SRT.DTYPE(1:1).EQ.'B') THEN
        SRT.DTYPE='BinDS'
      ELSEIF (SRT.DTYPE(1:1).EQ.'E') THEN
        SRT.DTYPE='EventDS'
      ELSE
        CALL MSG_PRNT('AST_ERR: invalid data type')
        GOTO 999
      ENDIF

*
*  Get the sort control information from the user
      CALL XRTSORT_RANGESELECT(HEAD,SRT,BSRT,SDIM,BDIM,NRBIN,NAZBIN,
     :                                  MDIM,MRES,SMPTR,BMPTR,STATUS)

      CALL USI_TASSOCO('OUT',SRT.DTYPE,SID,STATUS)
      CALL ADI1_GETLOC(SID,LOCS,STATUS)
      IF (SRT.BCKGND) THEN
        CALL USI_TASSOCO('BOUT',SRT.DTYPE,BID,STATUS)
        CALL ADI1_GETLOC(BID,LOCB,STATUS)
      ENDIF

      IF ( STATUS .NE. SAI__OK ) GOTO 999


*  Sorting to a binned data set ?
      IF ( SRT.DTYPE .EQ. 'BinDS') THEN

*  Create the output data set and get a pointer to mapped data array.
         CALL XRTSORT_CRE_BINNED (HEAD, SRT, LOCS, SDIM, NRBIN,
     &                                   SRCPTR, SQPTR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999
*
*  Create a background dataset if required
         IF (SRT.BCKGND) THEN
*
            CALL XRTSORT_CRE_BINNED (HEAD, BSRT, LOCB, BDIM, 1,
     &                                   BCKPTR, BQPTR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
*
         ELSE
*
*     Dummy up a couple of arrays.
            CALL DYN_MAPR(7,BDIM,BCKPTR,STATUS)
            CALL DYN_MAPB(7,BDIM,BQPTR,STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error mapping dynamic arrays')
               GOTO 999
            ENDIF
         ENDIF
*
*   Map a couple of workspace arrays
         CALL DYN_MAPR(1, NRBIN, WPNTR1, STATUS)
         CALL DYN_MAPR(1, NRBIN, WPNTR2, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping dynamic arrays')
            GOTO 999
         ENDIF
*
*
         CALL XRTSORT_SORT_BIN(HEAD, SRT, BSRT, SDIM(1), SDIM(2),
     &          SDIM(3), SDIM(4), SDIM(5), SDIM(6), SDIM(7),
     &          BDIM(1), BDIM(2), BDIM(3), BDIM(4), BDIM(5), BDIM(6),
     &          BDIM(7), NRBIN, NAZBIN, MDIM(1), MDIM(2),
     &          MRES,%val(SMPTR), %val(BMPTR),
     &          %val(WPNTR1), %val(WPNTR2), %val(SRCPTR),
     &          %val(BCKPTR), %val(SQPTR), %val(BQPTR), STATUS)
*
*
         CALL DYN_UNMAP(SMPTR,STATUS)
         IF (SRT.BCKGND) THEN
           CALL DYN_UNMAP(BMPTR,STATUS)
         ENDIF
         CALL DYN_UNMAP(WPNTR1,STATUS)
         CALL DYN_UNMAP(WPNTR2,STATUS)

         IF (STATUS .NE. SAI__OK) GOTO 999
*
*  Sorting to an Event data set
      ELSEIF (SRT.DTYPE .EQ. 'EventDS') THEN
*
*   Calculate the maximum number of photons which may be put into
*   the event lists.
         CALL XRTSORT_PHOTONCNT(HEAD, SRT, BSRT, MAPLIM, STATUS)
*
*   Create & map the output Event data set.
         CALL XRTSORT_CRE_EVENT(HEAD, SRT , MAPLIM, LOCS,
     &                                       ELOCS, SEVPTR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999
*
*   Create background file if wanted
         IF (SRT.BCKGND) THEN
            CALL XRTSORT_CRE_EVENT(HEAD, BSRT, MAPLIM, LOCB,
     &                                     ELOCB, BEVPTR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
*
         ELSE
*
*     Otherwise create dummy arrays
            DO LP=1,7
               CALL DYN_MAPR(1, MAPLIM, BEVPTR(LP), STATUS)
            ENDDO
*
*
         ENDIF
*
*   Do the sort
         CALL XRTSORT_SORT_EVE(HEAD, SRT, BSRT, MAPLIM,
     &          %val(SEVPTR(1)), %val(SEVPTR(2)), %val(SEVPTR(3)),
     &          %val(SEVPTR(4)), %val(SEVPTR(5)), %val(SEVPTR(6)),
     &          %val(SEVPTR(7)), %val(BEVPTR(1)), %val(BEVPTR(2)),
     &          %val(BEVPTR(3)), %val(BEVPTR(4)), %val(BEVPTR(5)),
     &          %val(BEVPTR(6)), %val(BEVPTR(7)),
     &          MDIM(1),MDIM(2),MRES,%val(SMPTR),%val(BMPTR),
     &          TOTEV_SRC,TOTEV_BCK, STATUS )
*

         IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Tidy up and shrink lists to the number of events recorded
         DO LP=1,7
            CALL DAT_UNMAP(ELOCS(LP), STATUS)
            CALL DAT_ALTER(ELOCS(LP), 1, TOTEV_SRC, STATUS)
            CALL DAT_ANNUL(ELOCS(LP),STATUS)
         ENDDO
*
         IF (SRT.BCKGND) THEN
*
            DO LP=1,7
               CALL DAT_UNMAP(ELOCB(LP), STATUS)
               CALL DAT_ALTER(ELOCB(LP), 1, TOTEV_BCK, STATUS)
               CALL DAT_ANNUL(ELOCB(LP),STATUS)
            ENDDO
*
         ELSE
            DO LP=1,7
               CALL DYN_UNMAP(BEVPTR(LP),STATUS)
            ENDDO
*
         ENDIF
      ENDIF
*
*   Put SORT box into output files
      CALL XRTSORT_WRISORT(SID, VERSION, SRT, STATUS)
*

*   Background sort box
      IF (SRT.BCKGND) THEN
         CALL XRTSORT_WRISORT(BID, VERSION,  BSRT, STATUS)
      ENDIF

*   History
      CALL HSI_NEW(SID, STATUS)
      CALL HSI_ADD(SID, VERSION, STATUS)

      IF (SRT.BCKGND) THEN
         CALL HSI_NEW(BID, STATUS)
         CALL HSI_ADD(BID, VERSION, STATUS)
      ENDIF
*
999   CONTINUE
*
*   Tidy up
      CALL USI_ANNUL('OUT',STATUS)
      IF (SRT.BCKGND) THEN
         CALL USI_ANNUL('BOUT',STATUS)
      ENDIF
      CALL AST_CLOSE()
*

      END


*+XRTSORT_AXES   Writes the axes info into the output datafile
	SUBROUTINE XRTSORT_AXES(LOC,NELS,NRBIN,NAXES,AXES,HIGH,LOW,
     :                                                       STATUS)
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
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Import :
      CHARACTER*(DAT__SZLOC) LOC        !Locator of HDS file

      INTEGER NELS(7)                   !Number of elements in each dimension
      INTEGER NRBIN                     !Number of radial bins
      INTEGER NAXES                     !Number of axes of output array
      INTEGER AXES(8)                   !Code for each axis (1-8)
      REAL HIGH(8),LOW(8)               !Extreme values for each dimension
* Export :
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*6 VARIANT(8)               ! The type of data-array
      INTEGER DIMENSION(8)                 ! Length of each axis
      REAL BASE(8)                         ! Zero point for each axis
      REAL SCALE(8)                        ! Scale value for each axis
      LOGICAL NORMALISED(8)                ! Data normalised to this axis ?
      CHARACTER*30 LABEL(8)                ! Axes labels
      CHARACTER*30 UNITS(8)                ! Axes units
      INTEGER PNTR,LP,AXLP
      INTEGER SELS(8)                      ! An array of dimensions
*-
* Check status:
      IF (STATUS .NE. SAI__OK) RETURN
*
* Create new element array
      DO LP=1,7
         SELS(LP) = NELS(LP)
      ENDDO
      SELS(8) = NRBIN
*
* Set up units and labels for axes
*
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
*
* Set arrays for outputting later
*
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
*
* Create axis structure in output file
*
      CALL BDA_CREAXES(LOC,NAXES,STATUS)
*
      IF (STATUS.NE.SAI__OK) GOTO 999
*
* Put in components of each axis
*
      DO AXLP=1,NAXES
*
         CALL BDA_PUTAXLABEL(LOC,AXLP,LABEL(AXLP),STATUS)
*
         CALL BDA_PUTAXUNITS(LOC,AXLP,UNITS(AXLP),STATUS)
*
         CALL BDA_PUTAXNORM(LOC,AXLP,NORMALISED(AXLP),STATUS)
*
* Write axis data as a spaced array
*
         CALL BDA_PUTAXVAL(LOC, AXLP, BASE(AXLP), SCALE(AXLP),
     &                                   DIMENSION(AXLP), STATUS)
      ENDDO
*
999   CONTINUE
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from XRTSORT_AXES',STATUS)
      ENDIF
*
      END

*+XRTSORT_BADTIME     Finds times when quality is outside defined range
      SUBROUTINE XRTSORT_BADTIME(SRT, OBS, MAXBAD, NBAD, STBAD,
     &                              ENBAD, STATUS)
*    Description :
*      Interogates a file to find the times when data has been rejected
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
*    Import :
      RECORD /XRT_SCFDEF/ SRT          ! Sort control structure
      INTEGER OBS                      ! Observation number
      INTEGER MAXBAD                   ! Max number of bad time ranges
*    Import-Export :
*    Export :
      INTEGER NBAD                     ! Number of bad time ranges
      REAL STBAD(MAXBAD)               ! Start times of bad time ranges
      REAL ENBAD(MAXBAD)               ! End times of bad time ranges
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*3 COBS
      CHARACTER*80 QUAFIL                          ! Quality filename
      CHARACTER*(DAT__SZLOC) QLOC                  ! Locator to quality file
      INTEGER QLEN                                 ! Length of event lists
      INTEGER TPNTR,TEPNTR,GPNTR                   ! Pointers to arrays
      INTEGER K
*    Local data :
*     <any DATA initialisations for local variables>
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Create quality filename
      CALL CHR_ITOC(OBS, COBS, K)
      QUAFIL=SRT.ROOTNAME//'_'//COBS//'.QUAL'
*
* Open file of quality values
      CALL HDS_OPEN(QUAFIL, 'READ', QLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC('QUALFIL', QUAFIL)
         CALL MSG_PRNT('Error opening quality file ^QUALFIL')
         GOTO 999
      ENDIF
*
* Map quality arrays from file
      CALL CMP_MAPV(QLOC, 'TIME', '_REAL', 'READ', TPNTR, QLEN, STATUS)
      CALL CMP_MAPV(QLOC, 'TEMPERATURE', '_INTEGER', 'READ', TEPNTR,
     &                                                    QLEN, STATUS)
      CALL CMP_MAPV(QLOC, 'GAIN', '_INTEGER', 'READ', GPNTR,
     &                                                    QLEN, STATUS)
*
* Find the quality bad times
      CALL XRTSORT_BADTIME_DOIT(QLEN, %val(TPNTR), %val(TEPNTR),
     &                 %val(GPNTR), SRT, MAXBAD, NBAD, STBAD, ENBAD)
*
* Unmap and annul
      CALL CMP_UNMAP(QLOC, 'TIME', STATUS)
      CALL CMP_UNMAP(QLOC, 'TEMPERATURE', STATUS)
      CALL CMP_UNMAP(QLOC, 'GAIN', STATUS)
*
      CALL HDS_CLOSE(QLOC, STATUS)
      CALL DAT_ANNUL(QLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_BADTIME',STATUS)
      ENDIF
*
      END

*+XRTSORT_BADTIME_DOIT    Find times when quality is outside selected range
      SUBROUTINE XRTSORT_BADTIME_DOIT(QLEN, TIME, TEMP, GAIN, SRT,
     &                              MAXBAD, NBAD, STBAD, ENBAD)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
*    Import :
      INTEGER QLEN               ! Length of quality property lists
      REAL TIME(QLEN)            ! Time of quality state
      INTEGER TEMP(QLEN)         ! Temperature quality
      INTEGER GAIN(QLEN)         ! Gain quality
*
      RECORD /XRT_SCFDEF/ SRT    ! Sort control structure
*
      INTEGER MAXBAD             ! Dimension of output arrays
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      INTEGER NBAD               ! Number of bad time ranges
      REAL STBAD(MAXBAD)         ! Start of bad range
      REAL ENBAD(MAXBAD)         ! End of bad range
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER TLP
      INTEGER NLOW,NHIGH
*-
* Loop over times
      DO TLP=1,QLEN
*
*   Check if quality is within bounds for this time
         IF (TEMP(TLP).LT.SRT.TEMP_MIN .OR. TEMP(TLP).GT.SRT.TEMP_MAX
     &       .OR. GAIN(TLP) .LT. SRT.GAIN_MIN .OR.
     &            GAIN(TLP) .GT. SRT.GAIN_MAX) THEN
*
            IF (NLOW .EQ. NHIGH) THEN
               NLOW=NLOW+1
               STBAD(NLOW)=TIME(TLP)
            ENDIF
*
         ELSEIF (NLOW .GT. NHIGH) THEN
               NHIGH=NHIGH+1
               ENBAD(NHIGH)=TIME(TLP)
         ENDIF
*
      ENDDO
*
      END

*+XRTSORT_CRE_ASTERIX - Create an ASTERIX structure
      SUBROUTINE XRTSORT_CRE_ASTERIX( OUTLOC, SRT, HEAD, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      CHARACTER*(DAT__SZLOC) OUTLOC
      RECORD /XRT_HEAD/ HEAD
      RECORD /XRT_SCFDEF/ SRT
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables:
      INTEGER                 LOOP,LOOP2          ! Loop counter
      REAL                    EXPO_TIM            ! Exposure time (s)
      REAL                    OBSLEN              ! Observation length (s)time
      CHARACTER*20	      SDATE
      DOUBLE PRECISION	      STAI
      DOUBLE PRECISION	      BASEUT

      CHARACTER*(DAT__SZLOC)  HEA              ! HEADER
      CHARACTER*(DAT__SZLOC)  INS              ! INSTRUMENT
      CHARACTER*(DAT__SZLOC)  SOR              ! SORT
      CHARACTER*(DAT__SZLOC)  PSF              ! PSF
      CHARACTER*(DAT__SZLOC)  AST              ! ASTERIX
      CHARACTER*(DAT__SZLOC)  MOR              ! MORE

*-

*   Check status - return if bad
      IF (STATUS .NE. SAI__OK) RETURN
*
*    Create the MORE ASTERIX and HEADER boxes
      CALL BDA_CREHEAD( OUTLOC, STATUS )
      CALL BDA_LOCHEAD(OUTLOC, HEA, STATUS)
*    Create an instrument extension
      CALL BDA_CREINSTR( OUTLOC, STATUS )
      CALL BDA_LOCINSTR( OUTLOC, INS, STATUS )
*    Create an instrument extension
      CALL BDA_CREPSF( OUTLOC, STATUS )
      CALL BDA_LOCPSF( OUTLOC, PSF, STATUS )
*    Create LIVE_TIME structure
C     CALL BDA_CRELIVE( OUTLOC, STATUS )
C     CALL BDA_LOCLIVE( OUTLOC, LIV, STATUS )
*    Create and write CORRECTIONS structure for use by corrections prog
      CALL XRT_CREPROC(OUTLOC, .FALSE., .FALSE., .FALSE., STATUS)
*
*   Create & write the header components.
      CALL HDX_PUTC( HEA, 'TARGET', 1, HEAD.TARGET, STATUS )
      CALL HDX_PUTC( HEA, 'OBSERVER', 1, HEAD.OBSERVER, STATUS )
      CALL HDX_PUTC( HEA, 'OBSERVATORY', 1, 'ROSAT', STATUS)
      CALL HDX_PUTC( HEA, 'INSTRUMENT', 1, 'XRT', STATUS )
      CALL HDX_PUTD( HEA, 'AXIS_RA', 1, HEAD.AXIS_RA, STATUS )
      CALL HDX_PUTD( HEA, 'AXIS_DEC', 1, HEAD.AXIS_DEC, STATUS )
      CALL HDX_PUTD( HEA, 'FIELD_RA', 1, SRT.FIELD_RA, STATUS )
      CALL HDX_PUTD( HEA, 'FIELD_DEC', 1, SRT.FIELD_DEC, STATUS )
      CALL HDX_PUTI( HEA, 'EQUINOX', 1, 2000, STATUS )
*
      IF (HEAD.ROLLCI .LT. -180.D0) HEAD.ROLLCI = HEAD.ROLLCI + 360.D0
      IF (HEAD.ROLLCI .GT.  180.D0) HEAD.ROLLCI = HEAD.ROLLCI - 360.D0
*
      CALL HDX_PUTD( HEA, 'POSITION_ANGLE', 1, HEAD.ROLLCI, STATUS )
*
*  Times:
      CALL HDX_PUTI( HEA, 'BASE_MJD', 1, INT(HEAD.BASE_MJD), STATUS)
      CALL CONV_MJDDAT( HEAD.BASE_MJD, SDATE)
      CALL HDX_PUTC( HEA, 'BASE_DATE', 1, SDATE(1:11), STATUS )
      BASEUT = DMOD(HEAD.BASE_MJD, 1.0D0) * 86400.0
      CALL HDX_PUTD( HEA, 'BASE_UTC', 1, BASEUT, STATUS )
*
*   Convert to atomic time
      CALL TIM_MJD2TAI( HEAD.BASE_MJD, STAI)
      CALL HDX_PUTD(HEA, 'BASE_TAI', 1, STAI, STATUS)
*
      OBSLEN = 0.0
      DO LOOP=1,SRT.NTIME
         OBSLEN = OBSLEN + SRT.MAX_T(LOOP) - SRT.MIN_T(LOOP)
      ENDDO
      CALL HDX_PUTR( HEA, 'OBS_LENGTH', 1, OBSLEN, STATUS)
*
* Calculate exposure time and write to header
      EXPO_TIM=0.0
      DO LOOP=1,HEAD.NTRANGE
        DO LOOP2=1,SRT.NTIME
*
          IF ( SRT.MIN_T(LOOP2) .LT. HEAD.TEND(LOOP) .AND.
     &          SRT.MAX_T(LOOP2) .GT. HEAD.TSTART(LOOP)) THEN
*
            EXPO_TIM = EXPO_TIM + MIN(HEAD.TEND(LOOP), SRT.MAX_T(LOOP2))
     &                    - MAX (HEAD.TSTART(LOOP), SRT.MIN_T(LOOP2))
          ENDIF
*
        ENDDO
      ENDDO
*
      CALL HDX_PUTR( HEA, 'EXPOSURE_TIME', 1, EXPO_TIM, STATUS)

*    Create elements in the livetime extension
C      CALL DAT_NEW ( LIV, 'ON',  '_DOUBLE', 1, SRT.NWINS, STATUS )
C      CALL DAT_NEW ( LIV, 'OFF', '_DOUBLE', 1, SRT.NWINS, STATUS )
C      CALL CMP_PUT1D (LIV, 'ON', SRT.NWINS, SRT.WST, STATUS)
C      CALL CMP_PUT1D (LIV, 'OFF',SRT.NWINS, SRT.WET, STATUS)

*    Create elements in the instrument extension
      CALL HDX_PUTR ( INS, 'PIXEL_SIZE', 1, HEAD.PIXEL, STATUS )
      CALL HDX_PUTC ( INS, 'FILTER', 1, HEAD.FILTER, STATUS)
      CALL HDX_PUTC ( INS, 'DETECTOR', 1, HEAD.DETECTOR, STATUS)
*
*    Write spacecraft clock start time and conversion time
      CALL HDX_PUTD ( INS, 'SC_BASE', 1, HEAD.BASE_SCTIME, STATUS )
      CALL HDX_PUTD ( INS, 'SC_CONV', 1, HEAD.SCCONV, STATUS )
*
*    Write in the raw data type - MPE, US & RDF at the moment.
      CALL HDX_PUTC ( INS, 'RAWDATA', 1, HEAD.ORIGIN, STATUS )
      CALL HDX_PUTC ( INS, 'SASS_VERSION',1,HEAD.SASS_DATE,STATUS )
*
*    Create elements in the PSF extension
      CALL HDX_PUTC( PSF, 'LIBRARY_NAME', 1, 'PSFLIB', STATUS)
      IF (HEAD.DETECTOR .EQ. 'HRI') THEN
         CALL HDX_PUTC( PSF, 'ROUTINE_NAME', 1, 'XRT_HRI', STATUS)
      ELSE
         CALL HDX_PUTC( PSF, 'ROUTINE_NAME', 1, 'XRT_PSPC', STATUS)
      ENDIF
*
C     CALL BDA_ANNUL(HEA, STATUS)
C     CALL BDA_ANNUL(INS, STATUS)
C     CALL BDA_ANNUL(PSF, STATUS)
C     CALL BDA_ANNUL(LIV, STATUS)
*
*      Check status
999   IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_CRE_ASTERIX',STATUS)
      ENDIF
*
      END

*+XRTSORT_CRE_BINNED - Create output binned dataset
      SUBROUTINE XRTSORT_CRE_BINNED(HEAD, SRT, OUTLOC, SDIM, NRBIN,
     &                                      ARRPTR, QPTR, STATUS )
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
*     8-Apr-1994    parameter LMPE removed, now checks HEAD.YSTART (JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD
      RECORD /XRT_SCFDEF/ SRT
      CHARACTER*(DAT__SZLOC)  OUTLOC            ! HDS locator to binned file
      INTEGER SDIM(7)                           ! Dimensions of binned axes
      INTEGER NRBIN                             ! Number of radial bins
*    Import-Export :
*    Export :
      INTEGER ARRPTR                            ! Pointer to mapped array
      INTEGER QPTR                              ! Pointer to quality array
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZTYP)  TYPE
      INTEGER IDIMS(7)                          ! Dimensions of output array
      REAL LOW(8),HIGH(8)                       ! Range of each axis
      REAL PTOD                                 ! Pixels to degrees conversion
      INTEGER TOTELS,LP
*-
*   Check status - return if bad
      IF (STATUS .NE. SAI__OK) RETURN
*
*   Work out dataset type
      CALL XRT_GETTYPE(SRT.NAXES,SRT.BINAXIS,TYPE)
*
*   Find dimensions of output array from the SORT structure
      TOTELS=1
      DO LP=1,SRT.NAXES
         IF (SRT.BINAXIS(LP) .NE. 8) THEN
            IDIMS(LP)=SDIM(SRT.BINAXIS(LP))
         ELSE
            IDIMS(LP)=NRBIN
         ENDIF
         TOTELS=TOTELS*IDIMS(LP)
      ENDDO
*
*   Write in units and title
      CALL BDA_PUTTITLE (OUTLOC, HEAD.TITLE, STATUS)
      CALL BDA_PUTUNITS (OUTLOC, 'Counts',    STATUS)
*
*   Create and map the data array
      CALL BDA_CREDATA  (OUTLOC, SRT.NAXES, IDIMS, STATUS)
      CALL BDA_MAPDATA (OUTLOC, 'WRITE', ARRPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating output data array')
         GOTO 999
      ENDIF
*
      CALL BDA_CREQUAL (OUTLOC, SRT.NAXES, IDIMS, STATUS)
      CALL BDA_MAPQUAL (OUTLOC, 'WRITE', QPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating output quality array')
         GOTO 999
      ENDIF
*
*   Initialise data and quality
      CALL ARR_INIT1R(0.0, TOTELS, %val(ARRPTR),STATUS)
      CALL ARR_INIT1B(0, TOTELS, %val(QPTR),STATUS)
*
*   Calculate maximum and minimum axis values
      PTOD = HEAD.PIXEL/3600.
*
*    Assume centre pixel is 0,0
      HIGH(1)= - (SRT.MAX_X - HEAD.SKYCX) * PTOD
      LOW(1) = - (SRT.MIN_X - HEAD.SKYCX) * PTOD

*
      IF (HEAD.YSTART.LT.0) THEN
         HIGH(2)= - (SRT.MIN_Y - HEAD.SKYCY) * PTOD
         LOW(2) = - (SRT.MAX_Y - HEAD.SKYCY) * PTOD
      ELSE
         LOW(2)= (SRT.MIN_Y - HEAD.SKYCY) * PTOD
         HIGH(2) = (SRT.MAX_Y - HEAD.SKYCY) * PTOD
      ENDIF
*
      HIGH(3)=REAL(SRT.MAX_XD) + 0.5
      LOW(3) =REAL(SRT.MIN_XD) - 0.5
      HIGH(4)=REAL(SRT.MAX_YD) + 0.5
      LOW(4) =REAL(SRT.MIN_YD) - 0.5
      HIGH(5)=SRT.MAX_T(SRT.NTIME)
      LOW(5) =SRT.MIN_T(1)
      HIGH(6)=REAL(SRT.MAX_PH) + 0.5
      LOW(6) =REAL(SRT.MIN_PH) - 0.5
      HIGH(7)=REAL(SRT.MAX_EN) + 0.5
      LOW(7) =REAL(SRT.MIN_EN) - 0.5
      HIGH(8)=REAL(SRT.ELAMAX) * SRT.PTOD
      LOW(8) =REAL(SRT.ELAMIN) * SRT.PTOD
*
*   Create and fill axis data
      CALL XRTSORT_AXES( OUTLOC, SDIM, NRBIN, SRT.NAXES, SRT.BINAXIS,
     &                                            HIGH, LOW, STATUS)
*
*   Now create the ASTERIX box
      CALL XRTSORT_CRE_ASTERIX( OUTLOC, SRT, HEAD, STATUS )
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ',' from XRTSORT_CRE_BINNED',STATUS)
      ENDIF
*
      END

*+XRTSORT_CRE_EVENT - Create & map the output event dataset
      SUBROUTINE XRTSORT_CRE_EVENT(HEAD, SRT, MAPLIM,
     &                              OUTLOC, DATALOC, DATAPTR, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Structures :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'

*    Import :
      RECORD /XRT_HEAD/ HEAD
      RECORD /XRT_SCFDEF/ SRT
      INTEGER  MAPLIM                          ! Current mapping extent of lists
      CHARACTER*(DAT__SZLOC)  OUTLOC           ! Locator to Event dataset
*    Export :
      CHARACTER*(DAT__SZLOC)  DATALOC(7)       ! Locators to DATA_ARRAY objects

      INTEGER                 DATAPTR(7)       ! Pointers to mapped DATA_ARRAYS
*    Status :
      INTEGER                 STATUS

*    Local variables :
      CHARACTER*(DAT__SZLOC)  DELLOC
      REAL MINX,MAXX,MINY,MAXY                 ! Min. and max. X values (arcmin)
*-
*   Check status
      IF ( STATUS .NE. SAI__OK) RETURN


*   Create min and max values in arcmins - note the reversal which seems
*   to be necessary to give correct value of FIELD_MIN/MAX in output
      MAXX = -(SRT.MIN_X - HEAD.SKYCX) * SRT.PTOD * 60.
      MINX = -(SRT.MAX_X - HEAD.SKYCX) * SRT.PTOD * 60.

      IF (HEAD.YSTART .LT. 0) THEN
        MAXY = -(SRT.MIN_Y - HEAD.SKYCY) * SRT.PTOD * 60.
        MINY = -(SRT.MAX_Y - HEAD.SKYCY) * SRT.PTOD * 60.
      ELSE
        MINY = (SRT.MIN_Y - HEAD.SKYCY) * SRT.PTOD * 60.
        MAXY = (SRT.MAX_Y - HEAD.SKYCY) * SRT.PTOD * 60.
      ENDIF


*   Create the list structures to hold the event data
      CALL LIST_CREMAP( OUTLOC, 'X_CORR', '_REAL', MAPLIM, 0.0,'arcmin',
     &            MINX, MAXX, DATAPTR(1), DATALOC(1), STATUS)
*
*   Write in a flag to say that this needs to be binned backwards
      CALL DAT_FIND( OUTLOC, 'X_CORR', DELLOC, STATUS)
      CALL HDX_PUTL( DELLOC, 'DECREASING', 1, .TRUE., STATUS)
      CALL DAT_ANNUL(DELLOC, STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'Y_CORR', '_REAL', MAPLIM, 0.0,'arcmin',
     &            MINY, MAXY, DATAPTR(2), DATALOC(2), STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'X_DET', '_INTEGER', MAPLIM, 1.0,
     &           'pixels', REAL(SRT.MIN_XD), REAL(SRT.MAX_XD),
     &           DATAPTR(3), DATALOC(3), STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'Y_DET', '_INTEGER', MAPLIM, 1.0,
     &           'pixels', REAL(SRT.MIN_YD), REAL(SRT.MAX_YD),
     &           DATAPTR(4), DATALOC(4), STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'RAW_TIMETAG', '_DOUBLE', MAPLIM, 0.0d0,
     &   'seconds', SRT.MIN_T(1), SRT.MAX_T(SRT.NTIME), DATAPTR(5),
     &    DATALOC(5), STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'PULSE_HEIGHT_CH', '_INTEGER', MAPLIM,
     &           1.0,'channel no.',REAL(SRT.MIN_PH), REAL(SRT.MAX_PH),
     &                                  DATAPTR(6), DATALOC(6), STATUS)
*
      CALL LIST_CREMAP( OUTLOC, 'CORR_PH_CH', '_INTEGER', MAPLIM, 1.0,
     &          'channel_no', REAL(SRT.MIN_EN), REAL(SRT.MAX_EN),
     &           DATAPTR(7), DATALOC(7), STATUS)
*
*
*   Initialise lists
      CALL ARR_INIT1R(0.0, MAPLIM, %val(DATAPTR(1)),STATUS)
      CALL ARR_INIT1R(0.0, MAPLIM, %val(DATAPTR(2)),STATUS)
      CALL ARR_INIT1I(0, MAPLIM, %val(DATAPTR(3)),STATUS)
      CALL ARR_INIT1I(0, MAPLIM, %val(DATAPTR(4)),STATUS)
      CALL ARR_INIT1D(0.0d0, MAPLIM, %val(DATAPTR(5)),STATUS)
      CALL ARR_INIT1I(0, MAPLIM, %val(DATAPTR(6)),STATUS)
      CALL ARR_INIT1I(0, MAPLIM, %val(DATAPTR(7)),STATUS)
*
*   Write the title and units
      CALL BDA_PUTTITLE (OUTLOC, HEAD.TITLE, STATUS)
      CALL BDA_PUTUNITS (OUTLOC, 'Counts', STATUS)

*
*   Create ASTERIX structure.
      CALL XRTSORT_CRE_ASTERIX( OUTLOC, SRT, HEAD, STATUS )
*
999   IF (STATUS .NE. SAI__OK) THEN
	 CALL ERR_REP(' ','from XRTSORT_CRE_EVENT', STATUS)
      ENDIF
*
      END


*+XRTSORT_FILESELECT - Select XRT observations to sort
	SUBROUTINE XRTSORT_FILESELECT(SRT, HEAD, STATUS)
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
*     8-Dec-1993	 Looks for <rtn>_hdr file. sets HEAD.ORIGIN = 'OMD'
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
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
* Import :
* Import-Export :
      RECORD /XRT_SCFDEF/ SRT
      RECORD /XRT_HEAD/ HEAD
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
        LOGICAL LISTDIR                         ! List files on directory ?
*
        INTEGER K
        INTEGER LP
*
* Check status :
        IF (STATUS .NE. SAI__OK) RETURN
*
* Find the name of the present directory and set as a default
        CALL UTIL_GETCWD(RAWDIR, STATUS)
*
* Annul the status variable if the current directory couldn't be found
        IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
*
* Get directory name
        CALL MSG_BLNK()
	CALL USI_DEF0C('RAWDIR', RAWDIR, STATUS)
	CALL USI_GET0C('RAWDIR', RAWDIR, STATUS)
        CALL MSG_BLNK()
*
	IF (STATUS .NE. SAI__OK) GOTO 999
*
*

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
          SRT.ROOTNAME = RAWDIR(1:CHR_LEN(RAWDIR))//FIL_SEP_CH//RTNAME
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
*
*
999     CONTINUE
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from XRTSORT_FILESELECT' ,STATUS)
        ENDIF
*
        END

*+XRTSORT_GETQLIM    Get quality limits from file and user
      SUBROUTINE XRTSORT_GETQLIM(HEAD, SRT, STATUS)
*    Description :
*     Reads the file containing the quality limits imposed by the preprocessing
*     system at GSOC. Displays these limits to the user and asks if
*     these are adequate. If not, the user can select quality ranges to use
*     in the subsequent sorting.
*    Environment parameters :
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton
*    History :
*     25-Apr-1990        original  (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD                    ! Header from files
*    Import-Export :
      RECORD /XRT_SCFDEF/ SRT                   ! Sort control structure
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC) QLOC               ! Locator to quality file
      LOGICAL QCHANGE                           ! Change quality limits ?
      CHARACTER*80 QNAME                        ! Name of quality file
      CHARACTER*20 CPROP                        ! Properties to change
      INTEGER TEMP_MIN,TEMP_MAX                 ! Min and max temperature quals
      INTEGER GAIN_MIN,GAIN_MAX                 ! Min and max gain qualities
*    Local data :
*     <any DATA initialisations for local variables>
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Set quality logicals false
      SRT.QUAL_LESS=.FALSE.
      SRT.QUAL_MORE=.FALSE.
*
* See if user wants to change quality limits at all
      CALL USI_GET0L('QCHANGE', QCHANGE, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Is a quality change wanted ?
      IF (.NOT. QCHANGE) GOTO 999
*
* Get the quality limits used in the pre-processing phase at GSOC.
*    Create the filename for the quality limits file
      QNAME = SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//'_000.QLIM'
*
*    Open the file
      CALL HDS_OPEN(QNAME, 'READ', QLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error opening quality limits file')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF
*
*    Read the various quality limits from the file
      CALL CMP_GET0I(QLOC, 'TEMP_MIN', TEMP_MIN, STATUS)
      CALL CMP_GET0I(QLOC, 'TEMP_MAX', TEMP_MAX, STATUS)
      CALL CMP_GET0I(QLOC, 'GAIN_MIN', GAIN_MIN, STATUS)
      CALL CMP_GET0I(QLOC, 'GAIN_MAX', GAIN_MAX, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error reading quality limits from file')
         STATUS=SAI__ERROR
         GOTO 999
      ENDIF
*
*    Close the file
      CALL HDS_CLOSE(QLOC, STATUS)
      CALL DAT_ANNUL(QLOC, STATUS)
*
*    Show the user the current ranges
      CALL MSG_PRNT('    Quality ranges')
      CALL MSG_PRNT('   ****************')
      CALL MSG_SETI('QTEMPMIN', TEMP_MIN)
      CALL MSG_SETI('QTEMPMAX', TEMP_MAX)
      CALL MSG_PRNT(' 1. Temperature: ^QTEMPMIN : ^QTEMPMAX')
      CALL MSG_SETI('QGAINMIN', GAIN_MIN)
      CALL MSG_SETI('QGAINMAX', GAIN_MAX)
      CALL MSG_PRNT(' 2. Gain       : ^QGAINMIN : ^QGAINMAX')
*
*    Ask the user which ranges to change
      CALL USI_GET0C('PROPERTY', CPROP, STATUS)
*
*    Check for a par_null and jump to the end if received
      IF (STATUS .EQ. PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
         GOTO 999
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Get user ranges
      IF (INDEX (CPROP, '1') .NE. 0) THEN
*
         CALL USI_DEF0I('TEMPMIN', TEMP_MIN, STATUS)
         CALL USI_DEF0I('TEMPMAX', TEMP_MAX, STATUS)
         CALL USI_GET0I('TEMPMIN', SRT.TEMP_MIN, STATUS)
         CALL USI_GET0I('TEMPMAX', SRT.TEMP_MAX, STATUS)
*
*      Check if acceptable temperature range has been made more stringent
         IF (SRT.TEMP_MIN .GT. TEMP_MIN .OR.
     &                   SRT.TEMP_MAX .LT. TEMP_MAX) THEN
            SRT.QUAL_MORE=.TRUE.
         ENDIF
*
*      Check if acceptable temperature range has been made less stringent
         IF (SRT.TEMP_MIN .LT. TEMP_MIN .OR.
     &                   SRT.TEMP_MAX .GT. TEMP_MAX) THEN
            SRT.QUAL_LESS=.TRUE.
         ENDIF
*
      ELSE
*
*      Set the sort range to the pre-processing range
         SRT.TEMP_MIN=TEMP_MIN
         SRT.TEMP_MAX=TEMP_MAX
*
      ENDIF
*
      IF (INDEX (CPROP, '2') .NE. 0) THEN
*
         CALL USI_DEF0I('GAINMIN', GAIN_MIN, STATUS)
         CALL USI_DEF0I('GAINMAX', GAIN_MAX, STATUS)
         CALL USI_GET0I('GAINMIN', SRT.GAIN_MIN, STATUS)
         CALL USI_GET0I('GAINMAX', SRT.GAIN_MAX, STATUS)
*
*      Check if acceptable GAIN range has been made more stringent
         IF (SRT.GAIN_MIN .GT. GAIN_MIN .OR.
     &                   SRT.GAIN_MAX .LT. GAIN_MAX) THEN
            SRT.QUAL_MORE=.TRUE.
         ENDIF
*
*      Check if acceptable GAIN range has been made less stringent
         IF (SRT.GAIN_MIN .LT. GAIN_MIN .OR.
     &                   SRT.GAIN_MAX .GT. GAIN_MAX) THEN
            SRT.QUAL_LESS=.TRUE.
         ENDIF
*
      ELSE
*
*      Set the sort range to the pre-processing range
         SRT.GAIN_MIN=GAIN_MIN
         SRT.GAIN_MAX=GAIN_MAX
*
      ENDIF
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','From XRTSORT_GETQLIM',STATUS)
      ENDIF
*
      END


*+XRTSORT_CRE_MASK - Create spatial mask
      SUBROUTINE XRTSORT_CRE_MASK(HEAD,SRT,RESFACT,MDIM,MRES,MPTR,
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
      INCLUDE 'DAT_PAR'
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_SCFDEF/ SRT
      RECORD /XRT_HEAD/ HEAD
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
        IF (HEAD.ORIGIN.EQ.'MPE') THEN
          MDIM(1)=INT(ABS(HEAD.XEND-HEAD.XSTART)/MRES)
          MDIM(2)=INT(ABS(HEAD.YEND-HEAD.YSTART)/MRES)
        ELSE
          MDIM(1)=HEAD.XEND/MRES
          MDIM(2)=HEAD.YEND/MRES
        ENDIF
        SCALE(1)=-HEAD.PIXEL*MRES/3600.0
        SCALE(2)=HEAD.PIXEL*MRES/3600.0
        BASE(1)=-MDIM(1)/2*SCALE(1)+SCALE(1)/2.0
        BASE(2)=-MDIM(2)/2*SCALE(2)+SCALE(2)/2.0
        UNITS(1)='degrees'
        UNITS(2)='degrees'

        CALL DYN_MAPI(2,MDIM,MPTR,STATUS)
        CALL ARX_MASK(SRT.ARDID,MDIM,BASE,SCALE,UNITS,%val(MPTR),STATUS)

        IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ',' from XRTSORT_CRE_MASK',STATUS)
        ENDIF

      ENDIF

      END


*+XRTSORT_SCAN_MASK - Create spatial mask
      SUBROUTINE XRTSORT_SCAN_MASK(HEAD,MDIM1,MDIM2,MASK,MRES,XC,YC,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'INC_XRTHEAD'
*    Structure definitions :
*    Import :
      RECORD /XRT_HEAD/ HEAD
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

	print*,mdim1,mdim2

      SCALE(1)=-HEAD.PIXEL*MRES/3600.0
      SCALE(2)=HEAD.PIXEL*MRES/3600.0
      BASE(1)=-MDIM1/2*SCALE(1)+SCALE(1)/2.0
      BASE(2)=-MDIM2/2*SCALE(2)+SCALE(2)/2.0

	print*,base(1),scale(1),base(2),scale(2)

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

	print*,iXmin,iXmax,IYmin,IYmax

      XC=XTOT/REAL(NPIX)
      YC=YTOT/REAL(NPIX)
      HX=0.5*SCALE(1)
      HY=0.5*SCALE(2)
      OFFSET=REAL(IXMIN-1)*SCALE(1)
      XMIN=BASE(1) +OFFSET -HX
      OFFSET=REAL(IXMAX-1)*SCALE(1)
      XMAX=BASE(1)+ OFFSET +HX
      OFFSET=REAL(IYMIN-1)*SCALE(2)
      YMIN=BASE(2)+ OFFSET -HY
      OFFSET=REAL(IYMAX-1)*SCALE(2)
      XMAX=BASE(2)+ OFFSET +HY

	print*,xmin,xmax,ymin,ymax

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ',' from XRTSORT_SCAN_MASK',STATUS)
      ENDIF
*
      END



*+XRTSORT_PHOTONCNT   - Sorts XRT raw data into a binned data array
      SUBROUTINE XRTSORT_PHOTONCNT(HEAD, SRT, BSRT, MAXLIM, STATUS)
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
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD                      ! Header values
*
      RECORD /XRT_SCFDEF/ SRT                     ! Source sorting parameters
      RECORD /XRT_SCFDEF/ BSRT                    ! Bckgnd sorting parameters
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
      DO MAP=1,HEAD.ISMTNU
*
         END_XMAP = HEAD.XSMAP(MAP) + HEAD.IFDSZX/REAL(HEAD.ISMNUX)
         END_YMAP = HEAD.YSMAP(MAP) + HEAD.IFDSZY/REAL(HEAD.ISMNUY)
*
*     Check if the map is within either sort box
         IF ( (SRT.MIN_X .LE. END_XMAP .AND. SRT.MAX_X .GE.
     &        HEAD.XSMAP(MAP) .AND. SRT.MIN_Y .LE. END_YMAP .AND.
     &        SRT.MAX_Y .GE. HEAD.YSMAP(MAP)) .OR. (SRT.BCKGND
     &          .AND. BSRT.MIN_X .LE. END_XMAP .AND. BSRT.MAX_X .GE.
     &        HEAD.XSMAP(MAP) .AND. BSRT.MIN_Y .LE. END_YMAP .AND.
     &        BSRT.MAX_Y .GE. HEAD.YSMAP(MAP)) ) THEN
*
*     Add the number of photons in this map to the total
              IF (MAP .EQ. HEAD.ISMTNU) THEN
                 NINMAP = HEAD.IEVTNU - (HEAD.EVSTART(HEAD.ISMTNU)-1)
              ELSE
                 NINMAP = HEAD.EVSTART(MAP+1)-HEAD.EVSTART(MAP)
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
      SUBROUTINE XRTSORT_RANGESELECT(HEAD,SRT,BSRT,SDIM,BDIM,
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
      INCLUDE 'INC_XRTSRT'                      ! Sorting struc. incl. MXTIME
      INCLUDE 'INC_XRTHEAD'
* Import :
      RECORD /XRT_HEAD/ HEAD
      RECORD /XRT_SCFDEF/ SRT, BSRT
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
        REAL VC(3),VS(3)              ! Used in transforming input pixels
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
        REAL BXWIDW,BYWIDW,BRADW      ! Other parameters in world coords.
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
        INTEGER J,LP
        INTEGER ENBIN                 ! Bin width for energy axis
        INTEGER PHBIN                 ! Bin width for PHA channel axis
        INTEGER MFACT                 ! Y orientation factor (-1 or +1)
        INTEGER RES		      ! Resolution factor
        LOGICAL ANNULAR		      ! Background is annular
        LOGICAL JUMPOUT
* Function declarations :
        INTEGER CHR_LEN
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
      CALL ARX_OPEN('WRITE',SRT.ARDID,STATUS)
      IF (SRT.BCKGND) THEN
        CALL ARX_OPEN('WRITE',BSRT.ARDID,STATUS)
      ENDIF

* Set a factor for dealing with different orientations of y-axes in raw data
      IF (HEAD.ORIGIN.EQ.'MPE') THEN
         MFACT = -1
      ELSE
         MFACT = 1
      ENDIF
*
* Calculate pixel size in degrees
      PTOD = HEAD.PIXEL/3600.0
*
* Store pixel conversion factor
      SRT.PTOD = PTOD
      BSRT.PTOD = PTOD
*
* Calculate conversion factor from pixels to radians
      PTOR=PTOD*DTOR
*
* Generate the attitude matrix of the spacecraft
      RARAD =  HEAD.AXIS_RA * DTOR
      DECRAD = HEAD.AXIS_DEC * DTOR
*
*   Convert the roll angle to radians.
      DROLL = (HEAD.ROLLCI) * DTOR
*
      CALL CONV_GENDMAT(RARAD, DECRAD, DROLL, TMAT)
*
*
* Ask which properties will be used as binning axes in the output file.
      IF (SRT.DTYPE.EQ.'BinDS')THEN
*
        SRT.NAXES=0
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
      CALL USI_GET0C('SHAPE', SRT.SHAPE, STATUS)
      CALL CHR_UCASE(SRT.SHAPE)
      IF (INDEX('RCAEI', SRT.SHAPE) .EQ. 0) THEN
        CALL MSG_PRNT('AST_ERR: unknown shape')
        STATUS=SAI__ERROR
        GOTO 999
      ENDIF

*  ARD description
      IF (SRT.SHAPE.EQ.'I') THEN

*  get ARD file
        CALL ARX_READ('ARD',SRT.ARDID,STATUS)

*  Get resolution factor
        CALL USI_GET0I('RES',RES,STATUS)
        IF (RES.LT.1.OR.RES.GT.3) THEN
          CALL MSG_PRNT('** Invalid resolution factor - using 2')
          RES=2
        ENDIF

*  Create spatial mask
        CALL MSG_PRNT('Creating spatial mask...')
        CALL XRTSORT_CRE_MASK(HEAD,SRT,RES,MDIM,MRES,SMPTR,STATUS)
        CALL MSG_PRNT('Done!')

        CALL XRTSORT_SCAN_MASK(HEAD,MDIM(1),MDIM(2),%val(SMPTR),MRES,
     :                                  XW,YW,XW1,XW2,YW1,YW2,STATUS)

	print*,xw,yw,xw1,xw2,yw1,yw2

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
     :             SRT.FIELD_RA,SRT.FIELD_DEC,STATUS)
        ELSEIF (CENTRE.EQ.'2') THEN
          CENTRE='MEDIAN'
          XW=(XW1+XW2)/2.0
          YW=(YW1+YW2)/2.0
          CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             SRT.FIELD_RA,SRT.FIELD_DEC,STATUS)
        ELSEIF (CENTRE.EQ.'3') THEN
          CENTRE='AXIS'
          SRT.FIELD_RA=HEAD.AXIS_RA
          SRT.FIELD_DEC=HEAD.AXIS_DEC
          CALL XRTSORT_RADEC2AXIS(SRT.FIELD_RA,SRT.FIELD_DEC,TMAT,
     :                                                XW,YW,STATUS)
        ELSEIF (CENTRE.EQ.'4') THEN
          CENTRE='USER'
          CALL USI_GET0D('RA', SRT.FIELD_RA, STATUS)
          CALL USI_GET0D('DEC', SRT.FIELD_DEC, STATUS)
          CALL XRTSORT_RADEC2AXIS(SRT.FIELD_RA,SRT.FIELD_DEC,TMAT,
     :                                                XW,YW,STATUS)
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
        CALL USI_DEF0D('RA', HEAD.AXIS_RA, STATUS)
        CALL USI_DEF0D('DEC', HEAD.AXIS_DEC, STATUS)

        CALL USI_GET0D('RA', SRT.FIELD_RA, STATUS)
        CALL USI_GET0D('DEC', SRT.FIELD_DEC, STATUS)

        CALL XRTSORT_RADEC2AXIS(SRT.FIELD_RA,SRT.FIELD_DEC,TMAT,
     :                                              XW,YW,STATUS)

      ENDIF

      IF (STATUS .NE. SAI__OK) GOTO 999

*  Calculate the source position in raw pixels

      SOURCE_X = NINT( -XW / PTOD)
      SOURCE_Y = NINT( YW / PTOD) * MFACT

*  Add the sky pixel centre to the source centre
      SOURCE_X = SOURCE_X + HEAD.SKYCX
      SOURCE_Y = SOURCE_Y + HEAD.SKYCY

*  Find the closest distance to each border
      CBORDX=MIN(ABS(SOURCE_X-HEAD.XSTART),ABS(SOURCE_X-HEAD.XEND))
     &                                   * PTOD * 2.0
      CBORDY=MIN(ABS(SOURCE_Y-HEAD.YSTART),ABS(SOURCE_Y-HEAD.YEND))
     &                                   * PTOD * 2.0


*  Rectangle
      IF (SRT.SHAPE .EQ. 'R') THEN

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
         SRT.PHI=0.0
         SRT.ELAMIN=0.0
         SRT.ELBMIN=0.0
         SRT.ELAMAX=X_HWIDTH
         SRT.ELBMAX=Y_HWIDTH

*  store ARD description
         CALL ARX_BOX(SRT.ARDID,0,'ADD',.FALSE.,XW,YW,XWIDW,YWIDW,
     :                                                     STATUS)
*
*   Circular area
      ELSEIF (SRT.SHAPE .EQ. 'C') THEN
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
	 SRT.ELAMIN = 0.0
	 SRT.ELAMAX = REAL(NINT(SRAD2 / PTOD - 0.5))
         SRT.ELBMIN = 0.0
         SRT.ELBMAX = SRT.ELAMAX
         SRT.PHI = 0.0
*
	 X_HWIDTH = SRT.ELAMAX
	 Y_HWIDTH = SRT.ELAMAX
*
*   back to world coords
         RADW=SRT.ELAMAX*PTOD

*   store ARD description
         CALL ARX_CIRCLE(SRT.ARDID,0,'ADD',.FALSE.,XW,YW,RADW,STATUS)

*   Get box inner and outer radii for an annular box
      ELSEIF (SRT.SHAPE .EQ. 'A') THEN
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
         SRT.ELAMIN = REAL(NINT(SRAD1 / PTOD))
         SRT.ELAMAX = REAL(NINT(SRAD2 / PTOD))
         SRT.ELBMIN = SRT.ELAMIN
         SRT.ELBMAX = SRT.ELAMAX
         SRT.PHI=0.0
*
*   and back to world coords
         IRADW=SRT.ELAMIN*PTOD
         ORADW=SRT.ELAMAX*PTOD

*   store ARD description
         CALL ARX_ANNULUS(SRT.ARDID,0,'ADD',.FALSE.,XW,YW,
     :                                  IRADW,ORADW,STATUS)

	 X_HWIDTH = SRT.ELAMAX
	 Y_HWIDTH = SRT.ELAMAX
*
*   Get ellipse parameters for an elliptical box
      ELSEIF (SRT.SHAPE .EQ. 'E') THEN
*
*      Get orientation. +ve is anticlockwise from east
	 CALL USI_GET0R('ANGLE', ANGLE, STATUS)
*
*      Convert orientation angle to radians
         SRT.PHI = ANGLE * DTOR
*
*      Find cos and sine of orientation
         SRT.COSPHI = COS(SRT.PHI)
         SRT.SINPHI = SIN(SRT.PHI)
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
            EXMAX=SQRT((ELAMAX*SRT.COSPHI)**2 + (ELBMAX*SRT.SINPHI)**2)
            EYMAX=SQRT((ELAMAX*SRT.SINPHI)**2 + (ELBMAX*SRT.COSPHI)**2)
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
         SRT.ELAMIN = REAL(NINT(ELAMIN / PTOD))
         SRT.ELAMAX = REAL(NINT(ELAMAX / PTOD))
         SRT.ELBMAX = REAL(NINT(ELBMAX / PTOD))
*
*      Calculate the inner Y axis
         SRT.ELBMIN = SRT.ELAMIN * SRT.ELBMAX / SRT.ELAMAX

*   convert back to world coords and store ARD description
         XWIDW=SRT.ELAMAX*PTOD
         YWIDW=SRT.ELBMAX*PTOD
         CALL ARX_ELLIPSE(SRT.ARDID,0,'ADD',.FALSE.,XW,YW,
     :                             XWIDW,YWIDW,ANGLE,STATUS)
         IF ((SRT.ELAMIN).GT.0.0.AND.(SRT.ELBMIN).GT.0.0) THEN
           XWIDW=SRT.ELAMIN*PTOD
           YWIDW=SRT.ELBMIN*PTOD
           CALL ARX_ELLIPSE(SRT.ARDID,0,'AND',.TRUE.,XW,YW,
     :                               XWIDW,YWIDW,ANGLE,STATUS)
         ENDIF
*
*      Calculate maximum X and Y half widths for this ellipse
	 X_HWIDTH = SQRT( (SRT.ELAMAX * SRT.COSPHI) ** 2 +
     &                           (SRT.ELBMAX * SRT.SINPHI) ** 2 )
	 Y_HWIDTH = SQRT( (SRT.ELAMAX * SRT.SINPHI) ** 2 +
     &                           (SRT.ELBMAX * SRT.COSPHI) ** 2 )
*
      ENDIF
*
*   Put in a modification which restricts the PH range of the corrected
*   amplitude data to be >=1 and <=256, if a spectrum is being produced.
      IF ( INDEX(BIN_AXES,'7') .NE. 0) THEN

        IF ( HEAD.CEND .GT. 256 ) THEN
          CALL MSG_PRNT(' ')
          CALL MSG_SETI('CMAX',HEAD.CEND)
          CALL MSG_PRNT('** WARNING: This observation contains data '/
     &      /'up to PHA=^CMAX. ONLY the first 256 '/
     &      /'channels can be analysed in a spectrum **')
          CALL MSG_PRNT(' ')
          HEAD.CEND = MIN(HEAD.CEND, 256)
        ENDIF
        IF (HEAD.CSTART .LE. 0) THEN
          CALL MSG_PRNT(' ')
          CALL MSG_SETI('CMIN',HEAD.CSTART)
          CALL MSG_PRNT('** WARNING: Illegal lower channel number - '/
     &       /'^CMIN - substituting 1')
        ENDIF
      ENDIF
*
* Display data ranges
      WRITE (*,*) '   Data array axes'
      WRITE (*,*) ' *******************'
      WRITE (*,1010)3,' XDET:',HEAD.XDSTART,HEAD.XDEND,' (pixels)'
      WRITE (*,1010)4,' YDET:',HEAD.YDSTART,HEAD.YDEND,' (pixels)'
      WRITE (*,1000)5,' Time:',HEAD.TSTART(1),HEAD.TEND(HEAD.NTRANGE),
     &                                           ' (seconds)'
      WRITE (*,1010)6,' PHA channel:',HEAD.ASTART,HEAD.AEND,' (chn.)'
      WRITE (*,1010)7,' Corr. PHA chan.',HEAD.CSTART,HEAD.CEND,' (chn.)'
*
1000  FORMAT(5X,I1,')',A,F12.3,' to ',F12.3,A)
1010  FORMAT(5X,I1,')',A,I7,' to ',I7,A)
*
      CALL USI_DEF0I('XDSTART', HEAD.XDSTART, STATUS)
      CALL USI_DEF0I('XDEND', HEAD.XDEND, STATUS)
      CALL USI_DEF0I('YDSTART', HEAD.YDSTART, STATUS)
      CALL USI_DEF0I('YDEND', HEAD.YDEND, STATUS)
*
      CALL CHR_RTOC(HEAD.TSTART(1), C1, K1)
      CALL CHR_RTOC(HEAD.TEND(HEAD.NTRANGE), C2, K2)
      TIMSTRING = C1(1:K1) // ':' // C2(1:K2)
*
      CALL USI_DEF0C('TIMRANGE', TIMSTRING, STATUS)
      CALL USI_DEF0I('MINPH', HEAD.ASTART, STATUS)
      CALL USI_DEF0I('MAXPH', HEAD.AEND, STATUS)
      CALL USI_DEF0I('MINEN', HEAD.CSTART, STATUS)
      CALL USI_DEF0I('MAXEN', HEAD.CEND, STATUS)
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
         CALL USI_GET0I('XDSTART',SRT.MIN_XD,STATUS)
         CALL USI_GET0I('XDEND', SRT.MAX_XD, STATUS)
      ELSE
         SRT.MIN_XD = HEAD.XDSTART
         SRT.MAX_XD = HEAD.XDEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Detector Y
      IF (INDEX(RANGES,'4') .NE. 0) THEN
         CALL USI_GET0I('YDSTART',SRT.MIN_YD,STATUS)
         CALL USI_GET0I('YDEND', SRT.MAX_YD, STATUS)
      ELSE
         SRT.MIN_YD = HEAD.YDSTART
         SRT.MAX_YD = HEAD.YDEND
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
         CALL UTIL_TDECODE(TIMSTRING, HEAD.BASE_MJD, MXTIME,
     &                      SRT.NTIME, SRT.MIN_T, SRT.MAX_T, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*
      ELSE
         SRT.NTIME=1
         SRT.MIN_T(1) = HEAD.TSTART(1)
         SRT.MAX_T(1) = HEAD.TEND(HEAD.NTRANGE)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   PH:
      IF (INDEX(RANGES,'6') .NE. 0) THEN
         CALL USI_GET0I('MINPH', SRT.MIN_PH, STATUS)
	 CALL USI_GET0I('MAXPH', SRT.MAX_PH, STATUS)
      ELSE
         SRT.MIN_PH=HEAD.ASTART
         SRT.MAX_PH=HEAD.AEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*
*  Energy:
      IF (INDEX(RANGES,'7') .NE. 0) THEN
         CALL USI_GET0I('MINEN', SRT.MIN_EN, STATUS)
         CALL USI_GET0I('MAXEN', SRT.MAX_EN, STATUS)
      ELSE
         SRT.MIN_EN=HEAD.CSTART
         SRT.MAX_EN=HEAD.CEND
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Display the default quality limits and get new ones if these aren't ok
      CALL XRTSORT_GETQLIM(HEAD, SRT, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Select bin widths for axes properties
* Calculate X and Y extremes
*
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
         SRT.MIN_X = SOURCE_X-INT(REAL(NREBINX)*REAL(NXBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT.MAX_X =SOURCE_X+NINT(REAL(NREBINX)*REAL(NXBIN)/2.0 +
     &                                0.4) - 1
*
* Make sure you haven't gone over the edge
         SRT.MIN_X = MAX(SRT.MIN_X, HEAD.XSTART)
         SRT.MAX_X = MIN(SRT.MAX_X, HEAD.XEND)
*
         CALL MSG_SETI('NXBIN',NXBIN)
         CALL MSG_SETI('NREB',NREBINX)
         CALL MSG_SETI('SPIX',SRT.MIN_X)
         CALL MSG_PRNT('There will be ^NXBIN X bins, each of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(1)=NXBIN
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=1
*
      ELSE
*
         SDIM(1)=1
         NREBINX=1
         SRT.MIN_X = MAX(SOURCE_X-X_HWIDTH, HEAD.XSTART)
         SRT.MAX_X = MIN(SOURCE_X+X_HWIDTH, HEAD.XEND)
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
         SRT.MIN_Y=SOURCE_Y-INT(REAL(NREBINY)*REAL(NYBIN)/2.0)
         SRT.MAX_Y=SOURCE_Y+NINT(REAL(NREBINY)*REAL(NYBIN)/2.0 +
     &                                        0.4) - 1
*
* Make sure you haven't gone over the edge
         SRT.MIN_Y = MAX(SRT.MIN_Y, HEAD.YSTART)
         SRT.MAX_Y = MIN(SRT.MAX_Y, HEAD.YEND)

         CALL MSG_SETI('NYBIN',NYBIN)
         CALL MSG_SETI('NREB',NREBINY)
         CALL MSG_SETI('SPIX',SRT.MIN_Y)
         CALL MSG_PRNT('There will be ^NYBIN Y bins, each of '//
     &                         '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(2)=NYBIN
*
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=2
*
      ELSE
*
         SDIM(2)=1
         NREBINY=1
         SRT.MIN_Y = MAX(SOURCE_Y-Y_HWIDTH, HEAD.YSTART)
         SRT.MAX_Y = MIN(SOURCE_Y+Y_HWIDTH, HEAD.YEND)
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
C         RBIN = (SRT.RADIN - SRT.RADOUT) / REAL(NRBIN)
*
C         CALL MSG_SETI('NRBIN',NRBIN)
C         CALL MSG_SETI('RBIN',RBIN)
C         CALL MSG_SETI('SPIX',SRT.RADIN)
C         CALL MSG_PRNT('There will be ^NRBIN radial bins, each of '//
C     &                         '^RBIN degrees starting from ^SPIX')
*
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=8
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
         CALL MSG_SETI('XDET',SRT.MAX_XD-SRT.MIN_XD+1)
         CALL MSG_OUT(' ',' There are ^XDET raw pixels within'//
     &                      ' the X detector range selected',STATUS)
*
         CALL USI_GET0I('NXDBIN', NXDBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBXD=NINT((SRT.MAX_XD - SRT.MIN_XD + 1) / REAL(NXDBIN))
*
         SRT.MIN_XD = (SRT.MAX_XD + SRT.MIN_XD)/2.0 -
     &                    INT(REAL(NREBXD)*REAL(NXDBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT.MAX_XD = (SRT.MAX_XD + SRT.MIN_XD)/2.0 +
     &                  NINT(REAL(NREBXD)*REAL(NXDBIN)/2.0 + 0.4) - 1
         CALL MSG_SETI('NXDBIN',NXDBIN)
         CALL MSG_SETI('NREB',NREBXD)
         CALL MSG_SETI('SPIX',SRT.MIN_XD)
         CALL MSG_PRNT('There will be ^NXDBIN X detector bins, of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(3)=NXDBIN
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=3
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
         CALL MSG_SETI('YDET',SRT.MAX_YD-SRT.MIN_YD+1)
         CALL MSG_OUT(' ',' There are ^YDET raw pixels within'//
     &                      ' the Y detector range selected',STATUS)
*
         CALL USI_GET0I('NYDBIN', NYDBIN, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate number of raw pixels per new pixel
         NREBYD=NINT((SRT.MAX_YD - SRT.MIN_YD + 1) / REAL(NYDBIN))
*
         SRT.MIN_YD = (SRT.MAX_YD + SRT.MIN_YD)/2.0 -
     &                    INT(REAL(NREBYD)*REAL(NYDBIN)/2.0 )
*
* Make the maximum 1 higher if odd no. of input pixels
         SRT.MAX_YD = (SRT.MAX_YD + SRT.MIN_YD)/2.0 +
     &                  NINT(REAL(NREBYD)*REAL(NYDBIN)/2.0 + 0.4) - 1
         CALL MSG_SETI('NYDBIN',NYDBIN)
         CALL MSG_SETI('NREB',NREBYD)
         CALL MSG_SETI('SPIX',SRT.MIN_YD)
         CALL MSG_PRNT('There will be ^NYDBIN Y detector bins, of '//
     &                     '^NREB raw pixels, starting from ^SPIX')
*
         SDIM(4)=NYDBIN
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=4
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
         SDIM(5)=INT((SRT.MAX_T(SRT.NTIME)-SRT.MIN_T(1))/TIMBIN)
*
* Need to restrict the allowed time ranges to the new max. value.
         SMAXT=SRT.MIN_T(1) + INT(REAL(SDIM(5))*TIMBIN)
*
*   Check which srt range is greater than this
         DO LP=1,SRT.NTIME
*
            IF (SRT.MAX_T(LP) .GT. SMAXT) THEN
               IF (SRT.MIN_T(LP) .LE. SMAXT) THEN
                  SRT.MAX_T(LP) = SMAXT
                  SRT.NTIME = LP
                  GOTO 100
               ELSE
                  IF (LP.GT.1) SRT.NTIME = LP-1
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO
*
100      CONTINUE
*
         CALL MSG_SETI('NTBIN',SDIM(5))
         CALL MSG_SETR('TIMBIN',TIMBIN)
         CALL MSG_SETR('STIM',REAL(SRT.MIN_T(1)))
         CALL MSG_PRNT('There will be ^NTBIN Time bins, each '//
     &                       'of ^TIMBIN seconds, starting from ^STIM')
*
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=5
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
         PHBIN = MIN( (SRT.MAX_PH-SRT.MIN_PH+1), PHBIN )
*
*   Calculate number of PH bins in output file and recalculate maximum
         SDIM(6)=INT((SRT.MAX_PH-SRT.MIN_PH+1)/PHBIN)
         SRT.MAX_PH=PHBIN*SDIM(6)+SRT.MIN_PH-1
*
         CALL MSG_SETI('NPBIN',SDIM(6))
         CALL MSG_SETI('PHBIN',PHBIN)
         CALL MSG_SETI('SPH',SRT.MIN_PH)
         CALL MSG_PRNT('There will be ^NPBIN PH bins, each '//
     &          'of ^PHBIN raw channels, starting from ^SPH')
*
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=6
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
         ENBIN = MIN( (SRT.MAX_EN-SRT.MIN_EN+1), ENBIN )
*
*   Calculate number of PH bins in output file and recalculate maximum
         SDIM(7)=INT((SRT.MAX_EN-SRT.MIN_EN+1)/ENBIN)
         SRT.MAX_EN=ENBIN*SDIM(7)+SRT.MIN_EN-1
*
         CALL MSG_SETI('NEBIN',SDIM(7))
         CALL MSG_SETI('ENBIN',ENBIN)
         CALL MSG_SETI('SEN',SRT.MIN_EN)
         CALL MSG_PRNT('There will be ^NEBIN energy bins, each '//
     &          'of ^ENBIN raw channels, starting from ^SEN')
*
         SRT.NAXES=SRT.NAXES+1
         SRT.BINAXIS(SRT.NAXES)=7
*
      ELSE
         SDIM(7)=1
      ENDIF
*
* Set number of azimuthal bins to 1 for now
      NAZBIN=1
*
* Calculate the centre X and Y value in degrees
      SRT.XCENT = - ((SRT.MIN_X + SRT.MAX_X) / 2.0 - HEAD.SKYCX) * PTOD
      SRT.YCENT = MFACT * ((SRT.MIN_Y + SRT.MAX_Y) / 2.0 - HEAD.SKYCY)
     &                                      * PTOD
*
* The outer X,Y values may have changed due to rebinning. If so
* recalculate them.
      IF (SDIM(1) .GT. 1 .OR. SDIM(2) .GT. 1 ) THEN
         IF (SRT.SHAPE .NE. 'E') THEN
            SRT.ELAMAX = ABS(SRT.MAX_X - (SRT.MIN_X + SRT.MAX_X)/2.0)
            SRT.ELBMAX = ABS(SRT.MAX_Y - (SRT.MIN_Y + SRT.MAX_Y)/2.0)
         ELSE
C????            SRT.ELAMAX = SRT.ELAMAX * SRT.MAX_X / X_HWIDTH
C????            SRT.ELBMAX = SRT.ELBMAX * SRT.MAX_X / X_HWIDTH
         ENDIF
      ENDIF
*
* Calculate cos and sine of the orientation
      SRT.COSPHI = COS(SRT.PHI)
      SRT.SINPHI = SIN(SRT.PHI)

* Calculate tot. no of elements
      SRT.NDATA=SDIM(1)*SDIM(2)*SDIM(3)*SDIM(4)*SDIM(5)*SDIM(6)*SDIM(7)
     &                      *NRBIN*NAZBIN
*
      SRT.IMAGE=(SRT.DTYPE.EQ.'BinDS'.AND.
     &              SDIM(1).GT.1.AND.SDIM(2).GT.1.AND.NRBIN.EQ.1)

* Is a background file wanted ? Not possible if source file contains
* radial bins.
      IF ( INDEX(BIN_AXES,'8') .EQ. 0) THEN
         CALL USI_GET0L('BACK', SRT.BCKGND, STATUS)
      ELSE
         SRT.BCKGND = .FALSE.
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999





* Get RA and DEC of background box in degrees.
      IF (SRT.BCKGND) THEN


*  ARD description
         IF (BSRT.SHAPE.EQ.'I') THEN

*  get ARD file
            CALL ARX_READ('BARD',BSRT.ARDID,STATUS)


*  Create spatial mask
            CALL MSG_PRNT('Creating background spatial mask...')
            CALL XRTSORT_CRE_MASK(HEAD,BSRT,RES,MDIM,MRES,BMPTR,STATUS)
            CALL MSG_PRNT('Done!')

            CALL XRTSORT_SCAN_MASK(HEAD,MDIM(1),MDIM(2),%val(BMPTR),
     :                           MRES,BXW,BYW,XW1,XW2,YW1,YW2,STATUS)

            IF (CENTRE.EQ.'MEAN') THEN
               CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             BSRT.FIELD_RA,BSRT.FIELD_DEC,STATUS)
            ELSEIF (CENTRE.EQ.'MEDIAN') THEN
               BXW=(XW1+XW2)/2.0
               BYW=(YW1+YW2)/2.0
               CALL XRTSORT_AXIS2RADEC(XW,YW,TMAT,
     :             BSRT.FIELD_RA,BSRT.FIELD_DEC,STATUS)
            ELSEIF (CENTRE.EQ.'AXIS') THEN
                BSRT.FIELD_RA=HEAD.AXIS_RA
                BSRT.FIELD_DEC=HEAD.AXIS_DEC
                CALL XRTSORT_RADEC2AXIS(BSRT.FIELD_RA,BSRT.FIELD_DEC,
     :                                             TMAT,XW,YW,STATUS)
            ELSEIF (CENTRE.EQ.'USER') THEN
                CALL USI_GET0D('RAB', BSRT.FIELD_RA, STATUS)
                CALL USI_GET0D('DECB', BSRT.FIELD_DEC, STATUS)
                CALL XRTSORT_RADEC2AXIS(BSRT.FIELD_RA,BSRT.FIELD_DEC,
     :                                              TMAT,XW,YW,STATUS)
            ENDIF

            BX_HWIDTH=NPIX(ABS(XW2-XW1))
            BY_HWIDTH=NPIX(ABS(YW2-YW1))


        ELSE

*   Set centre of the field as default
           CALL USI_DEF0D('RAB', HEAD.AXIS_RA, STATUS)
           CALL USI_DEF0D('DECB', HEAD.AXIS_DEC, STATUS)

           CALL USI_GET0D('RAB', BSRT.FIELD_RA, STATUS)
           CALL USI_GET0D('DECB',BSRT.FIELD_DEC, STATUS)

           CALL XRTSORT_RADEC2AXIS(BSRT.FIELD_RA,BSRT.FIELD_DEC,TMAT,
     :                                                   XW,YW,STATUS)

        ENDIF

        IF (STATUS .NE. SAI__OK) GOTO 999

*  Calculate the source position in raw pixels

        BCKGND_X = NINT( -XW / PTOD)
        BCKGND_Y = NINT( YW / PTOD) * MFACT

*  Add the sky pixel centre to the source centre
        BCKGND_X = BCKGND_X + HEAD.SKYCX
        BCKGND_Y = BCKGND_Y + HEAD.SKYCY

*  Find the closest distance to each border
        BBORDX=MIN(ABS(BCKGND_X-HEAD.XSTART),ABS(BCKGND_X-HEAD.XEND))
     &                                                   * PTOD * 2.0
        BBORDY=MIN(ABS(BCKGND_Y-HEAD.YSTART),ABS(BCKGND_Y-HEAD.YEND))
     &                                                   * PTOD * 2.0





	 IF (SRT.SHAPE .EQ. 'R') THEN
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
            IF (ABS(BSRT.FIELD_RA-SRT.FIELD_RA) .LT. 0.001 .AND.
     &          ABS(BSRT.FIELD_DEC-SRT.FIELD_DEC) .LT. 0.001) THEN
*
               CALL ARX_BOX(BSRT.ARDID,0,'ADD',.FALSE.,BXW,BYW,
     :                                     BXWIDW,BYWIDW,STATUS)
               CALL ARX_BOX(BSRT.ARDID,0,'AND',.TRUE.,XW,YW,
     :                                     XWIDW,YWIDW,STATUS)

*     Set sort parameters
               BSRT.ELAMIN = X_HWIDTH
               BSRT.ELBMIN = Y_HWIDTH
               BSRT.ELAMAX = BX_HWIDTH
               BSRT.ELBMAX = BY_HWIDTH
*
            ELSE
*
               CALL ARX_BOX(BSRT.ARDID,0,'ADD',.FALSE.,BXW,BYW,
     :                                     BXWIDW,BYWIDW,STATUS)

               BSRT.ELAMIN = 0.0
               BSRT.ELBMIN = 0.0
               BSRT.ELAMAX = BX_HWIDTH
               BSRT.ELBMAX = BY_HWIDTH
*
            ENDIF
*
	 ELSEIF (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A') THEN
*
*     If an annulus is wanted (i.e. the centre of the background box is
*     the same as that of the source box) then set the default for the
*     inner radius of the background annulus to the outer radius of the
*     source box.
            IF (ABS(BSRT.FIELD_RA-SRT.FIELD_RA) .LT. 0.001 .AND.
     &          ABS(BSRT.FIELD_DEC-SRT.FIELD_DEC) .LT. 0.001) THEN

	       CALL USI_DEF0R('BIRAD', SRAD2, STATUS)
               BSRT.SHAPE = 'A'
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

	    BSRT.ELAMAX = REAL(NINT(BRADOUT / PTOD))
	    BSRT.ELAMIN = REAL(NINT(BRADIN / PTOD))
            BSRT.ELBMIN = BSRT.ELAMIN
            BSRT.ELBMAX = BSRT.ELAMAX
	    BX_HWIDTH = BSRT.ELAMAX
	    BY_HWIDTH = BSRT.ELAMAX
*
            BIRADW=BSRT.ELAMIN*PTOD
            BORADW=BSRT.ELAMAX*PTOD

            CALL ARX_ANNULUS(BSRT.ARDID,0,'ADD',.FALSE.,BXW,BYW,
     :                                      BIRADW,BORADW,STATUS)

         ELSEIF (SRT.SHAPE .EQ. 'E') THEN
*
            ANNULAR= (ABS(BSRT.FIELD_RA-SRT.FIELD_RA) .LT. 0.001 .AND.
     &          ABS(BSRT.FIELD_DEC-SRT.FIELD_DEC) .LT. 0.001)

            IF (ANNULAR) THEN

*     Set some parameters if background box is an annular ellipse
*
	       CALL USI_DEF0R('BEXINN', ELAMAX, STATUS)
*
*        Background orientation equals source orientation
               BSRT.PHI = SRT.PHI
               BANGLE=ANGLE

            ELSE
*
*       Get orientation. +ve is anticlockwise from east
	       CALL USI_GET0R('BANGLE', BSRT.PHI, STATUS)
*
	       CALL USI_DEF0R('BEXINN', 0.0, STATUS)
*
*       Convert the angle to radians
               BSRT.PHI = BSRT.PHI * DTOR
*
            ENDIF
*
*      Find cos and sine of orientation
            BSRT.COSPHI = COS(BSRT.PHI)
            BSRT.SINPHI = SIN(BSRT.PHI)
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
               IF (SRT.ELAMAX .GT. 0) THEN
                  ELBMAX = SRT.ELBMAX * ELAMAX / SRT.ELAMAX
	          CALL USI_DEF0R('BEYOUT', ELBMAX, STATUS)
               ENDIF
*
               CALL USI_GET0R('BEYOUT', ELBMAX, STATUS)
*
               IF (STATUS .NE. SAI__OK) GOTO 999
*
*     Check user hasn't set the outer radii too large
*      Get outer value for X axis (semi-major) of ellipse
               EXMAX=SQRT((ELAMAX*BSRT.COSPHI)**2 +
     &                                  (ELBMAX*BSRT.SINPHI)**2)
               EYMAX=SQRT((ELAMAX*BSRT.SINPHI)**2 +
     &                                  (ELBMAX*BSRT.COSPHI)**2)
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
            BSRT.ELAMIN = REAL(NINT(ELAMIN / PTOD))
            BSRT.ELAMAX = REAL(NINT(ELAMAX / PTOD))
            BSRT.ELBMAX = REAL(NINT(ELBMAX / PTOD))
*
*      Calculate the inner Y axis
            BSRT.ELBMIN = BSRT.ELAMIN * BSRT.ELBMAX / BSRT.ELAMAX
*
*      Calculate maximum X and Y half widths for this ellipse
	    BX_HWIDTH = SQRT( (BSRT.ELAMAX*BSRT.COSPHI) ** 2 +
     &                             (BSRT.ELBMAX*BSRT.SINPHI) ** 2 )
	    BY_HWIDTH = SQRT( (BSRT.ELAMAX*BSRT.SINPHI) ** 2 +
     &                             (BSRT.ELBMAX*BSRT.COSPHI) ** 2 )
*
            BXWIDW=BSRT.ELAMAX*PTOD
            BYWIDW=BSRT.ELBMAX*PTOD

            CALL ARX_ELLIPSE(BSRT.ARDID,0,'ADD',.FALSE.,BXW,BYW,
     :                               BXWIDW,BYWIDW,BANGLE,STATUS)
            IF (ANNULAR) THEN
              CALL ARX_ELLIPSE(BSRT.ARDID,0,'AND',.TRUE.,XW,YW,
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
             BSRT.MIN_X=BCKGND_X-(NREBINX*BDIM(1))/2
             BSRT.MAX_X=BCKGND_X+NINT(REAL(NREBINX)*REAL(BDIM(1))/2.0
     &                                 + 0.4)
         ELSE
             BDIM(1)=1
             BSRT.MIN_X=BCKGND_X-BX_HWIDTH
             BSRT.MAX_X=BCKGND_X+BX_HWIDTH
*
         ENDIF
*
* Make sure you haven't gone over the edge
         BSRT.MIN_X = MAX(BSRT.MIN_X, HEAD.XSTART)
         BSRT.MAX_X = MIN(BSRT.MAX_X, HEAD.XEND)

         IF (INDEX(BIN_AXES,'2') .NE. 0) THEN
*
             BDIM(2)=INT(BY_HWIDTH*2/NREBINY)
             BSRT.MIN_Y=BCKGND_Y-INT(REAL(NREBINY)*REAL(BDIM(2))/2.0)
             BSRT.MAX_Y=BCKGND_Y+NINT(REAL(NREBINY)*REAL(BDIM(2))/2.0
     &                                    + 0.4)
*
         ELSE
             BDIM(2)=1
             BSRT.MIN_Y=BCKGND_Y-BY_HWIDTH
             BSRT.MAX_Y=BCKGND_Y+BY_HWIDTH

         ENDIF
*
* Make sure you haven't gone over the edge
         BSRT.MIN_Y = MAX(BSRT.MIN_Y, HEAD.YSTART)
         SRT.MAX_Y = MIN(SRT.MAX_Y, HEAD.YEND)

* Set time and sumsig for background same as source.
         BSRT.MIN_XD=SRT.MIN_XD
         BSRT.MIN_YD=SRT.MIN_YD
         BSRT.MIN_PH=SRT.MIN_PH
         BSRT.MIN_EN=SRT.MIN_EN
*
         BSRT.MAX_XD=SRT.MAX_XD
         BSRT.MAX_YD=SRT.MAX_YD
         BSRT.MAX_PH=SRT.MAX_PH
         BSRT.MAX_EN=SRT.MAX_EN
*
         BSRT.NTIME = SRT.NTIME
*
         DO LP=1,BSRT.NTIME
            BSRT.MIN_T(LP)=SRT.MIN_T(LP)
            BSRT.MAX_T(LP)=SRT.MAX_T(LP)
         ENDDO
*
         BDIM(3)=SDIM(3)
         BDIM(4)=SDIM(4)
         BDIM(5)=SDIM(5)
         BDIM(6)=SDIM(6)
         BDIM(7)=SDIM(7)
*
*  Set axes components the same as for source
         BSRT.NAXES=SRT.NAXES
*
         DO LP=1,8
            BSRT.BINAXIS(LP)=SRT.BINAXIS(LP)
         ENDDO
*
* Calculate the centre X and Y value in degrees
         BSRT.XCENT = - ((BSRT.MIN_X + BSRT.MAX_X) / 2.0 - HEAD.SKYCX)
     &                                                        * PTOD
         BSRT.YCENT = ((BSRT.MIN_Y + BSRT.MAX_Y) / 2.0 - HEAD.SKYCY)
     &                * PTOD * MFACT
*
* The outer X,Y values may have changed due to rebinning. If so
* recalculate them
         IF (SDIM(1) .GT. 1 .OR. SDIM(2) .GT. 1) THEN
            IF (BSRT.SHAPE .NE. 'E') THEN
               BSRT.ELAMAX = ABS(BSRT.MAX_X - (BSRT.MIN_X +
     &                                          BSRT.MAX_X)/2.0)
               BSRT.ELBMAX = ABS(BSRT.MAX_Y - (BSRT.MIN_Y +
     &                                          BSRT.MAX_Y)/2.0)
            ELSE
               BSRT.ELAMAX = BSRT.ELAMAX * ABS(BSRT.MAX_X -
     &             (BSRT.MIN_X + BSRT.MAX_X) / 2.0 ) / BX_HWIDTH
               BSRT.ELBMAX = BSRT.ELBMAX * ABS(BSRT.MAX_X -
     &             (BSRT.MIN_X + BSRT.MAX_X) / 2.0 ) / BX_HWIDTH
            ENDIF
         ENDIF
*
* Calculate cos and sine of the orientation
         BSRT.COSPHI = COS(BSRT.PHI)
         BSRT.SINPHI = SIN(BSRT.PHI)
*
      ELSE
*
*  Set background dimensions to one
         DO LP=1,7
            BDIM(LP)=1
         ENDDO
      ENDIF

      BSRT.IMAGE=(BSRT.DTYPE.EQ.'BinDS'.AND.
     &              BDIM(1).GT.1.AND.BDIM(2).GT.1)




*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_RANGESELECT',STATUS)
      ENDIF
*
      END



*+XRTSORT_SORT_BIN- Sorts rationalised XRT raw data into a binned data array
      SUBROUTINE XRTSORT_SORT_BIN(HEAD, SRT, BSRT, SDIM1, SDIM2,
     &           SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDIM1,
     &           BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &           NRBIN, NAZBIN, MDIM1,MDIM2,MRES,SMASK, BMASK,
     &           ELIPA2, ELIPB2, SDATA, BDATA, SQUAL, BQUAL, STATUS)
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
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD                      ! Header values
*
      RECORD /XRT_SCFDEF/ SRT                     ! Source sorting parameters
      RECORD /XRT_SCFDEF/ BSRT                    ! Bckgnd sorting parameters
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
      CHARACTER*20 EXT                            ! File extension name
      CHARACTER*80 DNAME                    ! Names of files
      REAL STBAD(MAXBAD), ENBAD(MAXBAD)           ! Bad time periods
      REAL XWIDTH,YWIDTH,TWIDTH,EWIDTH,PWIDTH     ! Binwidth of each axis
      REAL BXWIDTH,BYWIDTH                        ! Binwidth in backgnd box
      REAL XDWID,YDWID                            ! Width of detector axes
      REAL ELAWID,ELBWID                          ! Binwidth of elliptic axes
      REAL T1,T2                                  ! Lower and upper limits of
*                                                 ! a time bin
      INTEGER NBAD                                ! Number of bad time ranges
      INTEGER TLP,LP1,LP2,LP3,LP4,LP6,LP7,INLP,LP
      INTEGER BADEV                               ! No of events in hotspots
      LOGICAL QVAL
      CHARACTER*(DAT__SZLOC) ELOC,DLOC          ! Locators to datafiles
      CHARACTER*(DAT__SZLOC) LOCA(7),SLOCA(7)   ! Locators to data arrays
      INTEGER PTRA(7),NELEMS     ! Pointer to mapped arrays and item count
      INTEGER IX,UPPER,LOWER     ! number of indexes and ranges
*-

      IF (STATUS.NE.SAI__OK) RETURN

* Set events in hotspots counter to zero
      BADEV = 0
*
* Calculate bin widths for each axis. NB: SDIM1 can refer to the no. of
* radial bins.
      IF (NRBIN .EQ. 1) THEN
         XWIDTH = (SRT.MAX_X - SRT.MIN_X + 1) / REAL(SDIM1)
         YWIDTH = (SRT.MAX_Y - SRT.MIN_Y + 1) / REAL(SDIM2)
      ELSE
         XWIDTH = (SRT.MAX_X - SRT.MIN_X + 1) / 1.0
         YWIDTH = (SRT.MAX_Y - SRT.MIN_Y + 1) / 1.0
      ENDIF
*
      IF (SDIM1 .EQ. 1 .AND. SDIM2 .EQ. 1) THEN
         BXWIDTH = (BSRT.MAX_X - BSRT.MIN_X + 1) / REAL(BDIM1)
         BYWIDTH = (BSRT.MAX_Y - BSRT.MIN_Y + 1) / REAL(BDIM2)
      ELSE
         BXWIDTH=XWIDTH
         BYWIDTH=YWIDTH
      ENDIF
*
      XDWID = (SRT.MAX_XD - SRT.MIN_XD + 1) / REAL(SDIM3)
      YDWID = (SRT.MAX_YD - SRT.MIN_YD + 1) / REAL(SDIM4)
      TWIDTH = (SRT.MAX_T(SRT.NTIME) - SRT.MIN_T(1)) / REAL(SDIM5)
      PWIDTH = (SRT.MAX_PH - SRT.MIN_PH + 1) / REAL(SDIM6)
      EWIDTH = (SRT.MAX_EN - SRT.MIN_EN + 1) / REAL(SDIM7)
*
* Generate the squares of the elliptical minor and major axes
* for each radial bin (Max values).
      IF (NRBIN .GT. 1) THEN
*   Calculate binwidths for elliptical minor and major axes
         ELAWID = (SRT.ELAMAX - SRT.ELAMIN) / REAL(NRBIN)
         ELBWID = (SRT.ELBMAX - SRT.ELBMIN) / REAL(NRBIN)
*
         DO LP=1,NRBIN
            ELIPA2(LP) = (SRT.ELAMIN + (LP) * ELAWID) **2
            ELIPB2(LP) = (SRT.ELBMIN + (LP) * ELBWID) **2
         ENDDO
      ENDIF
*
*    Open observation event file STDEVT
      CALL RAT_HDLOOKUP(HEAD,'STDEVT','EXTNAME',EXT,STATUS)
      CALL HDS_OPEN(SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT,
     &              'READONLY',ELOC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Generate index into events
C     CALL RAT_INDX_COORD(HEAD,SRT,BSRT,EINDEX,STATUS)

***   Open quality file if wanted and get array of bad times for this obs.
      IF (SRT.QUAL_MORE .OR. SRT.QUAL_LESS) THEN
C         CALL XRTSORT_BADTIME(SRT,OBS,MAXBAD,NBAD,STBAD,ENBAD,STATUS)
C         IF (STATUS .NE. SAI__OK) GOTO 999
      ENDIF

***   Have the quality constraints been relaxed ?
      IF (SRT.QUAL_LESS) THEN

***      Open observation diff evenfile
         CALL RAT_HDLOOKUP(HEAD,'REJEVT','EXTNAME',EXT,STATUS)
         CALL USI_DEF0C('DOUT',SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))
     &                  //EXT,STATUS)
         CALL USI_GET0C('DOUT',DNAME,STATUS)
         CALL HDS_OPEN(DNAME,'READONLY',DLOC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

***   Locate the various data arrays
      CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,ELOC,LOCA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   For each index entry
      DO IX = 1,1
         LOWER = 1
         UPPER = HEAD.IEVTNU

***      map the event data arrays into memory
         CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &      NELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999


***      Check them against the sort parameters
         CALL XRTSORT_DOIT_BIN(HEAD,SRT,BSRT,%val(PTRA(1)),
     &      %val(PTRA(2)),%val(PTRA(3)), %val(PTRA(4)),
     &      %val(PTRA(5)), %val(PTRA(6)),%val(PTRA(7)),
     &      NELEMS, SDATA, SDIM1, SDIM2,
     &      SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &      BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &      MDIM1,MDIM2,MRES,SMASK,BMASK,
     &      SRT.QUAL_MORE, MAXBAD, NBAD, STBAD, ENBAD, XWIDTH, YWIDTH,
     &      XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &      BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)


***      unmap the arrays & memory
         CALL RAT_UNMAPEVE(SLOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDDO

***   annul the array locators
      CALL RAT_ANNULEVE(LOCA, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Same for the diff data?
      IF (SRT.QUAL_LESS) THEN
***      Locate the various data arrays
         CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,DLOC,LOCA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Find the size of the diff events
         CALL CMP_SIZE(DLOC,'TIME',UPPER,STATUS)
         DO IX = 1,1
            LOWER = 1

***         map the diff event data arrays into memory
            CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &         NELEMS,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

***         Check them against the sort parameters
            CALL XRTSORT_DOIT_BIN(HEAD,SRT,BSRT,%val(PTRA(1)),
     &         %val(PTRA(2)),
     &         %val(PTRA(3)),%val(PTRA(4)),%val(PTRA(5)), %val(PTRA(6)),
     &         %val(PTRA(7)), NELEMS, SDATA, SDIM1, SDIM2,
     &         SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &         BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &         MDIM1,MDIM2,MRES,SMASK,BMASK,
     &         SRT.QUAL_MORE, MAXBAD, NBAD,STBAD, ENBAD, XWIDTH, YWIDTH,
     &         XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &         BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)

***         unmap the arrays & memory
            CALL RAT_UNMAPEVE(SLOCA, STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999
         ENDDO

***      annul the array locators
         CALL RAT_ANNULEVE(LOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

      IF ( INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         CALL MSG_SETI('BAD', BADEV)
         CALL MSG_PRNT('Rejected ^BAD events found in '/
     &                                /'hotspots/deadspots')
      ENDIF

***  Set quality values for each pixel
***   Loop over each time bin
      DO TLP=1,SDIM5
***      Calculate lower and upper times of this bin
         T1=SRT.MIN_T(1) + TWIDTH * (TLP-1.0)
         T2=SRT.MIN_T(1) + TWIDTH * TLP

***      Test if this bin was within any of the on times of the instrument
***      Set the quality value to 0 if the bin is good or 1 if it is bad.
         QVAL=.FALSE.
         DO INLP=1,HEAD.NTRANGE
            IF (HEAD.TSTART(INLP).LT.T2.AND.
     &                         HEAD.TEND(INLP).GT.T1) THEN
               QVAL = .TRUE.
               GOTO 100
            ENDIF
         ENDDO

100      CONTINUE

***      If the time is within the pre-selection windows - check if it
***      is within the windows selected in XSORT
         IF (QVAL) THEN
*
            QVAL=.FALSE.
            DO INLP=1,SRT.NTIME
               IF ( SRT.MIN_T(INLP) .LT. T2 .AND.
     &                         SRT.MAX_T(INLP) .GT. T1 ) THEN
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
               IF (SRT.IMAGE) THEN
                 DO LP2=1,SDIM2
                    DO LP1=1,SDIM1
                       IF (.NOT. QVAL .OR. SMASK(LP1,LP2).EQ.0) THEN
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
***            Background quality
               IF (SRT.BCKGND) THEN
                 IF (BDIM1.GT.1.AND.BDIM2.GT.1.AND.NRBIN.EQ.1) THEN
                   DO LP2=1,BDIM2
                     DO LP1=1,BDIM1
                       IF (.NOT. QVAL .OR. BMASK(LP1,LP2).EQ.0) THEN
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

***   Close any opened files
      CALL HDS_CLOSE(ELOC, STATUS)
      IF (SRT.QUAL_LESS) CALL HDS_CLOSE(DLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_SORT_BIN',STATUS)
      ENDIF
*
      END


*+XRTSORT_DOIT_BIN     checks mapped events against sort parameters
      SUBROUTINE XRTSORT_DOIT_BIN(HEAD, SRT, BSRT, TIME, XPIX, YPIX,
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
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD            ! HEADER values
      RECORD /XRT_SCFDEF/ SRT, BSRT     ! Sort parameters
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
      REAL XWIDTH,YWIDTH,TWIDTH,PWIDTH,EWIDTH     ! Bin widths of axes
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
      SAVE_YMIN = SRT.MIN_Y
      SAVE_YMAX = SRT.MAX_Y

      YMAX = HEAD.YEND


***   Test if this is an HRI file
      LHRI = (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0)


***   Calculate the pixel centres of each box
      SCEN_X = (SRT.MIN_X + SRT.MAX_X) / 2.0
      SCEN_Y = (SRT.MIN_Y + SRT.MAX_Y) / 2.0
      BCEN_X = (BSRT.MIN_X + BSRT.MAX_X) / 2.0
      BCEN_Y = (BSRT.MIN_Y + BSRT.MAX_Y) / 2.0

      BSCTIM = HEAD.BASE_SCTIME

***   Calculate the squares of the elliptical axis - if any
      IF (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A' .OR.
     &                              SRT.SHAPE .EQ. 'E') THEN
        SAMIN2 = SRT.ELAMIN **2
        SAMAX2 = SRT.ELAMAX **2
        SBMIN2 = SRT.ELBMIN **2
        SBMAX2 = SRT.ELBMAX **2
        BAMIN2 = BSRT.ELAMIN **2
        BAMAX2 = BSRT.ELAMAX **2
        BBMIN2 = BSRT.ELBMIN **2
        BBMAX2 = BSRT.ELBMAX **2

***      Calculate the product of the squares of the two elliptical axes
        SA2B2I = SAMIN2 * SBMIN2
        SA2B2O = SAMAX2 * SBMAX2
        BA2B2I = BAMIN2 * BBMIN2
        BA2B2O = BAMAX2 * BBMAX2

***      Set a local cos and sin of the orientation angle for speed
        SCPHI = SRT.COSPHI
        SSPHI = SRT.SINPHI
        BCPHI = BSRT.COSPHI
        BSPHI = BSRT.SINPHI
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
        IF (LHRI .AND..NOT. XRT_HSPOT(HEAD, XEV, YEV)) THEN

          BADEV = BADEV + 1

        ELSE

*  If the source box is circular, annular or elliptical, calculate
***         various numbers.
          IF (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A' .OR.
     &                              SRT.SHAPE .EQ. 'E') THEN

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
            IF (SRT.BCKGND) THEN

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
          DO WHILE (.NOT.OK.AND.TLP.LE.SRT.NTIME)
            OK= (SRT.MIN_T(TLP) .LE. TEV .AND. SRT.MAX_T(TLP)
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
            OK= (SRT.MIN_PH .LE. AEV .AND.SRT.MAX_PH .GE. AEV) .AND.
     :           (SRT.MIN_EN .LE. CEV .AND.SRT.MAX_EN .GE. CEV) .AND.
     :           (SRT.MIN_XD .LE.XDEV .AND.SRT.MAX_XD .GE.XDEV) .AND.
     :           (SRT.MIN_YD .LE.YDEV .AND.SRT.MAX_YD .GE.YDEV)
*
          ENDIF

*  if event has survived this far check spatial selection
          SOK=OK
          BOK=OK
          IF (OK) THEN

*  rectangle
            IF ( SRT.SHAPE .EQ. 'R') THEN

              SOK=((SRT.MIN_X .LE. XEV .AND. SRT.MAX_X .GE. XEV)
     &                               .AND.
     &             (SRT.MIN_Y .LE. YEV .AND. SRT.MAX_Y .GE. YEV))
              IF (SRT.BCKGND) THEN
                BOK=((BSRT.MIN_X .LE.XEV.AND.BSRT.MAX_X.GE.XEV)
     &                            .AND.
     &                 (BSRT.MIN_Y.LE.YEV.AND.BSRT.MAX_Y.GE.YEV))
              ENDIF

*  circle, annulus or ellipse (all treated as ellipse)
            ELSEIF (INDEX('CAE',SRT.SHAPE).NE.0) THEN

              SOK=((SELPX2*SBMIN2 + SELPY2*SAMIN2) .GE. SA2B2I
     &                               .AND.
     &             (SELPX2*SBMAX2 + SELPY2*SAMAX2) .LE. SA2B2O)
              IF (SRT.BCKGND) THEN
                BOK=((BELPX2*BBMIN2 + BELPY2*BAMIN2) .GE. BA2B2I
     &                              .AND.
     &                (BELPX2*BBMAX2 + BELPY2*BAMAX2) .LE. BA2B2O)
              ENDIF

*  ARD description
            ELSEIF (SRT.SHAPE.EQ.'I') THEN

*  find position within spatial mask
              IF (SRT.IMAGE) THEN
                MEL1=INT((XEV-SRT.MIN_X)/XWIDTH) + 1
                IF (HEAD.ORIGIN.EQ.'MPE') THEN
                  MEL2=SDIM2 - INT((YEV-SRT.MIN_Y)/YWIDTH)
                ELSE
                  MEL2=INT((YEV-SRT.MIN_Y)/YWIDTH) + 1
                ENDIF
              ELSE
                MEL1=INT((XEV-HEAD.XSTART)/MRES)+1
                IF (HEAD.ORIGIN.EQ.'MPE') THEN
                  MEL2=INT((-YEV-HEAD.YSTART)/MRES)+1
                ELSE
                  MEL2=INT((YEV-HEAD.YSTART)/MRES)+1
                ENDIF
              ENDIF
              SOK=(SMASK(MEL1,MEL2).NE.0)
              IF (SRT.BCKGND) THEN
                BOK=(BMASK(MEL1,MEL2).NE.0)
              ENDIF
            ENDIF

          ENDIF


          IF (SOK) THEN

*  Calculate position of data in array
*  The first two dimensions of the array can be either
*  X and Y pixel or RADIAL and AZIMUTHAL bin, depending on
*  the user selection.
            IF (SRT.IMAGE) THEN
*
*  X,Y bins:
              EL1=INT((XEV-SRT.MIN_X)/XWIDTH) + 1
*  Calculate Y element according to orientation of raw pixels
              IF (HEAD.ORIGIN.EQ.'MPE') THEN
                EL2=SDIM2 - INT((YEV-SRT.MIN_Y)/YWIDTH)
              ELSE
                EL2=INT((YEV-SRT.MIN_Y)/YWIDTH) + 1
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
            EL3=INT((XDEV-SRT.MIN_XD)/XDWID) + 1
            EL4=INT((YDEV-SRT.MIN_YD)/YDWID) + 1
            EL5=MIN((INT((TEV-SRT.MIN_T(1))/TWIDTH) + 1), SDIM5)
            EL6=INT((AEV-SRT.MIN_PH)/PWIDTH) + 1
            EL7=INT((CEV-SRT.MIN_EN)/EWIDTH) + 1


            SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &         SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0


          ENDIF

          IF ( SRT.BCKGND.AND.BOK) THEN

*  Calculate position of data in array
*  NB: no polar bins in background
            IF (BDIM1.GT.1.AND.BDIM2.GT.1) THEN
              EL1=INT((XEV-BSRT.MIN_X)/BXWIDTH) + 1
*  Calculate Y element depending on orientation of raw pixels
              IF (HEAD.ORIGIN.EQ.'MPE') THEN
                EL2=BDIM2 - INT((YEV-BSRT.MIN_Y)/BYWIDTH)
              ELSE
                EL2=INT((YEV-BSRT.MIN_Y)/BYWIDTH) + 1
              ENDIF
*
            ELSE

              EL1=1
              EL2=1

            ENDIF

            EL3=INT((XDEV-BSRT.MIN_XD)/XDWID) + 1
            EL4=INT((YDEV-BSRT.MIN_YD)/YDWID) + 1
            EL5=INT((TEV-BSRT.MIN_T(1))/TWIDTH) + 1
            EL6=INT((AEV-BSRT.MIN_PH)/PWIDTH) + 1
            EL7=INT((CEV-BSRT.MIN_EN)/EWIDTH) + 1
*

            BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &           BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0

          ENDIF

        ENDIF

      ENDDO
*

999   CONTINUE
*
      SRT.MIN_Y = SAVE_YMIN
      SRT.MAX_Y = SAVE_YMAX
*
      END

*+XRTSORT_SORT_EVE - Sorts XRT data into a binned data array
      SUBROUTINE XRTSORT_SORT_EVE(HEAD,SRT,BSRT,MAXLIM, EVE_X, EVE_Y,
     &                 EVE_XD, EVE_YD, EVE_T, EVE_P, EVE_E, BEVE_X,
     &                 BEVE_Y,BEVE_XD,BEVE_YD,BEVE_T,BEVE_P,BEVE_E,
     &                 MDIM1,MDIM2,MRES,SMASK,BMASK,
     &                 TOTEV_SRC, TOTEV_BCK, STATUS)
*    Description :
*        Sorts raw events from an XRT event datafile into event lists
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_SCFDEF/ SRT,BSRT                ! Sort control structures
      RECORD /XRT_HEAD/ HEAD
      INTEGER MAXLIM                              ! Maximum number of events
      INTEGER MDIM1,MDIM2
      REAL MRES
      INTEGER SMASK(MDIM1,MDIM2)
      INTEGER BMASK(MDIM1,MDIM2)
*    Import-Export :
      REAL EVE_X(MAXLIM)
      REAL EVE_Y(MAXLIM)
      INTEGER EVE_XD(MAXLIM)
      INTEGER EVE_YD(MAXLIM)
      DOUBLE PRECISION EVE_T(MAXLIM)
      INTEGER EVE_P(MAXLIM)
      INTEGER EVE_E(MAXLIM)
      REAL BEVE_X(MAXLIM)
      REAL BEVE_Y(MAXLIM)
      INTEGER BEVE_XD(MAXLIM)
      INTEGER BEVE_YD(MAXLIM)
      DOUBLE PRECISION BEVE_T(MAXLIM)
      INTEGER BEVE_P(MAXLIM)
      INTEGER BEVE_E(MAXLIM)
*    Export :
      INTEGER TOTEV_SRC, TOTEV_BCK                ! Number of events put into
*                                                 ! source and bckgnd lists.
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXBAD
         PARAMETER (MAXBAD=1000)                  ! Max no. of bad time periods
*    Local variables :
      CHARACTER*20 EXT
      REAL STBAD(MAXBAD), ENBAD(MAXBAD)           ! Bad time periods
      INTEGER NBAD                                ! Number of bad time periods
      INTEGER BADEV                               ! Number of pixels in hotspots
      CHARACTER*80 DNAME                    ! Names of files
      CHARACTER*(DAT__SZLOC) ELOC,DLOC          ! Locators to datafiles
      CHARACTER*(DAT__SZLOC) LOCA(7),SLOCA(7)   ! Locators to data arrays
      INTEGER PTRA(7),NELEMS     ! Pointer to mapped arrays and item count
      INTEGER IX,UPPER,LOWER     ! number of indexes and ranges
*
*-
***   Initialise hotspot counter
      BADEV = 0
*
*    Open observation event file STDEVT from INC_RDF
      CALL RAT_HDLOOKUP(HEAD,'STDEVT','EXTNAME',EXT,STATUS)
      CALL HDS_OPEN(SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT,
     &              'READONLY',ELOC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Generate index into events
C     CALL RAT_INDX_COORD(HEAD,SRT,BSRT,INDEX,STATUS)

***   Open quality file if wanted and get array of bad times for this obs.
      IF (SRT.QUAL_MORE .OR. SRT.QUAL_LESS) THEN
C         CALL XRTSORT_BADTIME(SRT,OBS,MAXBAD,NBAD,STBAD,ENBAD,STATUS)
C         IF (STATUS .NE. SAI__OK) GOTO 999
      ENDIF

***   Have the quality constraints been relaxed ?
      IF (SRT.QUAL_LESS) THEN

***      Open observation diff evenfile
         CALL RAT_HDLOOKUP(HEAD,'REJEVT','EXTNAME',EXT,STATUS)
         CALL USI_DEF0C('DIFFILE',SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))
     &                  //EXT,STATUS)
         CALL USI_GET0C('DIFFILE',DNAME,STATUS)
         CALL HDS_OPEN(DNAME,'READONLY',DLOC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

***   Locate the various data arrays
      CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,ELOC,LOCA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   For each index entry
      DO IX = 1,1
         LOWER = 1
         UPPER = HEAD.IEVTNU

***      map the event data arrays into memory
         CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &      NELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Check them against the sort parameters
         CALL XRTSORT_DOIT_EVE(HEAD, SRT, BSRT,%val(PTRA(1)),
     &      %val(PTRA(2)),
     &      %val(PTRA(3)), %val(PTRA(4)), %val(PTRA(5)), %val(PTRA(6)),
     &      %val(PTRA(7)), NELEMS, MAXLIM, EVE_X, EVE_Y, EVE_XD, EVE_YD,
     &      EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y, BEVE_XD, BEVE_YD,
     &      BEVE_T, BEVE_P, BEVE_E, MDIM1,MDIM2,MRES,SMASK,BMASK,
     &      TOTEV_SRC, TOTEV_BCK, SRT.QUAL_MORE,
     &      MAXBAD, NBAD, STBAD, ENBAD, BADEV)

***      unmap the arrays & memory
         CALL RAT_UNMAPEVE(SLOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDDO

***   annul the array locators
      CALL RAT_ANNULEVE(LOCA, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Same for the diff data
      IF (SRT.QUAL_LESS) THEN

***      Locate the various data arrays
         CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,DLOC,LOCA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Find the size of the diff events
         CALL CMP_SIZE(DLOC,'TIME',UPPER,STATUS)
         DO IX = 1,1
            LOWER = 1

***         map the diff event data arrays into memory
            CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &         NELEMS,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

***         Check them against the sort parameters
            CALL XRTSORT_DOIT_EVE(HEAD,SRT,BSRT,%val(PTRA(1)),
     &         %val(PTRA(2)),
     &         %val(PTRA(3)),%val(PTRA(4)),%val(PTRA(5)), %val(PTRA(6)),
     &         %val(PTRA(7)),NELEMS,MAXLIM,EVE_X, EVE_Y, EVE_XD, EVE_YD,
     &         EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y, BEVE_XD, BEVE_YD,
     &         BEVE_T, BEVE_P,BEVE_E,MDIM1,MDIM2,MRES,SMASK,BMASK,
     &         TOTEV_SRC,TOTEV_BCK, SRT.QUAL_MORE,
     &         MAXBAD, NBAD, STBAD, ENBAD, BADEV)

***         unmap the arrays & memory
            CALL RAT_UNMAPEVE(SLOCA, STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999
         ENDDO

***      annul the array locators
         CALL RAT_ANNULEVE(LOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

      IF (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         CALL MSG_SETI('BAD', BADEV)
         CALL MSG_PRNT('Rejected ^BAD events found in '/
     &                             /'hotspots/deadspots')
      ENDIF

***   Close the files
      CALL HDS_CLOSE(ELOC, STATUS)
      IF (SRT.QUAL_LESS) CALL HDS_CLOSE(DLOC, STATUS)

999   CONTINUE

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from XRTSORT_SORT_EVE',STATUS)
      ENDIF

      END

*+XRTSORT_DOIT_EVE    Sorts XRT events into event lists
      SUBROUTINE XRTSORT_DOIT_EVE(HEAD,SRT,BSRT,TIME,XPIX,YPIX,XDET,
     &              YDET, AMPL, CAMPL, NELEMS, MAXLIM, EVE_X, EVE_Y,
     &              EVE_XD, EVE_YD, EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y,
     &              BEVE_XD, BEVE_YD, BEVE_T, BEVE_P, BEVE_E,
     &              MDIM1,MDIM2,MRES,SMASK,BMASK,
     &              TOTEV_SRC,TOTEV_BCK,QCHECK,MAXBAD,NBAD,STBAD,ENBAD,
     &              BADEV)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'INC_XRTSRT'
      INCLUDE 'INC_XRTHEAD'
*    Import :
      RECORD /XRT_HEAD/ HEAD                      ! Header values
      RECORD /XRT_SCFDEF/ SRT                     ! Source sorting parameters
      RECORD /XRT_SCFDEF/ BSRT                    ! Bckgnd sorting parameters
      INTEGER NELEMS			  ! Number of array elements
      DOUBLE PRECISION TIME(NELEMS)       ! Event times
      INTEGER XPIX(NELEMS), YPIX(NELEMS)  ! Array of coordinates
      INTEGER XDET(NELEMS), YDET(NELEMS)  ! Array of detector coords
      INTEGER AMPL(NELEMS), CAMPL(NELEMS) ! Array of photon events
      LOGICAL QCHECK                              ! Check quality values ?
      INTEGER MAXBAD                              ! Dimension of bad arrays
      INTEGER NBAD
      REAL STBAD(MAXBAD)                          ! Start of bad period
      REAL ENBAD(MAXBAD)                          ! End of bad period
      INTEGER MAXLIM                              ! Event list max extent
      INTEGER MDIM1,MDIM2
      REAL MRES
      INTEGER SMASK(MDIM1,MDIM2)
      INTEGER BMASK(MDIM1,MDIM2)
*    Import-Export :
      REAL EVE_X(MAXLIM),EVE_Y(MAXLIM)
      INTEGER EVE_XD(MAXLIM),EVE_YD(MAXLIM)
      INTEGER EVE_P(MAXLIM),EVE_E(MAXLIM)         ! Source lists
      DOUBLE PRECISION EVE_T(MAXLIM)
      REAL BEVE_X(MAXLIM),BEVE_Y(MAXLIM)
      INTEGER BEVE_XD(MAXLIM),BEVE_YD(MAXLIM)
      INTEGER BEVE_P(MAXLIM),BEVE_E(MAXLIM)       ! Bckgnd event lists
      DOUBLE PRECISION BEVE_T(MAXLIM)             ! Bckgnd event lists
*
      INTEGER TOTEV_SRC                           ! Number of source events
      INTEGER TOTEV_BCK                           ! Number of bckgnd events
      INTEGER BADEV                               ! Number of events in hotspots
*    Functions :
      INTEGER XRT_HSPOT
         EXTERNAL XRT_HSPOT
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER BLP,TLP
      INTEGER XEV,YEV,XDEV,YDEV,AEV,CEV
      DOUBLE PRECISION TEV
      INTEGER SCEN_X,SCEN_Y                        ! Pixel centre of src box
      INTEGER BCEN_X,BCEN_Y                        ! Pixel centre of src box
      REAL SAMIN2,SAMAX2,SBMIN2,SBMAX2       ! Squares of the min. and max.
*                                            ! values for the source ellip axes.
      REAL BAMIN2,BAMAX2,BBMIN2,BBMAX2       !    Same for the background.
      REAL SA2B2I,SA2B2O                     ! Product of the squares of the
*                                            ! inner and outer source axes
      REAL BA2B2I,BA2B2O                     ! Product of the squares of the
*                                            ! inner and outer background axes
      REAL SDIFFX,SDIFFY                     ! X,Y offset from box centre (pix)
      REAL BDIFFX,BDIFFY                     !    Same for the background
      REAL SELPX2,SELPY2                     ! Square of the photon X,Y pos.
*                                            ! in source box elliptical coords
      REAL BELPX2,BELPY2                     ! Square of the photon X,Y pos.
*                                            ! in bckgnd box elliptical coords
      REAL SCPHI,SSPHI                       ! Cos and Sine of src orientation
      REAL BCPHI,BSPHI                       ! Cos and Sine of bck orientation
      REAL HPIX60                            ! Pixel size in arcmins
*
      LOGICAL LHRI                           ! Is detector the HRI ?
      LOGICAL OK,SOK,BOK
      INTEGER IX
      INTEGER MEL1,MEL2
*-
      IF (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         LHRI = .TRUE.
      ELSE
         LHRI = .FALSE.
      ENDIF

*  Calculate the box pixel centres
      SCEN_X = (SRT.MIN_X + SRT.MAX_X) / 2.0
      SCEN_Y = (SRT.MIN_Y + SRT.MAX_Y) / 2.0
      BCEN_X = (BSRT.MIN_X + BSRT.MAX_X) / 2.0
      BCEN_Y = (BSRT.MIN_Y + BSRT.MAX_Y) / 2.0

*  Calculate pixel size in arcmins
      HPIX60 = HEAD.PIXEL / 60.0

*  Calculate the squares of the elliptical axis - if any
      IF (INDEX('CAE', SRT.SHAPE) .NE. 0) THEN
        SAMIN2 = SRT.ELAMIN **2
        SAMAX2 = SRT.ELAMAX **2
        SBMIN2 = SRT.ELBMIN **2
        SBMAX2 = SRT.ELBMAX **2
        BAMIN2 = BSRT.ELAMIN **2
        BAMAX2 = BSRT.ELAMAX **2
        BBMIN2 = BSRT.ELBMIN **2
        BBMAX2 = BSRT.ELBMAX **2

*  Calculate the product of the squares of the two elliptical axes
        SA2B2I = SAMIN2 * SBMIN2
        SA2B2O = SAMAX2 * SBMAX2
        BA2B2I = BAMIN2 * BBMIN2
        BA2B2O = BAMAX2 * BBMAX2
*  Set a local cos and sin of the orientation angle for speed
        SCPHI = SRT.COSPHI
        SSPHI = SRT.SINPHI
        BCPHI = BSRT.COSPHI
        BSPHI = BSRT.SINPHI
      ENDIF

*  Loop over each input record
      DO IX = 1, NELEMS

*  Copy event to simpler variables
        TEV=TIME(IX) - HEAD.BASE_SCTIME
        XEV=XPIX(IX)
        YEV=YPIX(IX)
        XDEV=XDET(IX)
        YDEV=YDET(IX)
        AEV=AMPL(IX)
        CEV=CAMPL(IX)
*  Fix for HRI no corrected events. set to value '1'
        IF (LHRI) CEV = 1

*  Test if this is from an HRI hotspot or deadspot
        IF (LHRI .AND..NOT. XRT_HSPOT(HEAD, XEV, YEV)) THEN

          BADEV = BADEV + 1

        ELSE

*  If the source box is circular, annular or elliptical, calculate
*  various numbers.
          IF (INDEX('CAE', SRT.SHAPE) .NE. 0) THEN

*  calculate the offset in X and Y celestial pixels from the
*  source box centre
            SDIFFX = XEV - SCEN_X
            SDIFFY = YEV - SCEN_Y

*  calculate the offset in X and Y celestial pixels from the
*  background box centre
            BDIFFX = XEV - BCEN_X
            BDIFFY = YEV - BCEN_Y

*  calculate the position in elliptical coordinates - source box
*  NB: This also handles circles
            SELPX2 = (SDIFFX * SCPHI + SDIFFY * SSPHI) ** 2
            SELPY2 = (SDIFFX * SSPHI + SDIFFY * SCPHI) ** 2

*  calculate the position in elliptical coordinates - backgnd box
            BELPX2 = (BDIFFX * BCPHI + BDIFFY * BSPHI) ** 2
            BELPY2 = (BDIFFX * BSPHI + BDIFFY * BCPHI) ** 2

          ENDIF

*  Check if each event is within the  selected time range:
          OK = .FALSE.
          TLP=1
          DO WHILE (.NOT.OK.AND.TLP.LE.SRT.NTIME)
            OK=(SRT.MIN_T(TLP).LE.TEV.AND.SRT.MAX_T(TLP).GE. TEV)
            TLP=TLP+1
          ENDDO

          IF (OK) THEN

*  Check various other limits
            OK=((SRT.MIN_PH .LE. AEV .AND.SRT.MAX_PH .GE. AEV) .AND.
     :          (SRT.MIN_EN .LE. CEV .AND.SRT.MAX_EN .GE. CEV) .AND.
     :          (SRT.MIN_XD .LE. XDEV .AND.SRT.MAX_XD .GE. XDEV) .AND.
     :          (SRT.MIN_YD .LE. YDEV .AND.SRT.MAX_YD .GE. YDEV))
          ENDIF

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

          SOK=OK
          BOK=OK
          IF (OK) THEN

*  Check if event is within the selected spatial region

*  Rectangle
            IF ( SRT.SHAPE .EQ. 'R' ) THEN
              SOK=((SRT.MIN_X .LE. XEV .AND. SRT.MAX_X .GE. XEV)
     &                                  .AND.
     &            (SRT.MIN_Y .LE. YEV .AND. SRT.MAX_Y .GE. YEV))
              IF (SRT.BCKGND) THEN
                BOK=((BSRT.MIN_X.LE.XEV .AND. BSRT.MAX_X.GE.XEV)
     &                                  .AND.
     &             (BSRT.MIN_Y.LE.YEV .AND. BSRT.MAX_Y.GE.YEV))
              ENDIF

*  Circle, annulus or ellipse (all treated as ellipse)
            ELSEIF (INDEX( 'CAE', SRT.SHAPE ) .NE. 0 ) THEN
              SOK=((SELPX2*SBMIN2 + SELPY2*SAMIN2) .GE. SA2B2I
     &                                  .AND.
     &            (SELPX2*SBMAX2 + SELPY2*SAMAX2) .LE. SA2B2O)
              IF (SRT.BCKGND) THEN
                BOK=((BELPX2*BBMIN2 + BELPY2*BAMIN2).GE.BA2B2I
     &                                  .AND.
     &             (BELPX2*BBMAX2 + BELPY2*BAMAX2).LE.BA2B2O)
              ENDIF

*  ARD description
            ELSEIF (SRT.SHAPE .EQ. 'I') THEN
              MEL1=INT((XEV-HEAD.XSTART)/MRES)+1
              IF (HEAD.ORIGIN.EQ.'MPE') THEN
                MEL2=INT((-YEV-HEAD.YSTART)/MRES)+1
              ELSE
                MEL2=INT((YEV-HEAD.YSTART)/MRES)+1
              ENDIF
              SOK=(SMASK(MEL1,MEL2).NE.0)
              IF (SRT.BCKGND) THEN
                OK=(BMASK(MEL1,MEL2).NE.0)
              ENDIF
            ENDIF

          ENDIF

*  Add to source event list
          IF (SOK) THEN

            TOTEV_SRC=TOTEV_SRC+1

            EVE_X(TOTEV_SRC)=-(XEV - HEAD.SKYCX)*HPIX60
            EVE_Y(TOTEV_SRC)=-(YEV - HEAD.SKYCY)*HPIX60
            EVE_XD(TOTEV_SRC)=XDEV
            EVE_YD(TOTEV_SRC)=YDEV
            EVE_T(TOTEV_SRC)=TEV
            EVE_P(TOTEV_SRC)=AEV
            EVE_E(TOTEV_SRC)=CEV

          ENDIF

*  Add to background event list
          IF ( SRT.BCKGND.AND.BOK ) THEN

            TOTEV_BCK=TOTEV_BCK+1
            BEVE_X(TOTEV_BCK)=-XEV*HPIX60
            BEVE_Y(TOTEV_BCK)=-YEV*HPIX60
            BEVE_XD(TOTEV_BCK)=XDEV
            BEVE_YD(TOTEV_BCK)=YDEV
            BEVE_T(TOTEV_BCK)=TEV
            BEVE_P(TOTEV_BCK)=AEV
            BEVE_E(TOTEV_BCK)=CEV

          ENDIF

        ENDIF

      ENDDO

999   CONTINUE

      END





*+XRTSORT_WRISORT    Writes the sorting conditions into selection structure
      SUBROUTINE XRTSORT_WRISORT(ID, VERSION, ASRT, STATUS)
*    Description :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'INC_XRTSRT'
*    Import :
      INTEGER ID		       ! ID of file
      CHARACTER*(*) VERSION
      RECORD /XRT_SCFDEF/ ASRT         ! Sort block for file
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      INTEGER SID
*-
      IF (STATUS .NE. SAI__OK) RETURN

      CALL SLN_NEWREC(VERSION,SID,STATUS)


*  write spatial selection
      CALL SLN_PUTARD(SID,'SPACE',ASRT.ARDID,STATUS)


*  write detector coordinate selection
      CALL SLN_PUTRNGI(SID,'XDET',1,ASRT.MIN_XD,ASRT.MAX_XD,STATUS)
      CALL SLN_PUTRNGI(SID,'YDET',1,ASRT.MIN_YD,ASRT.MAX_YD,STATUS)


*  write time ranges
      CALL SLN_PUTRNGI(SID,'TIME',ASRT.NTIME,
     :             ASRT.MIN_T,ASRT.MAX_T,STATUS)


*  write PH channel selection
      CALL SLN_PUTRNGI(SID,'PH_CHANNEL',1,ASRT.MIN_PH,ASRT.MAX_PH,
     :                                                      STATUS)

*  write corrected PH channel selection
      CALL SLN_PUTRNGI(SID,'ENERGY',1,ASRT.MIN_EN,ASRT.MAX_EN,
     :                                                    STATUS)


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
      INCLUDE 'DAT_PAR'
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
      INCLUDE 'DAT_PAR'
*    Global variables :
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
