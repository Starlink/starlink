*+  RATIO - Ratios two ranges of any axis of a dataset
      SUBROUTINE RATIO( STATUS )
*
*    Description :
*
*     Finds the ratio of two ranges on any axis of an arbitrarily dimensioned
*     data array. A few examples:-
*
*       o  1D array, ratioed on ENERGY gives a hardness index for spectrum
*
*       o  2D array, say TIME vs ENERGY. Ratio along TIME to give relative
*          intensity or along ENERGY to give a time series of hardness values.
*
*       o  3D array, say TIME vs X,Y. Ratio on TIME to give relative intensity
*          of two images.
*
*       o  3D array, say ENERGY vs X,Y. Ratio on ENERGY to get hardness map
*          ( or image ).
*
*       o  4D array, ENERGY vs TIME vs X,Y. Could get time sequence of hardness
*          maps ; a relative intensity of two spectral image ranges and so on.
*
*    Environment parameters :
*
*     INP         UNIV(R)    The input dataset
*     NORMAL      LOGICAL    Renormalise data on output
*     AXIS        INTEGER(R) The axis to ratio along
*     BANDS       REAL(R)[2] Ranges on axis to divide
*     OUT         UNIV(W)    The output dataset
*     WEIGHT      LOGICAL(R) Use variances if present
*
*    Method :
*
*     Initialisation.
*     Select input data object.
*     Tell user about input.
*     Grab input data , denormalise as required
*     If no valid quality so far, create a dummy.
*     Output table describing axes, getting axis data at same time.
*     Select axis to ratio on.
*     Transform input dims and selected axis into 7D system.
*     If Input ISNT Primitive
*        Denormalise any normalised axes
*     Select bands on axis where ratioing is to take place.
*     Find array indices corresponding to those bands.
*     Unmap any axis data remaining.
*     Create output dimensions and transform into 7D.
*     Create output data object with appropriate auxilliary structures.
*     Map output objects.
*     Create workspace arrays.
*     Do ratioing process. ( see RATIO_INT )
*     Unmap workspace, input and output data.
*     Copy axis and MORE information from input to output.
*     Create or append HISTORY information.
*     Unmap everything else.
*
*    Deficiencies :
*
*     REQUIRES FURTHER / ZERO PROTECTION
*     Will not work with non-contiguous axis data ( surprise, surprise! )
*
*    Bugs :
*
*    Authors :
*
*     David Allan (BHVAD::DJA)
*
*    History :
*
*      8 Sep 88 : V1.0-1  Original (DJA)
*     20 Dec 89 : V1.0-2  A well deserved overhaul (DJA)
*     15 Jan 90 : V1.0-3  Now informs user if output scalar  (DJA)
*     16 Feb 90 : V1.2-0  Uses QUAL_PAR to write quality values (DJA)
*     12 Mar 90 : V1.2-1  Bix fixed when input dataset had no quality (DJA)
*      7 Jul 90 : V1.2-2  Various improvements (DJA)
*      3 Mar 92 : V1.6-0  Bug fix in no variance case, and tidy up (DJA)
*      9 Jul 92 : V1.6-1  Provide WEIGHTS option (DJA)
*     21 Jul 92 : V1.6-2  More improvements to quality handling (DJA)
*     17 Nov 92 : V1.7-0  Normalisation done properly (DJA)
*     19 Nov 92 : V1.7-1  Updated arguments to AXIS_VAL2PIX (DJA)
*     25 Feb 94 : V1.7-2  Use BIT_ routines to do bit manipulations (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*     28 Mar 95 : V1.8-1  Use new data interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER                STATUS              ! Run-time error code
*
*    Function declarations :
*
      INTEGER                CHR_LEN             ! Logical length of a string
      LOGICAL                CHR_SIMLR           !
*
*    Local constants :
*
      INTEGER                MAXLINES
        PARAMETER            ( MAXLINES = 15 )
*
*    Local variables :
*
      CHARACTER*80		AX_LABEL(ADI__MXDIM)	! Axis labels
      CHARACTER*80		AX_UNITS(ADI__MXDIM)	! Axis labels
      CHARACTER*80           DATEXT              ! Message text
      CHARACTER*80           TEXT(MAXLINES)      ! History text

      REAL			AX_BASE(ADI__MXDIM)	! Axis base values
      REAL			AX_PX1(ADI__MXDIM)	! First axis value
      REAL			AX_PXN(ADI__MXDIM)	! Last axis value
      REAL			AX_SCALE(ADI__MXDIM)	! Axis scale values
      REAL                   BANDS(4)            ! Axis ranges boundaries
      REAL                   OVAR                ! Output variance if 1D input
      REAL                   LHC, RHC, LHW, RHW  ! Bin centres and widths
      REAL                   WIDTH1, WIDTH2      ! Band widths

      INTEGER			AX_PTR(ADI__MXDIM)	! Ptr to axis data
      INTEGER			AX_WPTR(ADI__MXDIM)	! Ptr to axis widths
      INTEGER                AXIS                ! Axis selected for ratio
      INTEGER                DIMS(ADI__MXDIM)    ! A spare dims array
      INTEGER                DUMMY               !
      INTEGER                I,J                 ! Axis loop counters
      INTEGER                IDIMS(ADI__MXDIM)   ! Input data dimensions
      INTEGER                ID_PTR              ! Input data pointer
      INTEGER			IFID			! Input dataset id
      INTEGER                IQ_PTR              ! Input quality pointer
      INTEGER                IV_PTR              ! Input variance pointer
      INTEGER                LOW1,HIGH1          ! Pixel positions of BANDS(1/2)
      INTEGER                LOW2,HIGH2          ! Pixel positions of BANDS(3/4)
      INTEGER                NDIM                ! Number of input data dims
      INTEGER                NELM                ! Total no of input elements
      INTEGER                NEWBAD              ! New bad points generated
      INTEGER                NREC                ! Number of history records
      INTEGER                NRNG                ! Number of ranges entered
      INTEGER                ODIMS(ADI__MXDIM)   ! Output data dimensions
      INTEGER                ONDIM               ! Number of output data dims
      INTEGER                ONELM               ! # output data items
      INTEGER                QNDIM               ! No of input quality dims
      INTEGER                OD_PTR              ! Output data pointer
      INTEGER			OFID			! Output dataset id
      INTEGER                OQ_PTR              ! Output quality pointer
      INTEGER                OV_PTR              ! Output variance pointer
      INTEGER                OW_PTR              ! Output work quality pointer
      INTEGER                TLEN                ! Length of a string
      INTEGER                T_PTR               ! Temporary pointer
      INTEGER                USE                 ! Number of history elements
      INTEGER                VNDIM               ! No of input variance dims
      INTEGER                WD_PTR              ! Workspace data
      INTEGER                WV_PTR              ! Workspace variance

      LOGICAL                ANY_BAD             ! Any bad quality input data?
      LOGICAL			AX_DUMMY(ADI__MXDIM)	! Dummy axis values?
      LOGICAL			AX_GOT_WIDS(ADI__MXDIM)	! Got axis widths?
      LOGICAL			AX_MAPPED(ADI__MXDIM)	! Axis mapped?
      LOGICAL			AX_NORM(ADI__MXDIM)	! Axis normalised?
      LOGICAL			AX_REG(ADI__MXDIM)	! Axis values regular?
      LOGICAL			AX_WID_UNIF(ADI__MXDIM)	! Axis widths uniform?
      LOGICAL                DATA_OK             ! Input data ok?
      LOGICAL                OK                  ! General validity test
      LOGICAL                PRIM                ! Was input object primitive?
      LOGICAL                QUAL_OK             ! Input quality present?
      LOGICAL                RENORMALISE         ! Renormalise output data
      LOGICAL                USE_PIXELS          ! No axis data available?
      LOGICAL                VARI_OK             ! Input variance present?
      LOGICAL                USE_WEIGHT          ! Use weights?
*
*    Version id :
*
      CHARACTER*30           VERSION
        PARAMETER            ( VERSION = 'RATIO Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Output version id
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX
      CALL AST_INIT()

*    Initialise axis structure
      DO I = 1, ADI__MXDIM
        AX_DUMMY(I) = .TRUE.
        AX_LABEL(I) = ' '
        AX_UNITS(I) = ' '
        AX_REG(I) = .FALSE.
        AX_GOT_WIDS(I) = .FALSE.
        AX_MAPPED(I) = .FALSE.
        AX_WID_UNIF(I) =.FALSE.
      END DO

*    Get input object
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
      CALL BDI_PRIM( IFID, PRIM, STATUS )

*    Check input data
      CALL BDI_CHKDATA( IFID, DATA_OK, NDIM, IDIMS, STATUS )
      IF ( DATA_OK .AND. (STATUS .EQ. SAI__OK)) THEN

*      Check not scalar
        IF ( NDIM .LT. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'RATIO cannot work on scalar input',
     :                                                   STATUS )
        ELSE
          CALL ARR_SUMDIM( NDIM, IDIMS, NELM )

*        Map the input data
          CALL BDI_MAPDATA( IFID, 'READ', ID_PTR, STATUS )

        END IF

      ELSE
        CALL MSG_PRNT( 'Numerical data required' )
        STATUS = SAI__ERROR

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find out about the input.
      IF ( PRIM ) THEN

*      Don't have to bother about this stuff
        QUAL_OK = .FALSE.
        VARI_OK = .FALSE.
        RENORMALISE = .FALSE.

      ELSE

*      See if we've got quality available
        CALL BDI_CHKQUAL( IFID, QUAL_OK, QNDIM, DIMS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( QUAL_OK ) THEN
          IF ( QNDIM .NE. NDIM ) THEN
            CALL MSG_PRNT( 'Quality and data dimensions'/
     :                                /' do not match.' )
            STATUS = SAI__ERROR

          ELSE

*          Map it and tell user
            CALL MSG_PRNT( 'Input has valid QUALITY' )
            CALL BDI_MAPLQUAL( IFID, 'READ', ANY_BAD, IQ_PTR, STATUS )

          END IF
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      See if we've got variance available
        CALL BDI_CHKVAR( IFID, VARI_OK, VNDIM, DIMS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( VARI_OK .AND. ( VNDIM .NE. NDIM )) THEN
          CALL MSG_PRNT( 'Variance and data '/
     :           /'dimensions do not match.' )
          STATUS = SAI__ERROR

        ELSE IF ( VARI_OK ) THEN

*        Use weights?
          CALL USI_GET0L( 'WEIGHT', USE_WEIGHT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Reset variance flag if no weighting
          VARI_OK = USE_WEIGHT

*        Map it and tell user
          IF ( VARI_OK ) THEN
            CALL MSG_PRNT( 'Input has valid VARIANCE' )
            CALL BDI_MAPVAR( IFID, 'READ', IV_PTR, STATUS )
          END IF

        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    If we've no quality so far, whether because the input was primitive or
*    the non-primitive input didn't have a quality array, then we have to
*    generate a dummy quality array. The reason we do this is simple. It is
*    more efficient and easier to code RATIO_INT to assume its got quality
*    there. The overheads of having one more array mapped shouldn't be too
*    high.
      IF ( .NOT. QUAL_OK ) THEN
        CALL DYN_MAPL( NDIM, IDIMS, IQ_PTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Set all the quality to good
        CALL ARR_INIT1L( .TRUE., NELM, %VAL(IQ_PTR), STATUS )

      END IF

*    Output a small table giving the relevant information. Tells the user
*    about the axes if the input isn't primitive, otherwise it just shows
*    the dimensions.
      CALL MSG_PRNT( ' ' )
      IF ( PRIM ) THEN
        CALL MSG_PRNT( 'Assuming data regularly spaced' )
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( 'Dimn     Range' )
      ELSE
        CALL MSG_PRNT( 'Axis  Label          '/
     :         /'    Units             Range' )
      END IF
      CALL MSG_PRNT( ' ' )

      DO I = 1, NDIM

        USE_PIXELS = .FALSE.

        IF ( .NOT. PRIM ) THEN

*        Try for the label and units first
          CALL BDI_GETAXLABEL( IFID, I, AX_LABEL(I), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            AX_LABEL(I) = '** Unreadable **'
            CALL ERR_FLUSH( STATUS )
          END IF
          TLEN = CHR_LEN(AX_LABEL(I))
          IF ( TLEN .LT. 1 ) THEN
            AX_LABEL(I) = 'Not Set'
          END IF
          CALL BDI_GETAXUNITS( IFID, I, AX_UNITS(I), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            AX_UNITS(I) = '** Unreadable **'
            CALL ERR_FLUSH( STATUS )
          END IF
          TLEN = CHR_LEN(AX_UNITS(I))
          IF ( TLEN .LT. 1 ) THEN
            AX_UNITS(I) = 'Not Set'
          END IF

*         Now look for axis data. If none is present we'll use the size of
*         the equivalent dimension and work in Pixels.
           DATEXT = ' '

           CALL BDI_CHKAXVAL( IFID, I, OK, AX_REG(I), TLEN, STATUS )
           IF ( STATUS .NE. SAI__OK ) THEN
             DATEXT = 'No axis data. '
             USE_PIXELS = .TRUE.
             CALL ERR_FLUSH( STATUS )

           ELSE IF ( OK ) THEN

*           Axis data is there. If its regularly spaced ( a spaced array )
*           then find the axis constants, otherwise map its data.
             IF ( AX_REG(I) ) THEN
               CALL BDI_GETAXVAL( IFID, I, AX_BASE(I), AX_SCALE(I),
     :                                               TLEN, STATUS )
             END IF
             CALL BDI_MAPAXVAL( IFID, 'READ', I, AX_PTR(I), STATUS )
             AX_MAPPED(I) = ( STATUS .EQ. SAI__OK )

             IF ( STATUS .NE. SAI__OK ) THEN
               DATEXT = 'Axis data unreadable. '
               USE_PIXELS = .TRUE.
               CALL ERR_FLUSH( STATUS )

             ELSE IF ( TLEN .NE. IDIMS(I) ) THEN
               DATEXT = 'Axis wrong size! '
               USE_PIXELS = .TRUE.

             END IF

           ELSE

*           Axis data is not valid for some reason
             DATEXT = 'Axis data invalid. '
             USE_PIXELS = .TRUE.

           END IF

*         Find normalisation. If we can't get normalisation then we assume
*         it is unnormalised.
           IF ( OK ) THEN
             CALL BDI_GETAXNORM( IFID, I, AX_NORM(I), STATUS )
             IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               AX_NORM(I) = .FALSE.
             END IF
           END IF

         ELSE

*          Primitive data...just use pixels
            USE_PIXELS = .TRUE.

         END IF

         AX_DUMMY(I) = USE_PIXELS

         IF ( USE_PIXELS ) THEN
           AX_REG(I) = .TRUE.
           AX_BASE(I) = 1.0
           AX_SCALE(I) = 1.0
           AX_UNITS(I) = 'Pixels'
         END IF

*       Output the information for the current axis
         CALL MSG_SETI( 'N', I )

         IF ( PRIM ) THEN
           CALL MSG_SETC( 'ABIT', '   From ' )

         ELSE
           CALL MSG_SETC( 'ABIT', AX_LABEL(I)(1:18)//' '//
     :                 AX_UNITS(I)(1:18)//DATEXT(1:MIN(22,
     :                         CHR_LEN(DATEXT)))//'From' )

         END IF

         IF ( AX_MAPPED(I) ) THEN
           CALL ARR_ELEM1R( AX_PTR(I), IDIMS(I), 1, AX_PX1(I), STATUS )
           CALL ARR_ELEM1R( AX_PTR(I), IDIMS(I), IDIMS(I), AX_PXN(I),
     :                                                       STATUS )
         ELSE
           AX_PX1(I) = AX_BASE(I)
           AX_PXN(I) = AX_BASE(I) + (IDIMS(I)-1)*AX_SCALE(I)
         END IF
         CALL MSG_SETR( 'LOW', AX_PX1(I) )
         CALL MSG_SETR( 'HIGH', AX_PXN(I) )
         CALL MSG_PRNT( '  ^N   ^ABIT ^LOW to ^HIGH' )

        IF ( (AX_UNITS(I) .EQ. 'Not Set') .OR. (AX_UNITS(I) .EQ.
     :                                  '** Unreadable **')) THEN
          AX_UNITS(I) = 'Pixels'
        END IF
      END DO

*    End of axis table production
      CALL MSG_PRNT( ' ' )

*    Pick the axis to ratio along and put in AXIS
      IF ( NDIM .EQ. 1 ) THEN
        AXIS = 1
      ELSE
        CALL USI_GET0I( 'AXIS', AXIS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF (( AXIS.LT.1 ) .OR. ( AXIS.GT.NDIM )) THEN
          CALL MSG_PRNT( 'Axis must be between 1 and ^AX' )
          STATUS = SAI__ERROR
          GOTO 99
        END IF
      END IF

*    Transform input dimensions
      DO I = NDIM+1, ADI__MXDIM
        IDIMS(I) = 1
      END DO

*    Give user an idea what is going to happen
      CALL MSG_PRNT( 'RATIO will divide two ranges on this '
     :                 //'axis - the second by the first.' )
      CALL MSG_PRNT( ' ' )

*    Get two bands in selected axis
      CALL PRS_GETRANGES( 'BANDS', 4, 1, AX_PX1(AXIS), AX_PXN(AXIS),
     :                                         BANDS, NRNG, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( NRNG .NE. 2 ) THEN
          CALL MSG_PRNT( 'Two ranges are required...' )
          STATUS = SAI__ERROR
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Normalise output?
      CALL USI_DEF0L( 'NORMAL', .TRUE., STATUS )
      CALL USI_GET0L( 'NORMAL', RENORMALISE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    We need axis widths if either the selected axis is normalised, or if
*    output is going to be normalised
      IF ( AX_NORM(AXIS) .OR. RENORMALISE ) THEN

*      Get axis widths if present
        IF ( AX_MAPPED(AXIS) ) THEN
          CALL BDI_CHKAXWID( IFID, AXIS, OK, AX_WID_UNIF(AXIS),
     :                                          DUMMY, STATUS )
          IF ( OK ) THEN
            CALL BDI_MAPAXWID( IFID, 'READ', AXIS, AX_WPTR(AXIS),
     :                                                   STATUS )
          END IF
        ELSE
          OK = .FALSE.
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
          OK = .FALSE.
        END IF

*      Otherwise invent them
        IF ( .NOT. OK ) THEN
          CALL DYN_MAPR( 1, IDIMS(AXIS), AX_WPTR(AXIS), STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
          IF ( AX_REG(AXIS) ) THEN
            CALL ARR_INIT1R( ABS(AX_SCALE(AXIS)), IDIMS(AXIS),
     :                        %VAL(AX_WPTR(AXIS)), STATUS )
          ELSE
            CALL BDA_MAPAXWID_INVENT( %VAL(AX_PTR(AXIS)),
     :            IDIMS(AXIS), %VAL(AX_WPTR(AXIS)), STATUS )
          END IF

        END IF
        AX_GOT_WIDS(AXIS) = .TRUE.

      END IF

*    Denormalise axis to be RATIOed
      IF ( AX_NORM(AXIS) ) THEN

*      Create dynamic data array copy
        CALL DYN_MAPR( ADI__MXDIM, IDIMS, T_PTR, STATUS )
        CALL ARR_COP1R( NELM, %VAL(ID_PTR), %VAL(T_PTR), STATUS )
        ID_PTR = T_PTR
        CALL BDI_UNMAPDATA( IFID, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Denormalise it
        CALL AR7_DENORM( %VAL(AX_WPTR(AXIS)), IDIMS, AXIS,
     :                              %VAL(ID_PTR), STATUS )

*      Variance present?
        IF ( VARI_OK ) THEN

*        Create dynamic copy
          CALL DYN_MAPR( ADI__MXDIM, IDIMS, T_PTR, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
          CALL ARR_COP1R( NELM, %VAL(IV_PTR), %VAL(T_PTR), STATUS )
          IV_PTR = T_PTR
          CALL BDI_UNMAPVAR( IFID, STATUS )

*        Denormalise it
          CALL AR7_DENORMV( %VAL(AX_WPTR(AXIS)), IDIMS, AXIS,
     :                                 %VAL(IV_PTR), STATUS )

        END IF

      END IF

*    Find axis array indices corresponding to the range values given
      CALL AXIS_VAL2PIX( IDIMS(AXIS), %VAL(AX_PTR(AXIS)), .FALSE.,
     :                   BANDS(1), BANDS(2), LOW1, HIGH1, STATUS )
      CALL AXIS_VAL2PIX( IDIMS(AXIS), %VAL(AX_PTR(AXIS)), .FALSE.,
     :                   BANDS(3), BANDS(4), LOW2, HIGH2, STATUS )

*    Unmap axis data if used
      IF ( (.NOT. PRIM) .AND. AX_MAPPED(AXIS) ) THEN
        CALL BDI_UNMAPAXVAL( IFID, AXIS, STATUS )
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      END IF

*    If renormalising, we need the total width of each band. This width is
*    from the lower bound of the left-most bin to upper bound of the
*    rightmost bin
      IF ( RENORMALISE ) THEN

*      First band
        CALL ARR_ELEM1R( AX_PTR(AXIS), IDIMS(AXIS), LOW1, LHC, STATUS )
        CALL ARR_ELEM1R( AX_PTR(AXIS), IDIMS(AXIS), HIGH1, RHC, STATUS )
        CALL ARR_ELEM1R( AX_WPTR(AXIS), IDIMS(AXIS), LOW1, LHW, STATUS )
        CALL ARR_ELEM1R( AX_WPTR(AXIS), IDIMS(AXIS), HIGH1, RHW,STATUS )
        IF ( RHC .GT. LHC ) THEN
          WIDTH1 = (RHC+RHW/2.0) - (LHC-LHW/2.0)
        ELSE
          WIDTH1 = (LHC+LHW/2.0) - (RHC-RHW/2.0)
        END IF

*      Second band
        CALL ARR_ELEM1R( AX_PTR(AXIS), IDIMS(AXIS), LOW2, LHC, STATUS )
        CALL ARR_ELEM1R( AX_PTR(AXIS), IDIMS(AXIS), HIGH2, RHC, STATUS )
        CALL ARR_ELEM1R( AX_WPTR(AXIS), IDIMS(AXIS), LOW2, LHW, STATUS )
        CALL ARR_ELEM1R( AX_WPTR(AXIS), IDIMS(AXIS), HIGH2, RHW,STATUS )
        IF ( RHC .GT. LHC ) THEN
          WIDTH2 = (RHC+RHW/2.0) - (LHC-LHW/2.0)
        ELSE
          WIDTH2 = (LHC+LHW/2.0) - (RHC-RHW/2.0)
        END IF

      ELSE
        WIDTH1 = 1.0
        WIDTH2 = 1.0

      END IF

*    Create output dimensions. In the case of a one-dimensional input we
*    degenerate into a scalar. This is dealt with as a 1D array of unit length
      IF ( NDIM .EQ. 1 ) THEN
        ONDIM = 1
        ODIMS(1) = 1
      ELSE
        ONDIM = NDIM - 1
        I = 1
        J = 1
        DO WHILE ( I .LE. NDIM )
          IF ( I .NE. AXIS ) THEN
            ODIMS(J) = IDIMS(I)
            J = J + 1
          END IF
          I = I + 1
        END DO
      END IF

*    Transform these into the 7D system
      DO I = ONDIM+1, ADI__MXDIM
        ODIMS(I) = 1
      END DO
      CALL ARR_SUMDIM( ONDIM, ODIMS, ONELM )

*    Create components in output
      CALL BDI_CREDATA( OFID, ONDIM, ODIMS, STATUS )
      CALL BDI_CREQUAL( OFID, ONDIM, ODIMS, STATUS )
      CALL BDI_CREAXES( OFID, ONDIM, STATUS )
      IF ( VARI_OK ) THEN
         CALL BDI_CREVAR( OFID, ONDIM, ODIMS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Output label
      CALL BDI_PUTLABEL( OFID, 'Relative intensity', STATUS )

*    Map these output objects
      CALL BDI_MAPDATA( OFID, 'WRITE', OD_PTR, STATUS )
      CALL BDI_MAPQUAL( OFID, 'WRITE', OQ_PTR, STATUS )
      IF ( VARI_OK ) THEN
        CALL BDI_MAPVAR( OFID, 'WRITE', OV_PTR, STATUS )
      ELSE
        CALL DYN_MAPR( ONDIM, ODIMS, OV_PTR, STATUS )
        CALL ARR_INIT1R( 0.0, ONELM, %VAL(OV_PTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Grab workspace required by RATIO_INT. These are WD_ and WV_ the work
*    space data and variance arrays, and OW_ which is an extra quality array
*    needed by RATIO_INT.
      CALL DYN_MAPR( ONDIM, ODIMS, WD_PTR, STATUS )
      CALL DYN_MAPR( ONDIM, ODIMS, WV_PTR, STATUS )
      CALL DYN_MAPR( ONDIM, ODIMS, OW_PTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Do the ratioing
      CALL RATIO_INT( IDIMS(1), IDIMS(2), IDIMS(3), IDIMS(4),
     :                IDIMS(5), IDIMS(6), IDIMS(7), %VAL(ID_PTR),
     :                %VAL(IQ_PTR), VARI_OK, %VAL(IV_PTR),
     :                LOW1, HIGH1, WIDTH1, LOW2, HIGH2, WIDTH2, AXIS,
     :                ODIMS(1), ODIMS(2),
     :                  ODIMS(3), ODIMS(4), ODIMS(5), ODIMS(6),
     :                         ODIMS(7), %VAL(WD_PTR),
     :                  %VAL(WV_PTR), %VAL(OW_PTR), %VAL(OD_PTR),
     :               %VAL(OQ_PTR), %VAL(OV_PTR), NEWBAD, STATUS )

*    There could still be a considerable amount of processing still to do
*    so we unmap all the work data, the input data and the output quality.
      CALL DYN_UNMAP( WD_PTR, STATUS )
      CALL DYN_UNMAP( WV_PTR, STATUS )
      CALL DYN_UNMAP( OW_PTR, STATUS )

      CALL BDI_UNMAPDATA( IFID, STATUS )
      CALL BDI_UNMAPQUAL( OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Inform the user if any new bad quality points were generated
      IF ( NEWBAD .NE. 0 ) THEN
        CALL MSG_SETI( 'NB', NEWBAD )
        CALL MSG_PRNT( '^NB bad quality points were generated'/
     :                                           /' by RATIO' )
        IF ( NDIM .EQ. 1 ) THEN
          CALL MSG_PRNT( 'RATIO aborting...' )
          GOTO 99
        END IF
      END IF

*    Copy axis information
      IF ( ( NDIM .GT. 1 ) .AND. .NOT. PRIM ) THEN
        I = 1
        J = 1
        DO WHILE ( (I .LE. NDIM) .AND. (STATUS .EQ. SAI__OK) )
          IF ( I .NE. AXIS ) THEN
            IF ( AX_DUMMY(I) ) THEN

*            Could create output axes for primitive input?

            ELSE
              CALL BDI_COPAXIS( IFID, OFID, I, J, STATUS )
              CALL BDI_PUTAXNORM( OFID, J, RENORMALISE, STATUS )

            END IF
            J = J + 1

          END IF
          I = I + 1
        END DO

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy MORE box unless primitive
      IF ( .NOT. PRIM ) CALL BDI_COPMORE( IFID, OFID, STATUS )

*    If output is scalar tell user about it
      IF ( NDIM .EQ. 1 ) THEN
        CALL MSG_SETR( 'VAL', %VAL(OD_PTR) )
        CALL ARR_COP1R( 1, %VAL(OV_PTR), OVAR, STATUS )
        CALL MSG_SETR( 'ERR', SQRT(OVAR) )
        IF ( RENORMALISE ) THEN
          CALL MSG_PRNT( 'Normalised ratio is ^VAL +- ^ERR' )
        ELSE
          CALL MSG_PRNT( 'Unnormalised ratio is ^VAL +- ^ERR' )
        END IF
      END IF

*    Create HISTORY structure. Tell user input file, axis selected and the
*    bounds on the axis use to ratio.
      TEXT(1) = 'Input dataset {INP}'
      USE=2

*    Describe ratio axis
      IF ( (.NOT. (PRIM.OR.AX_DUMMY(AXIS))) .AND.
     :     CHR_SIMLR( AX_LABEL(AXIS), 'NOT SET' ) .AND.
     :     CHR_SIMLR( AX_LABEL(AXIS), '** UNREADABLE **' ) ) THEN
        CALL MSG_SETC( 'AXIS', AX_LABEL(AXIS) )
      ELSE
        CALL MSG_SETI( 'AXIS', AXIS )
      END IF
      CALL MSG_MAKE( 'Ratio on axis ^AXIS', TEXT(USE), TLEN )
      USE = USE + 1

*    Describe ratio bands
      CALL MSG_SETR( 'LOW1', BANDS(1) )
      CALL MSG_SETR( 'HIGH1', BANDS(2) )
      CALL MSG_SETC( 'UNITS', AX_UNITS(AXIS) )
      CALL MSG_MAKE( 'Lower range ^LOW1 to ^HIGH1 ^UNITS', TEXT(USE),
     :                                                         TLEN )
      USE = USE + 1
      CALL MSG_SETC( 'UNITS', AX_UNITS(AXIS) )
      CALL MSG_SETR( 'LOW2', BANDS(3) )
      CALL MSG_SETR( 'HIGH2', BANDS(4) )
      CALL MSG_MAKE( 'Upper range ^LOW2 to ^HIGH2 ^UNITS', TEXT(USE),
     :                                                         TLEN )
      USE = USE + 1

      IF ( RENORMALISE ) THEN
        TEXT(USE) = 'Ratio data normalised wrt selected axis'
      ELSE
        TEXT(USE) = 'Ratio data not normalised wrt selected axis'
      END IF

*    Copy history
      CALL HSI_COPY( IFID, OFID, STATUS )

*    Add History records
      CALL HSI_ADD( OFID, VERSION, STATUS )

*    Process text
      NREC = MAXLINES
      CALL USI_TEXT( USE, TEXT, NREC, STATUS )
      CALL HSI_PTXT( OFID, USE, TEXT, STATUS )

*    Release datasets
      CALL BDI_RELEASE( IFID, STATUS )
      CALL BDI_RELEASE( OFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END




*+  RATIO_INT - Peform ratioing on 7D data sets
      SUBROUTINE RATIO_INT( L1,L2,L3,L4,L5,L6,L7,I_D, I_Q, VFLAG,
     :             I_V, LOW1, HIGH1, WIDTH1, LOW2, HIGH2,
     :             WIDTH2, AXIS, M1,M2,M3,
     :       M4,M5,M6,M7,W_D, W_V, O_W, O_D, O_Q, O_V, NBAD, STATUS )
*
*    Description :
*
*     Accumulates data from two subranges of an axis of a 7D data array and
*     divides one by the other to leave a 6D data array ( but still in a 7D
*     form ).
*
*    Method:
*
*     Fig 1:      <----------------N6----------------->
*                 +-----------------------------------+  ^
*         Row1->  |   |abcdef|            |ghij|      |  |
*                 |   |      |            |    |      |  N7
*                 |   |      |            |    |      |  |
*                 +-----------------------------------+  v
*                    /      /            /    /
*                  LOW1   HIGH1        LOW2  HIGH2
*
*     Fig 2:          +-+                 +-+         +-+
*                     |X|                 |Y|   Y/X   |Z|
*                     | |                 | |    =>   | |
*                     | |                 | |         | |
*                     +-+                 +-+         +-+
*
*     Consider Fig (1) above. This the setup if a 2D array was being
*     ratioed along its first dimension. The data values in the two ranges
*     in ROW1 are summed, ie. ( a+b+...+f ) and ( g+...+j )  to produce
*     elements X and Y in two arrays of dimensionality one less than the
*     input ( Fig 2). Then these arrays are divided to produce the final
*     output.
*
*    Author :
*
*     David Allan (BHVAD::DJA)
*
*    History :
*
*      8 Sep 88 : Original (DJA)
*      9 Aug 93 : Use PRIM constants for extreme data values (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER                STATUS              ! Run-time error code
*
*    Import :
*
      INTEGER                AXIS                ! Axis to ratio on
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      REAL                   WIDTH1              ! First range total width
      INTEGER                LOW2,HIGH2          ! Bounds on second range
      REAL                   WIDTH2              ! Second range total width

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qual
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input vari
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M6,M7!
      LOGICAL                O_W(M1,M2,M3,M4,M5,M6,M7) ! Work space
      BYTE                   O_Q(M1,M2,M3,M4,M5,M6,M7) ! Output quality
      REAL                   O_D(M1,M2,M3,M4,M5,M6,M7) ! Output data
      REAL                   O_V(M1,M2,M3,M4,M5,M6,M7) ! Output variance
      REAL                   W_D(M1,M2,M3,M4,M5,M6,M7) ! Work data
      REAL                   W_V(M1,M2,M3,M4,M5,M6,M7) ! Work variance

      INTEGER                NBAD                ! No of new bad points made
*
*    Local constants :
*
      REAL                   TINY_REAL           ! Smallest +ve real number
        PARAMETER            (TINY_REAL=VAL__SMLR)
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
      INTEGER                NELM

      REAL                   WD                  ! Temp element of W_D
      REAL                   WV                  ! Temp element of W_V
      REAL                   OD                  ! Temp element of O_D
      REAL                   OV                  ! Temp element of O_V
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find number of output elements
      NELM = M1*M2*M3*M4*M5*M6*M7

*    Initialise output arrays
      CALL ARR_INIT1R( 0.0, NELM, W_D, STATUS )
      CALL ARR_INIT1R( 0.0, NELM, O_D, STATUS )
      CALL ARR_INIT1B( QUAL__GOOD, NELM, O_Q, STATUS )
      CALL ARR_INIT1L( .FALSE., NELM, O_W, STATUS )     ! Bad quality
      IF ( VFLAG ) THEN
        CALL ARR_INIT1R( 0.0, NELM, W_V, STATUS )
        CALL ARR_INIT1R( 0.0, NELM, O_V, STATUS )
      END IF

*    Set bad points to zero
      NBAD = 0

*    Accumulate the data from I_D into W_D and O_D
      IF ( AXIS .EQ. 7 ) THEN
        CALL RATIO_SEL_7( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                   LOW1, HIGH1, LOW2, HIGH2, M1,M2,M3,M4,M5,M6,
     :                                      W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 6 ) THEN
        CALL RATIO_SEL_6( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M1,M2,M3,M4,M5,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 5 ) THEN
        CALL RATIO_SEL_5( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M1,M2,M3,M4,M6,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 4 ) THEN
        CALL RATIO_SEL_4( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M1,M2,M3,M5,M6,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 3 ) THEN
        CALL RATIO_SEL_3( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M1,M2,M4,M5,M6,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 2 ) THEN
        CALL RATIO_SEL_2( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M1,M3,M4,M5,M6,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      ELSE IF ( AXIS .EQ. 1 ) THEN
        CALL RATIO_SEL_1( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q, VFLAG,
     :                      LOW1, HIGH1, LOW2, HIGH2, M2,M3,M4,M5,M6,
     :                                  M7, W_D, W_V, O_D, O_W, O_V )

      END IF

*    Divide the O_ data array by the W_ data array, taking renormalisation
*    into account if necessary
      DO O = 1, M7
        DO N = 1, M6
          DO M = 1, M5
            DO L = 1, M4
              DO K = 1, M3
                DO J = 1, M2
                  DO I = 1, M1

*                  Test whether all the points which contributed to this bin
*                  had bad quality.
                    IF ( .NOT.  O_W(I,J,K,L,M,N,O) ) THEN

*                    They did so the output quality must be bad
                      O_Q(I,J,K,L,M,N,O) = QUAL__MISSING
                      NBAD = NBAD + 1

                    ELSE

*                    Get data values into temps to save access time. Take
*                    this opportunity to renormalise.
                      WD = W_D(I,J,K,L,M,N,O) / WIDTH1
                      WV = W_V(I,J,K,L,M,N,O) / WIDTH1**2
                      OD = O_D(I,J,K,L,M,N,O) / WIDTH2
                      OV = O_V(I,J,K,L,M,N,O) / WIDTH2**2

*                    Test for over-flow. If true set output quality bad
                      IF (ABS(WD) .LE. ABS(TINY_REAL*OD)) THEN
                        O_Q(I,J,K,L,M,N,O) = QUAL__ARITH
                        NBAD = NBAD + 1

                      ELSE

*                      Output data
                        O_D(I,J,K,L,M,N,O) = OD / WD

*                      Variances do v3 = v1+v2+2*sqrt(v1*v2) under division
                        IF ( VFLAG ) THEN
                          O_V(I,J,K,L,M,N,O) = OV + WV +
     :                                           2.0*SQRT(WV*OV)
                        END IF

                      END IF

                    END IF

                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END




*+ RATIO_SEL_7 - Select values from array in 2 ranges along its seventh dimension
      SUBROUTINE RATIO_SEL_7( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M2,M3,M4,M5,M6, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the seventh axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M6   !
      LOGICAL                O_W(M1,M2,M3,M4,M5,M6) ! Work space
      REAL                   O_D(M1,M2,M3,M4,M5,M6) ! Output data
      REAL                   O_V(M1,M2,M3,M4,M5,M6) ! Output variance
      REAL                   W_D(M1,M2,M3,M4,M5,M6) ! Work data
      REAL                   W_V(M1,M2,M3,M4,M5,M6) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 7 check on index O - output to (I,J,K,L,M,N)
      DO N = 1, L6
        DO M = 1, L5
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO O = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,M,N) = W_D(I,J,K,L,M,N) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,M,N) = W_V(I,J,K,L,M,N) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,M,N) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO O = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,M,N) = W_D(I,J,K,L,M,N) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,M,N) = W_V(I,J,K,L,M,N) + 1.0
                        O_W(I,J,K,L,M,N) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,J,K,L,M,N) ) THEN
                    W_D(I,J,K,L,M,N) = W_D(I,J,K,L,M,N) /
     :                                      W_V(I,J,K,L,M,N)
                    W_V(I,J,K,L,M,N) = 1.0 / W_V(I,J,K,L,M,N)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO O = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,M,N) = O_D(I,J,K,L,M,N) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,M,N) = O_V(I,J,K,L,M,N) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,M,N) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO O = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,M,N) = O_D(I,J,K,L,M,N) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,M,N) = O_V(I,J,K,L,M,N) + 1.0
                        O_W(I,J,K,L,M,N) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,J,K,L,M,N) ) THEN
                    O_D(I,J,K,L,M,N) = O_D(I,J,K,L,M,N) /
     :                                        O_V(I,J,K,L,M,N)
                    O_V(I,J,K,L,M,N) = 1.0 / O_V(I,J,K,L,M,N)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ RATIO_SEL_6 - Select values from array in 2 ranges along its sixth dimension
      SUBROUTINE RATIO_SEL_6( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M2,M3,M4,M5,M7, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the sixth axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M5,M7   !
      LOGICAL                O_W(M1,M2,M3,M4,M5,M7) ! Work space
      REAL                   O_D(M1,M2,M3,M4,M5,M7) ! Output data
      REAL                   O_V(M1,M2,M3,M4,M5,M7) ! Output variance
      REAL                   W_D(M1,M2,M3,M4,M5,M7) ! Work data
      REAL                   W_V(M1,M2,M3,M4,M5,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 6 check on index N - output to (I,J,K,L,M,O)
      DO O = 1, L7
        DO M = 1, L5
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO N = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,M,O) = W_D(I,J,K,L,M,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,M,O) = W_V(I,J,K,L,M,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,M,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO N = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,M,O) = W_D(I,J,K,L,M,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,M,O) = W_V(I,J,K,L,M,O) + 1.0
                        O_W(I,J,K,L,M,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,J,K,L,M,O) ) THEN
                    W_D(I,J,K,L,M,O) = W_D(I,J,K,L,M,O) /
     :                                      W_V(I,J,K,L,M,O)
                    W_V(I,J,K,L,M,O) = 1.0 / W_V(I,J,K,L,M,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO N = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,M,O) = O_D(I,J,K,L,M,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,M,O) = O_V(I,J,K,L,M,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,M,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO N = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,M,O) = O_D(I,J,K,L,M,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,M,O) = O_V(I,J,K,L,M,O) + 1.0
                        O_W(I,J,K,L,M,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,J,K,L,M,O) ) THEN
                    O_D(I,J,K,L,M,O) = O_D(I,J,K,L,M,O) /
     :                                        O_V(I,J,K,L,M,O)
                    O_V(I,J,K,L,M,O) = 1.0 / O_V(I,J,K,L,M,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ RATIO_SEL_5 - Select values from array in 2 ranges along its fifth dimension
      SUBROUTINE RATIO_SEL_5( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M2,M3,M4,M6,M7, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the fifth axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M2,M3,M4,M6,M7!
      LOGICAL                O_W(M1,M2,M3,M4,M6,M7) ! Work space
      REAL                   O_D(M1,M2,M3,M4,M6,M7) ! Output data
      REAL                   O_V(M1,M2,M3,M4,M6,M7) ! Output variance
      REAL                   W_D(M1,M2,M3,M4,M6,M7) ! Work data
      REAL                   W_V(M1,M2,M3,M4,M6,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 5 check on index M - output to (I,J,K,L,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO L = 1, L4
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO M = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,N,O) = W_D(I,J,K,L,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,N,O) = W_V(I,J,K,L,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,N,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO M = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,L,N,O) = W_D(I,J,K,L,N,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,J,K,L,N,O) = W_V(I,J,K,L,N,O) + 1.0
                        O_W(I,J,K,L,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,J,K,L,N,O) ) THEN
                    W_D(I,J,K,L,N,O) = W_D(I,J,K,L,N,O)/
     :                                       W_V(I,J,K,L,N,O)
                    W_V(I,J,K,L,N,O) = 1.0 / W_V(I,J,K,L,N,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO M = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,N,O) = O_D(I,J,K,L,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,N,O) = O_V(I,J,K,L,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,L,N,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO M = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,L,N,O) = O_D(I,J,K,L,N,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,J,K,L,N,O) = O_V(I,J,K,L,N,O) + 1.0
                        O_W(I,J,K,L,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,J,K,L,N,O) ) THEN
                    O_D(I,J,K,L,N,O) = O_D(I,J,K,L,N,O) /
     :                                        O_V(I,J,K,L,N,O)
                    O_V(I,J,K,L,N,O) = 1.0 / O_V(I,J,K,L,N,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ RATIO_SEL_4 - Select values from array in 2 ranges along its fourth dimension
      SUBROUTINE RATIO_SEL_4( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M2,M3,M5,M6,M7, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the fourth axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M2,M3,M5,M6,M7!
      LOGICAL                O_W(M1,M2,M3,M5,M6,M7) ! Work space
      REAL                   O_D(M1,M2,M3,M5,M6,M7) ! Output data
      REAL                   O_V(M1,M2,M3,M5,M6,M7) ! Output variance
      REAL                   W_D(M1,M2,M3,M5,M6,M7) ! Work data
      REAL                   W_V(M1,M2,M3,M5,M6,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 4 check on index L - output to (I,J,K,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO K = 1, L3
              DO J = 1, L2
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO L = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,M,N,O) = W_D(I,J,K,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,J,K,M,N,O) = W_V(I,J,K,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,M,N,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO L = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,K,M,N,O) = W_D(I,J,K,M,N,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,J,K,M,N,O) = W_V(I,J,K,M,N,O) + 1.0
                        O_W(I,J,K,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,J,K,M,N,O) ) THEN
                    W_D(I,J,K,M,N,O) = W_D(I,J,K,M,N,O)/
     :                                       W_V(I,J,K,M,N,O)
                    W_V(I,J,K,M,N,O) = 1.0 / W_V(I,J,K,M,N,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO L = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,M,N,O) = O_D(I,J,K,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,J,K,M,N,O) = O_V(I,J,K,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,K,M,N,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO L = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,K,M,N,O) = O_D(I,J,K,M,N,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,J,K,M,N,O) = O_V(I,J,K,M,N,O) + 1.0
                        O_W(I,J,K,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,J,K,M,N,O) ) THEN
                    O_D(I,J,K,M,N,O) = O_D(I,J,K,M,N,O) /
     :                                        O_V(I,J,K,M,N,O)
                    O_V(I,J,K,M,N,O) = 1.0 / O_V(I,J,K,M,N,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ RATIO_SEL_3 - Select values from array in 2 ranges along its third dimension
      SUBROUTINE RATIO_SEL_3( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M2,M4,M5,M6,M7, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the third axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance
      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M2,M4,M5,M6,M7!
      LOGICAL                O_W(M1,M2,M4,M5,M6,M7) ! Work space
      REAL                   O_D(M1,M2,M4,M5,M6,M7) ! Output data
      REAL                   O_V(M1,M2,M4,M5,M6,M7) ! Output variance
      REAL                   W_D(M1,M2,M4,M5,M6,M7) ! Work data
      REAL                   W_V(M1,M2,M4,M5,M6,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 3 check on index K - output to (I,J,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO J = 1, L2
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO K = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,L,M,N,O) = W_D(I,J,L,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,J,L,M,N,O) = W_V(I,J,L,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO K = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,J,L,M,N,O) = W_D(I,J,L,M,N,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,J,L,M,N,O) = W_V(I,J,L,M,N,O) + 1.0
                        O_W(I,J,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,J,L,M,N,O) ) THEN
                    W_D(I,J,L,M,N,O) = W_D(I,J,L,M,N,O)/
     :                                  W_V(I,J,L,M,N,O)
                    W_V(I,J,L,M,N,O) = 1.0 / W_V(I,J,L,M,N,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO K = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,L,M,N,O) = O_D(I,J,L,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,J,L,M,N,O) = O_V(I,J,L,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,J,L,M,N,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO K = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,J,L,M,N,O) = O_D(I,J,L,M,N,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,J,L,M,N,O) = O_V(I,J,L,M,N,O) + 1.0
                        O_W(I,J,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,J,L,M,N,O) ) THEN
                    O_D(I,J,L,M,N,O) = O_D(I,J,L,M,N,O) /
     :                                        O_V(I,J,L,M,N,O)
                    O_V(I,J,L,M,N,O) = 1.0 / O_V(I,J,L,M,N,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+ RATIO_SEL_2 - Select values from array in 2 ranges along its second dimension
      SUBROUTINE RATIO_SEL_2( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M1,
     :                   M3,M4,M5,M6,M7, W_D, W_V, O_D, O_W, O_V )
*
*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the second axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M1,M3,M4,M5,M6,M7!
      LOGICAL                O_W(M1,M3,M4,M5,M6,M7) ! Work space
      REAL                   O_D(M1,M3,M4,M5,M6,M7) ! Output data
      REAL                   O_V(M1,M3,M4,M5,M6,M7) ! Output variance
      REAL                   W_D(M1,M3,M4,M5,M6,M7) ! Work data
      REAL                   W_V(M1,M3,M4,M5,M6,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 2 check on index J - output to (I,K,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO K = 1, L3
                DO I = 1, L1

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO J = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,K,L,M,N,O) = W_D(I,K,L,M,N,O) +
     :                     I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(I,K,L,M,N,O) = W_V(I,K,L,M,N,O) + 1.0/
     :                                         I_V(I,J,K,L,M,N,O)
                        O_W(I,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO J = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(I,K,L,M,N,O) = W_D(I,K,L,M,N,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(I,K,L,M,N,O) = W_V(I,K,L,M,N,O) + 1.0
                        O_W(I,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(I,K,L,M,N,O) ) THEN
                    W_D(I,K,L,M,N,O) = W_D(I,K,L,M,N,O)/
     :                                       W_V(I,K,L,M,N,O)
                    W_V(I,K,L,M,N,O) = 1.0 / W_V(I,K,L,M,N,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO J = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,K,L,M,N,O) = O_D(I,K,L,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(I,K,L,M,N,O) = O_V(I,K,L,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(I,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO J = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(I,K,L,M,N,O) = O_D(I,K,L,M,N,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(I,K,L,M,N,O) = O_V(I,K,L,M,N,O) + 1.0
                        O_W(I,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(I,K,L,M,N,O) ) THEN
                    O_D(I,K,L,M,N,O) = O_D(I,K,L,M,N,O) /
     :                                        O_V(I,K,L,M,N,O)
                    O_V(I,K,L,M,N,O) = 1.0 / O_V(I,K,L,M,N,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END




*+ RATIO_SEL_1 - Select values from array in 2 ranges along its first dimension
      SUBROUTINE RATIO_SEL_1( L1,L2,L3,L4,L5,L6,L7, I_D, I_V, I_Q,
     :                        VFLAG, LOW1, HIGH1, LOW2, HIGH2, M2,
     :                   M3,M4,M5,M6,M7, W_D, W_V, O_D, O_W, O_V )

*    Description :
*
*     Generates two six dimensional output data arrays by adding all the values
*     in two specified regions of the first axis of a seven dimensional array.
*     Additional quality and variance arrays may be filled.
*
*    History :
*
*     26 Sep 88 : Original (BHVAD::DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Input :
*
      INTEGER                LOW1,HIGH1          ! Bounds on first range
      INTEGER                LOW2,HIGH2          ! Bounds on second range

      LOGICAL                VFLAG               ! Use variance

      INTEGER                L1,L2,L3,L4,L5,L6,L7!
      LOGICAL                I_Q(L1,L2,L3,L4,L5,L6,L7) ! Input qualITY
      REAL                   I_D(L1,L2,L3,L4,L5,L6,L7) ! Input data
      REAL                   I_V(L1,L2,L3,L4,L5,L6,L7) ! Input variance
*
*    Export :
*
      INTEGER                M2,M3,M4,M5,M6,M7   !
      LOGICAL                O_W(M2,M3,M4,M5,M6,M7) ! Work space
      REAL                   O_D(M2,M3,M4,M5,M6,M7) ! Output data
      REAL                   O_V(M2,M3,M4,M5,M6,M7) ! Output variance
      REAL                   W_D(M2,M3,M4,M5,M6,M7) ! Work data
      REAL                   W_V(M2,M3,M4,M5,M6,M7) ! Work variance
*
*    Local variables :
*
      INTEGER                I,J,K,L,M,N,O       ! Axis loop indices
*-

*    For axis 1 check on index I - output to (J,K,L,M,N,O)
      DO O = 1, L7
        DO N = 1, L6
          DO M = 1, L5
            DO L = 1, L4
              DO K = 1, L3
                DO J = 1, L2

*                Sum data values in first range
                  IF ( VFLAG ) THEN
                    DO I = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(J,K,L,M,N,O) = W_D(J,K,L,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        W_V(J,K,L,M,N,O) = W_V(J,K,L,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(J,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  ELSE
                    DO I = LOW1, HIGH1
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        W_D(J,K,L,M,N,O) = W_D(J,K,L,M,N,O) +
     :                                         I_D(I,J,K,L,M,N,O)
                        W_V(J,K,L,M,N,O) = W_V(J,K,L,M,N,O) + 1.0
                        O_W(J,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

*                Evaluate denominator band sum
                  IF ( O_W(J,K,L,M,N,O) ) THEN
                    W_D(J,K,L,M,N,O) = W_D(J,K,L,M,N,O)/
     :                                         W_V(J,K,L,M,N,O)
                    W_V(J,K,L,M,N,O) = 1.0 / W_V(J,K,L,M,N,O)
                  ELSE
                    GOTO 50
                  END IF

*                Sum data values in second range
                  IF ( VFLAG ) THEN
                    DO I = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(J,K,L,M,N,O) = O_D(J,K,L,M,N,O) +
     :                         I_D(I,J,K,L,M,N,O)/I_V(I,J,K,L,M,N,O)
                        O_V(J,K,L,M,N,O) = O_V(J,K,L,M,N,O) + 1.0/
     :                                             I_V(I,J,K,L,M,N,O)
                        O_W(J,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO

                  ELSE
                    DO I = LOW2, HIGH2
                      IF ( I_Q(I,J,K,L,M,N,O) ) THEN
                        O_D(J,K,L,M,N,O) = O_D(J,K,L,M,N,O) +
     :                                      I_D(I,J,K,L,M,N,O)
                        O_V(J,K,L,M,N,O) = O_V(J,K,L,M,N,O) + 1.0
                        O_W(J,K,L,M,N,O) = .TRUE.
                      END IF
                    END DO
                  END IF

 50               IF ( O_W(J,K,L,M,N,O) ) THEN
                    O_D(J,K,L,M,N,O) = O_D(J,K,L,M,N,O) /
     :                                        O_V(J,K,L,M,N,O)
                    O_V(J,K,L,M,N,O) = 1.0 / O_V(J,K,L,M,N,O)
                  END IF

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END
