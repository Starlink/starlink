*+  MEDIAN - smooth a 2-D image using a 2-D weighted median filter
      SUBROUTINE MEDIAN( STATUS )
*    Description :
*     Filters the input image, %INPIC, with a Weighted Median Filter ( WMF ) of
*     a type defined by the user, %MODE = -1, or selected from some predefined
*     types, %MODE = 0 to 7. If the WMF is to be user defined then the
*     parameters for the weighting function, %CORNER, %SIDE and %CENTRE, will
*     be requested. A stepsize, %STEP, has to be specified, this determines
*     the spacing of the elements of the weighting function.
*     The weighting function has the form :
*
*               %CORNER  .   %SIDE   .  %CORNER
*                  .           .           .
*                %SIDE   .  %CENTRE  .   %SIDE
*                  .           .           .
*               %CORNER  .   %SIDE   .  %CORNER
*
*     The . indicates that the weights are separated by (%STEP-1) zeros.
*     A threshold, %DIFF, for replacement of a value by the median can be set.
*     If the absolute value of the difference between the actual value and the
*     median is less than %DIFF the replacement will not occur. The way in
*     which the image boundary is dealt with is given by %BOUND, the choices
*     are pixel replication or a reflection about the edge pixels of the image.
*     The WMF can be repeated %NUMIT times before the filtered version is
*     written to the output image %OUTPIC which is given the title %OTITLE.
*    Parameters :
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing the 2-D array to be filtered.
*     MODE   = INTEGER( READ )
*           Determines type of weighting used, -1 gives user defined weighting.
*     CORNER = INTEGER( READ )
*           Corner value for weighting function, required  if MODE = -1.
*     SIDE   = INTEGER( READ )
*           Side value for weighting function, required if MODE = -1.
*     CENTRE = INTEGER( READ )
*           Central value for weighting function, required if MODE = -1.
*     STEP   = INTEGER( READ )
*           Spacing between the median filter elements.
*     DIFF   = REAL( READ )
*           Replacement of value by median occurs if abs(value-median) > DIFF.
*     BOUND  = CHAR*12( READ )
*           Determines whether REFlection or REPlication occurs at the array
*           edge.
*     NUMIT  = INTEGER( READ )
*           Number of iterations of the filter.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain the 2-D array after filtering.
*     OTITLE = CHAR*72( READ )
*           Will form the TITLE component of the output IMAGE structure.
*    Method :
*     Get input IMAGE structure and map DATA_ARRAY component
*     Request filter type, user defined or pre-defined
*     If user defined then
*        Get CORNER, SIDE and CENTRE values for weighting function
*        Value for CENTRE must be odd
*     Endif
*     Calculate maximum stepsize as (minimum dimension - 2)/2
*     Get stepsize for filter in range 1 to maximum stepsize
*     Get threshold for replacement of pixel value by median
*     Ask whether to use pixel replication or reflection at edges of image
*     Find out how many iterations are required
*     Create output IMAGE structure, create and get a value for a TITLE
*       component and create a DATA_ARRAY component with the same shape as
*       the input DATA_ARRAY component
*     Map the output DATA_ARRAY component
*     Allocate the workspace required by MEDWTS
*     If no errors so far then
*        Copy input array into the output array
*        Call MEDSET to set up weighting function, position of median and
*          sample offset information
*        For required number of iterations
*           Put output array into workspace padding the edges by either
*             replication or reflection as required
*           Perform weighted median filtering using MEDWTS
*           Inform user of number of iterations completed
*        Endfor
*     Endif
*     Tidy up the input/output images and workspace
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     20/10/1983 : Original version                (ROE::ASOC5)
*     17/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     12-APR-1994  CHANGED DAT AND CMP CALLS TO NDF (SKL@JACH)
*     11-AUG-1994  Changed DIM arguments for COPY2D and
*                  MEDREP, MEDREF, MEDWTS  (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Status :
      INTEGER STATUS
*    External references :
      LOGICAL CHR_SIMLR
*    Local constants :
      INTEGER NDIM            ! dimensionality of input/output images
      PARAMETER ( NDIM = 2 )
      INTEGER LBND(2)         ! lower bound temporary NDF
      DATA LBND / 1, 1 /
      INTEGER SAMSIZ          ! maximum sample size for sorting
      PARAMETER ( SAMSIZ = 9 )
*    Local variables :
      CHARACTER*12
     :  BOUND ! determines what to do at image boundary

      INTEGER                ! locators for :
     :  LOCI,                ! input IMAGE structure
     :  LOCO,                ! output IMAGE structure
     :  WLOC                 ! workspace for MEDWTS routine

      INTEGER
     :  PLACE,               ! placeholder for temporary NDF
     :  NELEMENTS,           ! number of elements mapped
     :  NDIMS,               ! dimensions from NDF_DIM
     :  SAMINF( SAMSIZ, 3 ), ! weights, offsets for sample elements (in MEDWTS)
     :  SAMWT( SAMSIZ ), ! stores weights during sorting ( used in MEDWTS )
     :  WDIMS( NDIM ), !      "      " workspace array
     :  DIMS( NDIM ), ! dimensions of input/output DATA_ARRAYs
     :  PNTRO,  ! pointer to : output DATA_ARRAY
     :  PNTRI,  !            : input  DATA_ARRAY
     :  WPNTR,  !            : workspace for MEDWTS
     :  MODE    ! type of weighted median filter
      INTEGER
     :  STEP,   ! separation of weighted median filter elements
     :  NUMIT,  ! number of iterations of the filter
     :  CENTRE, ! central value of wmf weighting function
     :  CORNER, ! corner    "    "  "      "         "
     :  SIDE,   ! side      "    "  "      "         "
     :  MEDPOS, ! position of median in the sample
     :  NUMSAM, ! number of elements in the sample
     :  MXSTEP, ! maximum allowed stepsize
     :  ITERAT  ! iteration counter

      REAL
     :  SAMPLE( SAMSIZ ), ! holds sample for sorting ( used in MEDWTS )
     :  DIFF    ! if abs( value - median ) > diff then replacement occurs
*-

*    get the input IMAGE structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map the input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :     PNTRI, NELEMENTS, STATUS )

*       get dimensions
         CALL NDF_DIM( LOCI, NDIM, DIMS, NDIMS, STATUS )

*       value for MODE must be in the range -1 to 7, suggest default 3
         CALL APPG0I( 'MODE', 3, -1, 7, MODE, STATUS )

*       if MODE is -1 then a user defined weighting function will be input
         IF( MODE .EQ. -1 ) THEN

*          get the weighting values
            CALL APPG0I( 'CORNER', 1, 0, 10, CORNER, STATUS )
            CALL APPG0I(   'SIDE', 1, 0, 10,   SIDE, STATUS )

*          the value for CENTRE must be an odd number
            CALL ODDGET( 'CENTRE', 3, 1, 21, CENTRE, STATUS )
         ENDIF

*       calculate the maximum possible step value
         MXSTEP = ( MIN( DIMS( 1 ), DIMS( 2 ) ) - 2 ) / 2

*       value for STEP must be in the range 1 to MXSTEP
         CALL APPG0I( 'STEP', 1, 1, MXSTEP, STEP, STATUS )

*       value for DIFF must be in the range 0.0 to a very large number
         CALL APPG0R( 'DIFF', 0.0, 0.0, 1.0E38, DIFF, STATUS )

*       find out what to do at the image boundary
         CALL APPG0C( 'BOUND', 3, 'REP,REF', BOUND, STATUS )

*       value for NUMIT must be in the range 1 to 50
         CALL APPG0I( 'NUMIT', 1, 1, 50, NUMIT, STATUS )

*       create output IMAGE structure, create a TITLE component and get a
*       value for it and create a DATA_ARRAY component of dimensionality NDIM
*       and dimension DIMS( NDIM )
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          map the output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          calculate dimensions of workspace array
            WDIMS( 1 ) = DIMS( 1 ) + ( 2 * STEP )
            WDIMS( 2 ) = DIMS( 2 ) + ( 2 * STEP )

*          create and map workspace array
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW(  '_REAL', NDIM, LBND, WDIMS, PLACE, WLOC,
     :                     STATUS )
            CALL NDF_MAP( WLOC, 'DATA', '_REAL', 'WRITE', WPNTR,
     :                    NELEMENTS, STATUS )

*          check for errors
            IF( STATUS .EQ. SAI__OK ) THEN

*             copy input DATA_ARRAY component into output DATA_ARRAY component
               CALL COPY2D( DIMS(1), DIMS(2), %VAL( PNTRI ),
     :                      %VAL( PNTRO ), STATUS )

*             set up the weighting function, position of median etc.
               CALL MEDSET( MODE, STEP, SAMSIZ, CENTRE, CORNER, SIDE,
     :           MEDPOS, NUMSAM, SAMINF, STATUS )

*             perform weighted median filtering required number of iterations
               DO ITERAT = 1, NUMIT

*                transfer the output image into the workspace
                  IF( CHR_SIMLR( 'REF', BOUND ) ) THEN

*                   pad edges by reflection
                     CALL MEDREF( STEP, DIMS(1), DIMS(2), %VAL( PNTRO ),
     ;                 WDIMS(1), WDIMS(2), %VAL( WPNTR ), STATUS )
                  ELSE

*                   pad edges by replication
                     CALL MEDREP( STEP, DIMS(1), DIMS(2), %VAL( PNTRO ),
     :                 WDIMS(1), WDIMS(2), %VAL( WPNTR ), STATUS )
                  ENDIF

*                perform the 2-D median filtering using MEDWTS
                  CALL MEDWTS( DIFF, STEP, NUMSAM, MEDPOS, SAMSIZ,
     :              SAMINF, WDIMS(1), WDIMS(2), %VAL( WPNTR ),
     :              DIMS(1), DIMS(2), %VAL( PNTRO ),
     :              SAMPLE, SAMWT, STATUS )

*                let the user know what is going on
                  CALL MSG_SETI( 'ITERAT', ITERAT )
                  CALL MSG_OUT( 'MSG_ITERAT',
     :              'Iteration ^ITERAT completed.', STATUS )
               ENDDO
            ENDIF

*          tidy up the workspace and output structure
            CALL NDF_ANNUL(  WLOC, STATUS )
            CALL NDF_ANNUL(  LOCO, STATUS )
         ENDIF

*       tidy up the input structure
         CALL NDF_ANNUL(  LOCI, STATUS )
      ENDIF

      END
