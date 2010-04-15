
*+  ROTATE - rotate a 2-D image through any angle

      SUBROUTINE ROTATE ( STATUS )

*    Description :
*
*     The input image, %INPIC, which can be square or rectangular,
*     is rotated through %ANGLE degrees clockwise. The rotated
*     image is written to the output image, %OUTPIC, along with the
*     title %OTITLE.
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array to be rotated.
*     ANGLE  = REAL( READ )
*           Number of clockwise degrees the array is to be rotated by.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain the 2-D array after rotation.
*     OTITLE = CHAR*72( READ )
*           Will be used as the TITLE component for the output IMAGE structure.
*     SQRMAX = INTEGER( READ )
*           Maximum size of square sub-image to be used in rotation by
*           ROTAS4 - can be tuned to minimise processing time due to paging
*
*    Method :
*     Get locator to input IMAGE type data structure
*     Get number of clockwise degrees image to be rotated by,
*       must be between 0.0 and 360.0.
*     Map input DATA_ARRAY component
*     Determine dimensions of output DATA_ARRAY component either trivially
*       for 90.0 multiples or using ROTSIZE otherwise
*     Create output IMAGE type data structure, get a TITLE component for
*       it and create a DATA_ARRAY component of appropriate dimensions
*     If rotation angle is not divisible by 90.0 then use ROTNRS to
*        perform the rotation into the output array
*     Elseif number of right-angles is 2, ie 180 degree rotate, then
*        Copy input data_array component to output data_array component
*        Perform 180 degree rotate using ROTAS2
*     Else
*        Rotation through +/- 90 degrees
*        Get maximum rotation box size from environment
*        Calculate rotation box size and find longest and shortest dimensions
*        of input image using ROTAS3
*        Create workspace
*        Perform +/- 90 deg. rotate using ROTAS4
*        Tidy up workspace
*     Endif
*     Tidy up input/output structures
*    Authors :
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*     Konrad Flamm ( REVA::MJM )
*    History :
*     27/07/1983 : Original version                (ROE::ASOC5)
*     19/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     12-11-1985 : Implemented GENROT using much of the ROTATE
*                : software by Dave Baines plus extra code to
*                : do non-right angle rotations (REVA::MJM)
*     18-11-1985 : Renamed back to ROTATE as all options now
*                : work properly (REVA::MJM)
*     12-APR-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     11-AUG-1994  Changed input DIMs to COPY2D, ROTAS4, ROTNRS, ROTSIZE
*                  (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE        ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'    ! SSE global constants
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS       ! Global status parameter

*    Local constants :

      INTEGER NDIM
      PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images

*    Local variables :

      INTEGER                ! locators for :
     :  LOCI,                ! input data structure
     :  LOCO,                ! output data structure
     :  WKLOC                ! workspace for ROTAS4 subroutine

      INTEGER
     :  IDIMS( NDIM ), ! dimensions for input image
     :  ODIMS( NDIM ), ! dimensions for output image
     :  WKDIMS( NDIM ),! dimensions for workspace
     :  LBND( 2 ),     ! lower bounds of work array
     :  NDIMS,         ! number of dimensions from NDF_DIM
     :  NELEMENTS,     ! number of elements mapped by NDF_MAP
     :  PNTRI,  ! pointer to mapped input image
     :  PNTRO,  ! pointer to mapped output image
     :  WKPNTR, ! pointer to mapped workspace
     :  ROTSIZ, ! size of the square subimage for rotation
     :  LONG,   ! longest dimension of input image
     :  SHORT,  ! shortest    "      "   "     "
     :  NUMRA,  ! number of clockwise right angles to be applied
     :  SQRMAX  ! maximum sub-image box size to be used by ROTAS4

      DATA LBND / 1, 1 /

      REAL
     :  ANGLE   ! number of clockwise degrees rotation to be used

      LOGICAL
     :  NRAFLG, ! true if non-right angle rotation is requested
     :  XLARGE  ! true if first dimension is largest dimension

*-

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK )THEN

*       map the input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :     PNTRI, NELEMENTS, STATUS)

         CALL NDF_DIM( LOCI, NDIM, IDIMS, NDIMS, STATUS )

*       get the number of clockwise degrees rotation to be applied
         CALL AIF_GET0R( 'ANGLE', 90.0, 0.0, 360.0, ANGLE, STATUS )

*       check for values of ANGLE that are divisible by 90.0 -
*       these are dealt with separately

         IF( ANGLE .EQ. 0.0 .OR. ANGLE .EQ. 360.0 ) THEN
                                       ! no rotation requested - exit
            CALL MSG_OUT( 'NO_ROT', 'No rotation requested - exit',
     :                     STATUS )
            CALL NDF_ANNUL( LOCI, STATUS )
            RETURN

         ELSEIF( ANGLE .EQ. 90.0 ) THEN
                                       ! simple 90 degree rotation
            NUMRA   =  1
            NRAFLG  = .FALSE.

         ELSEIF( ANGLE .EQ. 180.0 ) THEN
                                       ! simple 180 degree rotation
            NUMRA   =  2
            NRAFLG  = .FALSE.

         ELSEIF( ANGLE .EQ. 270.0 ) THEN
                                       ! simple 270 degree rotation
            NUMRA   =  3
            NRAFLG  = .FALSE.

         ELSE
                                       ! not a simple 90.0 type rotation
            NUMRA   =  0
            NRAFLG  = .TRUE.

         ENDIF

*       now rotate image using one of two basic methods, depending
*       on whether or not the input rotation angle was found to be
*       an integer multiple of 90.0 degrees - if NRAFLG is true,
*       then a non-90.0 rotation has been requested

         IF( NRAFLG ) THEN

*          work out the dimensions of the output array to hold the
*          results of the non-right angle rotation
            CALL ROTSIZE( IDIMS(1), IDIMS(2), ANGLE, ODIMS(1),
     :                    ODIMS(2), STATUS )
*	type *, 'odims = ', odims( 1), odims( 2)

*          get an output array of the correct dimensions
            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, ODIMS, LOCO, STATUS )

*          map the output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          call the subroutine to do the non-right angle rotation
*	type *, 'angle = ', angle
            CALL ROTNRS( %VAL( PNTRI ), IDIMS(1), IDIMS(2),
     :                   %VAL( PNTRO ), ODIMS(1), ODIMS(2), ANGLE,
     :                   STATUS )

*          tidy up the output structure
            CALL NDF_ANNUL( LOCO, STATUS )

         ELSE

*       a rotation angle divisible by 90.0 degrees has been requested -
*       proceed according to the set value of NUMRA


            IF( NUMRA .EQ. 2 ) THEN

*             a 180 deg. rotate so output dimensions are same
*             as the work (or input) dimensions
               ODIMS( 1 ) = IDIMS( 1 )
               ODIMS( 2 ) = IDIMS( 2 )

            ELSE

*             must be 90 or 270 deg. rotate so reverse dimensions
               ODIMS( 1 ) = IDIMS( 2 )
               ODIMS( 2 ) = IDIMS( 1 )

            ENDIF

*          create output IMAGE type data structure, create and get
*          a value for the TITLE component and create a DATA_ARRAY
*          component of dimensions ODIMS
            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, ODIMS, LOCO, STATUS )

*          map the output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )


            IF( NUMRA .EQ. 2 ) THEN

*             rotation is through 180 degrees so can perform
*             straightforward rotate. Copy input DATA_ARRAY
*             component into output DATA_ARRAY component
               CALL COPY2D( ODIMS(1), ODIMS(2), %VAL( PNTRI ),
     :           %VAL( PNTRO ), STATUS )

*             perform the 180 degree rotate
               CALL ROTAS2( NUMRA, ODIMS(1), ODIMS(2), ODIMS(1),
     :           ODIMS(2), %VAL(PNTRO), STATUS )

            ELSE

*             rotation is through +/- 90 degrees

*             get the maximum rotation box size to be used from the
*             environment
               CALL PAR_GET0I( 'SQRMAX', SQRMAX, STATUS )

*             set up rotation box size, long dimension flag etc.
               CALL ROTAS3( IDIMS, SQRMAX, XLARGE,
     :           ROTSIZ, LONG, SHORT, STATUS )

*             create workspace and map it
               WKDIMS(1) = ROTSIZ
               WKDIMS(2) = ROTSIZ

	       CALL NDF_CREAT( 'SCRATCH_NAME', '_REAL', NDIM, LBND,
     :                         WKDIMS, WKLOC, STATUS)

	       CALL NDF_MAP( WKLOC, 'DATA', '_REAL', 'WRITE',
     :	                     WKPNTR, NELEMENTS, STATUS)

*             perform the +/- 90 deg. rotate
               CALL ROTAS4( NUMRA, LONG, SHORT, ROTSIZ, XLARGE,
     :                 IDIMS(1), IDIMS(2), %VAL(PNTRI), ODIMS(1),
     :                 ODIMS(2), %VAL(PNTRO), %VAL(WKPNTR), STATUS )

*             tidy up the workspace
               CALL NDF_ANNUL( WKLOC, STATUS )

            ENDIF

*          tidy up the output structure
            CALL NDF_ANNUL( LOCO, STATUS )

         ENDIF

*       tidy up the input structure
         CALL NDF_ANNUL( LOCI, STATUS )
      ENDIF


*    that's it.

      END
