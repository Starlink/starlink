
*+  SHIFT - perform an X,Y shift on an image
      SUBROUTINE SHIFT( STATUS )
*    Description :
*     The input image, %INPIC, is shifted, in either or both of the X and
*     Y axes, to produce the output image, %OUTPIC. The shifts in X and Y
*     are either input as absolute X and Y shifts by the user or alternatively
*     are calculated from the coordinates of two points provided by the user.
*     These are a fiducial point, with coordinates %FIDX, %FIDY, and a standard
*     object, with coordinates %OBJX, %OBJY. The shift in X is then given by
*     %FIDX - %OBJX and the shift in Y is given by %FIDY - %OBJY.
*     The output image is padded with zeros in the regions not occupied by
*     the shifted input image.
*    Parameters :
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing the 2-D array to be shifted.
*     STYPE  = CHAR( READ )
*           Which sort of shift is to be used : Relative or Absolute
*     ABSX   = REAL( READ )
*           Absolute X shift
*     ABSY   = REAL( READ )
*           Absolute Y shift
*     FIDX   = REAL( READ )
*           X-coordinate of the fiducial point.
*     FIDY   = REAL( READ )
*           Y-coordinate of the fiducial point.
*     OBJX   = REAL( READ )
*           X-coordinate of the standard object.
*     OBJY   = REAL( READ )
*           Y-coordinate of the standard object.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain the 2-D array after being shifted.
*     OTITLE = CHAR*72( READ )
*           Will be used as the TITLE component for the output IMAGE structure.
*    Method :
*     Get input IMAGE type data structure and map DATA_ARRAY component
*     Input which type of shift is to be used : Relative or Absolute
*     If Absolute then
*       Input absolute X and Y shifts
*     Else
*       Input X and Y coordinates of the fiducial point.
*       Input X and Y coordinates of the "standard object".
*     Endif
*     Calculate the shifts in the X and Y directions.
*     Write out the values of the shifts to the user.
*     Call SHIFTS for each axis to set up the parameters for performing
*       the shifts.
*     Create output IMAGE type data structure with DATA_ARRAY component
*       of the same dimensions as input DATA_ARRAY component and get a
*       TITLE component for it.
*     Create workspace of same dimensions as input/output images.
*     Call SHIFTX to move the input image, shifted in the X direction,
*        into the workspace.
*     Call SHIFTY to move the workspace, shifted in the Y direction, into
*        the output image.
*     Tidy up the input/output images and workspace.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*    History :
*     18/08/1983 : Original version                   (ROE::ASOC5)
*     19/02/1984 : Modified to use new SHIFTS routine (ROE::ASOC5)
*     03/06/1985 : Modified to allow Relative or Absolute
*                : shifting                           (REVA::MJM)
*     12-APR-1994  Changed DAT, CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Status :
      INTEGER STATUS
*    Local constants :
      INTEGER NDIM
      PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images
*    Local variables :
      INTEGER                ! locators for :
     :  LOCI,                ! input data structure
     :  LOCO,                ! output data structure
     :  WLOC                 ! workspace array
      INTEGER
     :  DIMS( NDIM ), ! dimensions of the input/output DATA_ARRAYs
     :  NDIMS,        ! number of dimensions from NDF_DIM
     :  NELEMENTS,    ! number of elements mapped by NDF_MAP
     :  PNTRI,        ! pointer to : input DATA_ARRAY
     :  PNTRO,        !            : output DATA_ARRAY
     :  PLACE,        ! place holder for temporary array
     :  WPNTR,        !            : workspace array
     :  LBND( 2 ),    ! lower bounds for temporary array
     :  INTXS, ! integer number of pixels for shift in X direction
     :  INTYS  !    "       "    "    "    "    "    " Y     "

      DATA LBND / 1, 1 /

      REAL
     :  ABSX,   ! absolute X shift
     :  ABSY,   !     "    Y   "
     :  FIDX,   ! fiducial point X coordinate
     :  FIDY,   !     "      "   Y      "
     :  OBJX,   ! standard object X coordinate
     :  OBJY,   !     "      "    Y     "
     :  XSHIFT, ! value of shift in X direction
     :  YSHIFT, !   "   "    "    " Y     "
     :  FRACX,  ! fractional part of shift in X direction
     :  FRACY   !      "       "   "   "    " Y     "

      LOGICAL
     :  ABSOLUTE, ! will be .true. if shift is absolute
     :  XWHOLE,   ! will be .true. if shift in X is whole number of pixels
     :  YWHOLE,   !   "   "    "    "   "    " Y  "   "      "    "    "
     :  XNEG,     !   "   "    "    "   "    " X  " negative
     :  YNEG      !   "   "    "    "   "    " Y  "     "

      CHARACTER*1
     :  STYPE     ! type of shifting to be used : Relative or Absolute
*-

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :      PNTRI, NELEMENTS, STATUS)

         CALL NDF_DIM( LOCI, NDIM, DIMS, NDIMS, STATUS )

*       get type of shift - Relative or Absolute
*       (uses an AIF call - library AIF.OLB by Baines and Beard)

         CALL AIF_CHOIC( 'STYPE', 'A,a,R,r', STYPE, STATUS )

*       now set logical accordingly

         IF( STYPE .EQ. 'R' .OR. STYPE .EQ. 'r' ) THEN
            ABSOLUTE = .FALSE.
         ELSE
            ABSOLUTE = .TRUE.
         ENDIF

*       now get shift amounts, depending on type of shift

         IF ( ABSOLUTE ) THEN

*          get x and y shifts

            CALL PAR_GET0R( 'ABSX', ABSX, STATUS )
            CALL PAR_GET0R( 'ABSY', ABSY, STATUS )

*          set x and y shifts accordingly

            XSHIFT = ABSX
            YSHIFT = ABSY

         ELSE

*          get position of fiducial point
            CALL PAR_GET0R( 'FIDX', FIDX, STATUS )
            CALL PAR_GET0R( 'FIDY', FIDY, STATUS )

*          get position of standard object
            CALL PAR_GET0R( 'OBJX', OBJX, STATUS )
            CALL PAR_GET0R( 'OBJY', OBJY, STATUS )

*          calculate X and Y shifts
            XSHIFT = FIDX - OBJX
            YSHIFT = FIDY - OBJY

         ENDIF

*       tell user what the X and Y shifts are
         CALL MSG_SETR( 'XSHIFT', XSHIFT )
         CALL MSG_SETR( 'YSHIFT', YSHIFT )
         CALL MSG_OUT( 'SHIFT_XY',
     :     'Shift in X = ^XSHIFT, Shift in Y = ^YSHIFT', STATUS )

*       call SHIFTS to set up information for SHIFTX and SHIFTY
         CALL SHIFTS( DIMS(1), XSHIFT, 'X', INTXS, XWHOLE, XNEG, FRACX,
     :     STATUS )
         CALL SHIFTS( DIMS(2), YSHIFT, 'Y', INTYS, YWHOLE, YNEG, FRACY,
     :     STATUS )

*       create the output IMAGE structure and get a title for it
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          create and map the workspace array
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, DIMS, PLACE, WLOC,
     :                    STATUS )
            CALL NDF_MAP( WLOC, 'DATA', '_REAL', 'WRITE', WPNTR,
     :                    NELEMENTS, STATUS )

            IF( STATUS .EQ. SAI__OK ) THEN

*             shift input image into workspace in X direction
               CALL SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIMS( 1),
     :           DIMS( 2), %VAL( PNTRI ), %VAL( WPNTR ), STATUS )

*             shift workspace into output array in Y direction
               CALL SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIMS( 1),
     :           DIMS( 2), %VAL( WPNTR ), %VAL( PNTRO ), STATUS )
            ENDIF

*          tidy up output structure and workspace
            CALL NDF_ANNUL(  LOCO, STATUS )
            CALL NDF_ANNUL(  WLOC, STATUS )
         ENDIF

*       tidy up input structure
         CALL NDF_ANNUL(  LOCI, STATUS )
      ENDIF

      END
