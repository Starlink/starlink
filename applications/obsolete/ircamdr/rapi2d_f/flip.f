
*+  FLIP - perform a left-right or top-bottom reversal of an image

      SUBROUTINE FLIP ( STATUS )

*    Description :
*
*     The input image, %INPIC, is reversed from left to right ( horizontally )
*     if %FTYPE is 'H' or from top to bottom ( vertically ) if %FTYPE is 'V'.
*     The image after reversal is written to %OUTPIC with the title %OTITLE.
*
*    Invocation :
*
*     CALL FLIP( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array to be flipped.
*     FTYPE   = CHAR( READ )
*           Type of flip, horizontal ( 'H' ) or vertical ( 'V' ), to be
*           performed.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain 2-D array after being flipped.
*     OTITLE = CHAR( READ )
*           Will form the TITLE component of the output IMAGE structure.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image type data structure
*     If no error so far then
*        Map its DATA_ARRAY component
*        Get a value for FTYPE which is either 'H' or 'V'
*        Create output image type data structure and title
*        If no error so far then
*           Map a DATA_ARRAY component of same size as input
*           If no error so far then
*              Call FLIPS to perform the reversal
*           Endif
*           Tidy up output image structure
*        Endif
*        Tidy up the input image structure
*     Endif
*     End
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*
*    History :
*
*     27/07/1983 : Original version                    (ROE::ASOC5)
*     17/02/1984 : Modified to use the TITLE component (ROE::ASOC5)
*     27/05/1985 : Changed TYPE to FTYPE - ADAM parameter system
*                : disliked use of TYPE in IFL file    (REVA::MJM)
*     11-04-1986 : Tidied up (REVA::MJM)
*     10-MAR-94    Changed DAT_, CMP_ to NDF_ (SKL@JACH)
*     11-Aug-1994  Changed input DIM arguments of FLIPS (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local constants :

      INTEGER NDIMS             ! dimensionality of images
      PARAMETER ( NDIMS = 2 )   ! 2-d only

*    Local variables :

      INTEGER
     :  NELEMENTS,              ! Number of elements mapped
     :  NDIM,                   ! Dimensions from NDF_DIM
     :  DIMS( NDIMS ),          ! dimensions of input/output DATA_ARRAYs
     :  PNTRI,                  ! pointer to  input DATA_ARRAY
     :  PNTRO                   !    "     "  output    "

      INTEGER                   ! locators for :
     :  LOCI,                   ! input data structure
     :  LOCO                    ! output data structure

      CHARACTER*1
     :  FTYPE                   ! type of flip to be performed

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :      PNTRI, NELEMENTS, STATUS )

*       get dimensions of array
         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS)

*       get the direction of flip to be done
         CALL AIF_CHOIC( 'FTYPE', 'H,V', FTYPE, STATUS )

*       create output IMAGE type data structure and title
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          check for error before calling working subroutine
            IF( STATUS .EQ. SAI__OK ) THEN

*             call FLIPS to perform the flipping
               CALL FLIPS( FTYPE, DIMS(1), DIMS(2), %VAL(PNTRI),
     :           %VAL(PNTRO), STATUS )

            ENDIF

*          tidy up the output structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-error-after-getting-output-structure check
         ENDIF

*       tidy up the input structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-error-after-getting-input-structure check
      ENDIF


*    end
      END
