*+  POLSEP - creates 4 new image from a dual-beam poarization image

      SUBROUTINE POLSEP ( STATUS )

*    Description :
*
*     Four new 2d images are created from a user-specified subset of an
*     existing input image.
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image from which subset is to be taken
*     OUTPIC1 = IMAGE( READ )
*           Image with 1st quadrant data
*     OUTPIC2 = IMAGE( READ )
*           Image with 2nd quadrant data
*     OUTPIC3 = IMAGE( READ )
*           Image with 3rd quadrant data
*     OUTPIC4 = IMAGE( READ )
*           Image with 4th quadrant data
*
*    Method :
*
*     Get input image from environment
*     If no error so far then
*        Map its data array component
*        If no error so far then
*           Output dimensions of array to interface
*           If no error so far then
*              Create an output image structure
*              If no errors so far then
*                 Map in a data array component
*                 If no error so far then
*                    Call PICKIMSUB to copy the subset of the input
*                     image to the output image
*                 Endif
*              Unmap the output data array
*              Endif
*              Tidy up the output image structure
*           Endif
*           Unmap the input data array
*        Endif
*        Tidy up the input image structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     26-11-1986 : First implementation (UKTH::MJM)
*     01-12-1987 : changed x,y end to x,y size (UKTH::CAA)
*     12-Apr-1994  changed DAT and CMP calls to NDF (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for PICKIMSUB (SKL@JACH)
*     25-Aug-1995: made this version from pcikim (caa@jach)
*
*    Type Definitions :

      IMPLICIT NONE               ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS              ! global status variable

*    Local Constants :

      INTEGER NDIMS               ! image dimensionality
      PARAMETER ( NDIMS = 2 )     ! defaulted to 2-d

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input image
     :    ODIMS( NDIMS ),         !      "      " output  "
     :    NELEMENTS,              ! number elements mapped by NDF_MAP
     :    NDIM,                   ! number of dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input image
     :    PNTRO1,                  !    "     " output  "
     :    PNTRO2,                  !    "     " output  "
     :    PNTRO3,                  !    "     " output  "
     :    PNTRO4,                  !    "     " output  "
     :    XSTART,                 ! first column to be included in output
     :    XFINISH,                ! last     "    "  "     "     "    "
     :    YSTART,                 ! first row     "  "     "     "    "
     :    YFINISH,                ! last   "      "  "     "     "    "
     :    LOCI,                   ! input data structure
     :    LOCO1,                  ! output data structure
     :    LOCO2,                  ! output data structure
     :    LOCO3,                  ! output data structure
     :    LOCO4                   ! output data structure

*-
*    check status on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       if no error so far then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          tell user dimensions of input array
            CALL MSG_SETI( 'XDIM', IDIMS(1) )
            CALL MSG_SETI( 'YDIM', IDIMS(2) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :        'Input image is ^XDIM by ^YDIM pixels', STATUS )

*          if no error so far then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             work out the size of the output image
               ODIMS( 1 )  =  IDIMS( 1)
               ODIMS( 2 )  =  IDIMS( 2)/4

*             tell user dimensions of output array
               CALL MSG_SETI( 'XDIM', ODIMS(1) )
               CALL MSG_SETI( 'YDIM', ODIMS(2) )
               CALL MSG_OUT( 'OUTPUT_DIMS',
     :     'Output image will be ^XDIM by ^YDIM pixels', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             now create output IMAGE type data structure
               CALL CREOUT( 'OUTPIC1', 'OTITLE', NDIMS, ODIMS, LOCO1,
     :                       STATUS )
               CALL CREOUT( 'OUTPIC2', 'OTITLE', NDIMS, ODIMS, LOCO2,
     :                       STATUS )
               CALL CREOUT( 'OUTPIC3', 'OTITLE', NDIMS, ODIMS, LOCO3,
     :                       STATUS )
               CALL CREOUT( 'OUTPIC4', 'OTITLE', NDIMS, ODIMS, LOCO4,
     :                       STATUS )

*             if no error so far then continue
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map output data array component
                  CALL NDF_MAP( LOCO1, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO1, NELEMENTS, STATUS )
                  CALL NDF_MAP( LOCO2, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO2, NELEMENTS, STATUS )
                  CALL NDF_MAP( LOCO3, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO3, NELEMENTS, STATUS )
                  CALL NDF_MAP( LOCO4, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO4, NELEMENTS, STATUS )

*                check for error before accessing pointer
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                  calculate the x and y finish pixels
	            XSTART = 1
	            YSTART = 1
	            XFINISH = IDIMS( 1)
	            YFINISH = IDIMS( 2)/4
	PRINT *, xstart, xFINISH, ystart, yFINISH

*                   copy the requested subset of the input image into
*                   the output image
                     CALL PICKIMSUB( %VAL( PNTRI ), IDIMS(1),
     :                       IDIMS(2), XSTART, XFINISH, YSTART,
     :                       YFINISH, %VAL( PNTRO1 ), ODIMS(1),
     :                       ODIMS(2), STATUS )

*                  calculate the x and y finish pixels
	            XSTART = 1
	            YSTART = IDIMS( 2)/4 + 1
	            XFINISH = IDIMS( 1)
	            YFINISH = IDIMS( 2)/2
	PRINT *, xstart, xFINISH, ystart, yFINISH

*                   copy the requested subset of the input image into
*                   the output image
                     CALL PICKIMSUB( %VAL( PNTRI ), IDIMS(1),
     :                       IDIMS(2), XSTART, XFINISH, YSTART,
     :                       YFINISH, %VAL( PNTRO2 ), ODIMS(1),
     :                       ODIMS(2), STATUS )

*                  calculate the x and y finish pixels
	            XSTART = 1
	            YSTART = IDIMS( 2)/2 + 1
	            XFINISH = IDIMS( 1)
	            YFINISH = 3*IDIMS( 2)/4
	PRINT *, xstart, xFINISH, ystart, yFINISH

*                   copy the requested subset of the input image into
*                   the output image
                     CALL PICKIMSUB( %VAL( PNTRI ), IDIMS(1),
     :                       IDIMS(2), XSTART, XFINISH, YSTART,
     :                       YFINISH, %VAL( PNTRO3 ), ODIMS(1),
     :                       ODIMS(2), STATUS )

*                  calculate the x and y finish pixels
	            XSTART = 1
	            YSTART = 3*IDIMS( 2)/4 + 1
	            XFINISH = IDIMS( 1)
	            YFINISH = IDIMS( 2)
	PRINT *, xstart, xFINISH, ystart, yFINISH

*                   copy the requested subset of the input image into
*                   the output image
                     CALL PICKIMSUB( %VAL( PNTRI ), IDIMS(1),
     :                       IDIMS(2), XSTART, XFINISH, YSTART,
     :                       YFINISH, %VAL( PNTRO4 ), ODIMS(1),
     :                       ODIMS(2), STATUS )

                  END IF


*             end of if-no-error-before-mapping-output check
               END IF

*             tidy output structure
               CALL NDF_ANNUL( LOCO1, STATUS )
               CALL NDF_ANNUL( LOCO2, STATUS )
               CALL NDF_ANNUL( LOCO3, STATUS )
               CALL NDF_ANNUL( LOCO4, STATUS )

*          end of if-no-error-after-creating-output check
            END IF

*       end of if-no-error-before-getting-new-size check
         END IF


*    end of if-no-error-getting-input-data-frame check
      END IF

*    tidy the input structure
      CALL NDF_ANNUL( LOCI, STATUS )

*    end
      END
