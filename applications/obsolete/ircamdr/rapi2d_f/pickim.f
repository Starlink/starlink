
*+  PICKIM - create a new image from a subset of another

      SUBROUTINE PICKIM ( STATUS )

*    Description :
*
*     A new 2d image is created from a user-specified subset of an
*     existing input image.
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image from which subset is to be taken
*     OUTPIC  =  IMAGE( WRITE )
*           Image resulting from taking subset
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     XSTART  =  INTEGER( READ )
*           x coordinate of first column to be included
*     Xsize   =  INTEGER( READ )
*           x size of box to be picked
*     Ysize   =  INTEGER( READ )
*           y size of box to be picked
*     YFINISH  =  INTEGER( READ )
*           y coordinate of last row to be included
*
*    Method :
*
*     Get input image from environment
*     If no error so far then
*        Map its data array component
*        If no error so far then
*           Output dimensions of array to interface
*           Get start and size of subimage to be picked
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
     :    PNTRO,                  !    "     " output  "
     :	  XSIZE,
     :    XSTART,                 ! first column to be included in output
     :    XFINISH,                ! last     "    "  "     "     "    "
     :	  YSIZE,
     :    YSTART,                 ! first row     "  "     "     "    "
     :    YFINISH,                ! last   "      "  "     "     "    "
     :    LOCI,                   ! input data structure
     :    LOCO                    ! output data structure

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

*          now get the column and row start and finish coordinates
*          of the subset image to be created
            CALL PAR_GET0I( 'XSTART', XSTART, STATUS )
            CALL PAR_GET0I( 'YSTART', YSTART, STATUS )
            CALL PAR_GET0I( 'XSIZE', XSIZE, STATUS )
            CALL PAR_GET0I( 'YSIZE', YSIZE, STATUS )

*          calculate the x and y finish pixels

	    IF( XSTART .LT. 1) XSTART = 1
	    IF( XSTART .GT. IDIMS( 1)) XSTART = IDIMS( 1)

	    IF( YSTART .LT. 1) YSTART = 1
	    IF( YSTART .GT. IDIMS( 2)) YSTART = IDIMS( 2)

	    XFINISH = XSTART + XSIZE - 1
	    YFINISH = YSTART + YSIZE - 1

	    IF( XFINISH .GT. IDIMS( 1)) XFINISH = IDIMS( 1)
	    IF( YFINISH .GT. IDIMS( 2)) YFINISH = IDIMS( 2)

*          if no error so far then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             work out the size of the output image
               ODIMS( 1 )  =  XFINISH - XSTART + 1
               ODIMS( 2 )  =  YFINISH - YSTART + 1

*             tell user dimensions of output array
               CALL MSG_SETI( 'XDIM', ODIMS(1) )
               CALL MSG_SETI( 'YDIM', ODIMS(2) )
               CALL MSG_OUT( 'OUTPUT_DIMS',
     :     'Output image will be ^XDIM by ^YDIM pixels', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             now create output IMAGE type data structure
               CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO,
     :                       STATUS )

*             if no error so far then continue
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map output data array component
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO, NELEMENTS, STATUS )

*                check for error before accessing pointer
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   copy the requested subset of the input image into
*                   the output image
                     CALL PICKIMSUB( %VAL( PNTRI ), IDIMS(1),
     :                       IDIMS(2), XSTART, XFINISH, YSTART,
     :                       YFINISH, %VAL( PNTRO ), ODIMS(1),
     :                       ODIMS(2), STATUS )

                  END IF


*             end of if-no-error-before-mapping-output check
               END IF

*             tidy output structure
               CALL NDF_ANNUL( LOCO, STATUS )

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
