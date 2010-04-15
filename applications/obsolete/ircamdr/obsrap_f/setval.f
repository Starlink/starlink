*+  SETVAL - sets a particular value in an image to another value

      SUBROUTINE SETVAL ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL SETVAL ( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be thresholded
*     OUTPIC  =  IMAGE( WRITE )
*           Thresholded version of the image
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     INVAL   =  REAL( READ )
*           The value to be changed
*     OUTVAL  =  REAL( READ )
*           The value to be changed to
*
*    Method :
*
*    Authors :
*
*     Colin Aspin (UKIRT)
*
*    History :
*
*     28-02-1987 : Original version (UKTH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-JUN-1994  Changed explicit max/min real value to NUM__MAXR/MINR
*                  from PRIMDAT (SKL@JACH)
*     11-Aug-1994  Changed input DIM arguments for SETVALSUB (SKL@JACVH)
*
*    Type Definitions :

      IMPLICIT NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'             ! SSE global definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'PRM_PAR'             ! PRIMDAT constants

*    Status :

      INTEGER STATUS                ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                     ! image dimensionality

      PARAMETER ( NDIMS  =  2 )     ! 2-d images only

      REAL
     :    MAXVAL,                   ! maximum allowable real value
     :    MINVAL                    ! minimum     "       "       "
      PARAMETER( MAXVAL  = NUM__MAXR )! maximum real
      PARAMETER( MINVAL  = NUM__MINR )! minimum real

*    Local variables :

      INTEGER
     :    LOCI,                     ! locator for input data structure
     :    LOCO,                     ! locator for output data structure
     :    IDIMS( NDIMS ),           ! dimensions of input data array
     :    ACTDIM,                   ! actual dimensions from NDF_DIM
     :    NELEMENTS,                ! number of elements mapped by NDF_MAP
     :    PNTRI,                    ! pointer to input data array component
     :    PNTRO                     !    "     " output  "    "       "

      REAL
     :    INVAL,                    ! value to be changed
     :    OUTVAL                    ! value to be changed to

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      END IF

*    get a locator to input IMAGE type data structure

      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error

      IF( STATUS .EQ. SAI__OK ) THEN

*       map the input data structure and get dimensions

         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, ACTDIM, STATUS )

*       get the value to be changed

         CALL AIF_GET0R( 'INVAL', 0.0, MINVAL, MAXVAL,
     :                    INVAL, STATUS )

*       get the value to be changed to

         CALL AIF_GET0R( 'OUTVAL', 0.0, MINVAL, MAXVAL,
     :                    OUTVAL, STATUS )

*       check for error so far

         IF ( STATUS .EQ. SAI__OK ) THEN

*          create output image type data structure

            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS,
     :                    LOCO, STATUS )

*          check for error

            IF( STATUS .EQ. SAI__OK ) THEN

*             map output DATA_ARRAY component

               CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                       PNTRO, NELEMENTS, STATUS )

*             if there have been no errors then perform the thresholding

               IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL SETVALSUB( %VAL( PNTRI ), %VAL( PNTRO ),
     :                            IDIMS(1), IDIMS(2), INVAL, OUTVAL,
     :                            STATUS )

               END IF

*             tidy up the output data structure

               CALL NDF_ANNUL( LOCO, STATUS )

*          end of if-no-error-getting-output-structure check

            END IF

*       end of if-no-error-after-getting-threshold-values check

         END IF

*       tidy up the input data structure

         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-getting-input-structure check

      END IF


*    end

      END
