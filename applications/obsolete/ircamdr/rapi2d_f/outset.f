
*+  OUTSET - sets value outside specified circle to specified value

      SUBROUTINE OUTSET ( STATUS )

*    Description :
*
*     A user-specified circle of given centre and diameter is taken and
*     all pixels outside this circle are set to a specified value, the
*     pixels within the circle remaining unchanged.
*
*    Invocation :
*
*     CALL OUTSET( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be modified
*     OUTPIC  =  IMAGE( WRITE )
*           Modified version of the image
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     XCENTRE  =  INTEGER( READ )
*           x coordinate of centre of circle to be used
*     YCENTRE  =  INTEGER( READ )
*           y coordinate of centre of circle to be used
*     DIAMETER  =  REAL( READ )
*           Diameter of circle to be used
*     NEWVAL  =  REAL( READ )
*           Value to replace old values in pixel outside circle
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input image structure
*     If no error then
*        Try to map a data array component
*        If no error then
*           Output dimensions of input array
*           Get x,y centre and diameter of circle to be taken, and
*            value to be substituted for pixels outside the circle
*           If no error then
*              Create an output structure to hold processed image
*              If no error then
*                 Map a data array component in the output structure
*                 If no error then
*                    Call working subroutine to copy input data array
*                     into output one, except outside defined circle
*                     where input new value is substituted
*                 Endif
*                 Unmap and tidy output data structure
*              Endif
*           Endif
*        Endif
*        Unmap and tidy input data structure
*     Endif
*     Return
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     17-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Tidied and more error checking (REVA::MJM)
*     12-APR-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     27-May-1994  Changed explicit min/max values to NUM__MAXR/MINR
*                  from PRIMDAT (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for OUTSETSUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                 ! image dimensionality
      PARAMETER ( NDIMS  =  2 ) ! 2-d images only

      REAL
     :    MAXVAL,               ! maximum value allowed for substitution
     :    MINVAL                ! minimum   "      "     "       "
      PARAMETER( MAXVAL  = NUM__MAXR ) ! maximum real
      PARAMETER( MINVAL  = NUM__MINR ) ! minimum real

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),       ! dimensions of input DATA_ARRAY
     :    NELEMENTS,            ! number of elements mapped by NDF_MAP
     :    NDIM,                 ! number of dimensions from NDF_DIM
     :    PNTRI,                ! pointer to input DATA_ARRAY component
     :    PNTRO,                ! pointer to output DATA_ARRAY component
     :    LOCI,                 ! input data structure
     :    LOCO,                 ! output data structure
     :    XCENTRE,              ! x coordinate of circle centre
     :    YCENTRE               ! y     "      "    "      "

      REAL
     :    DIAMETER,             ! diameter of circle to be used
     :    NEWVAL                ! new value for points outside circle


*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check status before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       try to map a data array component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       check status before continuing
         IF ( STATUS .EQ. SAI__OK ) THEN

*          tell user dimensions of input array
            CALL MSG_SETI( 'XDIM', IDIMS(1) )
            CALL MSG_SETI( 'YDIM', IDIMS(2) )
            CALL MSG_OUT( 'INPUT_DIMS',
     : ' Image is ^XDIM by ^YDIM pixels', STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )


*          get x and y coordinates of centre of circle to be used
            CALL AIF_GET0I( 'XCENTRE', NINT(IDIMS(1)/2.0), 0,
     :                       IDIMS(1), XCENTRE, STATUS )
            CALL AIF_GET0I( 'YCENTRE', NINT(IDIMS(2)/2.0), 0,
     :                       IDIMS(2), YCENTRE, STATUS )

*          get the diameter of the circle setting the maximum to be
*          just big enough to encompass the whole image
            CALL AIF_GET0R( 'DIAMETER', 10.0, 0.0,
     :       SQRT(2.0)*MAX( IDIMS(1), IDIMS(2) ), DIAMETER, STATUS )

*          get replacement value for pixels outside defined circle
            CALL AIF_GET0R( 'NEWVAL', 0.0, MINVAL, MAXVAL,
     :                       NEWVAL, STATUS )

*          check for error before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now get an output array to contain modified data
               CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS,
     :                                IDIMS, LOCO, STATUS )

*             check error before continuing
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map an output data array component
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL',
     :                    'WRITE', PNTRO, NELEMENTS, STATUS )

*                check status before accessing pointers
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   now call the subroutine that does the actual work
                     CALL OUTSETSUB( %VAL( PNTRI ), %VAL( PNTRO ),
     :                               IDIMS(1), IDIMS(2), XCENTRE,
     :                               YCENTRE, DIAMETER, NEWVAL,
     :                               STATUS )

*                end of if-no-error-before-accessing-pointers check
                  END IF

*                tidy up the output data structure
                  CALL NDF_ANNUL( LOCO, STATUS )

*             end of if-no-error-after-getting-output check
               END IF

*          end of if-no-error-before-creating-output check
            END IF

*       end of if-no-error-after-mapping-input-data check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
