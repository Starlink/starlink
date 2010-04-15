*+  MOFF - determines correct x,y spatial and d.c. sky offsets for a pair of
*          mosaic images

      SUBROUTINE MOFF ( STATUS )

*    Description :
*
*     This routine determines the best x,y spatial and d.c. sky offsets
*     for a pair of mosaic images. Starting from guessed initial offsets,
*     the best fit is calculated by minimising the mean square difference
*     between the values measured at (supposedly) the same point on the
*     sky in the two images.
*
*    Invocation :
*
*     CALL MOFF( STATUS )
*
*    Parameters :
*
*     INPIC1  =  IMAGE( READ )
*         First image of mosaic pair
*     INPIC2  =  IMAGE( READ )
*         Second image of mosaic pair
*     NXOFF  =  INTEGER( READ )
*         Nominal x spatial offset between the images
*     NYOFF  =  INTEGER( READ )
*         Nominal y spatial offset between the images
*     BOXSIZE  =  INTEGER( READ )
*         Size of box about nominal offsets which is to be searched for the
*         best fit
*     USEBAD  =  LOGICAL( READ )
*         True if a mask image is to be used to define the fixed bad pixels
*     BADMASK  =  IMAGE( READ )
*         Mask giving positions of fixed bad pixels within the images
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (UKTH::MJM)
*
*    History :
*
*     06-01-1988 : First implementation (UKTH::MJM)
*     Apr-12-1994  DAT and CMP calls changed to NDF (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for MOFF DC/XY BAD/SUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local Constants :

      INTEGER NDIMS           ! input image dimensionality
      PARAMETER ( NDIMS = 2 ) ! 2-d images only

*    Local variables :

      INTEGER
     :    IDIMS1( NDIMS ),    ! dimensions of first input image
     :    IDIMS2( NDIMS ),    !      "      " second  "     "
     :    BDIMS( NDIMS ),     !      "      " bad mask image
     :    NDIM,               ! total number dimensions from NDF_DIM
     :    NELEMENTS,          ! number elements mapped by NDF_MAP
     :    PNTRI1,             ! pointer to first input data array component
     :    PNTRI2,             !    "     " second  "     "    "       "
     :    PNTRB,              !    "     " bad mask      "    "       "
     :    NXOFF,              ! nominal x offset between the images
     :    NYOFF,              !    "    y    "      "     "     "
     :    BOXSIZE,            ! size of box to be searched for best fit (odd)
     :    BESTXOFF,           ! best x offset found
     :    BESTYOFF            !   "  y    "     "

      INTEGER                 ! locators for :
     :    LOCI1,              ! first input data structure
     :    LOCI2,              ! second  "     "      "
     :    LOCB                ! bad mask structure

      REAL
     :    DCOFF               ! calculated d.c. offset between first and second

      LOGICAL                 ! true if :
     :    USEBAD              ! a bad pixel mask to be used

*-
*    check status on entry - if not ok then return
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    get first input image
      CALL GETINP( 'INPIC1', LOCI1, STATUS )

*    get second input image
      CALL GETINP( 'INPIC2', LOCI2, STATUS )

*    check for error before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data array components of the input images
         CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )

         CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )

*       get dimensions of images
         CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, NDIM, STATUS)

         CALL NDF_DIM( LOCI2, NDIMS, IDIMS2, NDIM, STATUS)

*       check for error before continuing
         IF ( STATUS .EQ. SAI__OK ) THEN

*          get the nominal x and y offsets
            CALL AIF_GET0I( 'NXOFF', 0, -(IDIMS1(1)-1), (IDIMS1(1)-1),
     :                       NXOFF, STATUS )
            CALL AIF_GET0I( 'NYOFF', 0, -(IDIMS2(2)-1), (IDIMS1(2)-1),
     :                       NYOFF, STATUS )

*          get size of box about these nominal offsets that is to be
*          searched for the best fit - must be an odd integer
            CALL AIF_GODDN( 'BOXSIZE', 5, 1, 101, BOXSIZE, STATUS )

*          ask if bad pixel handling wanted
            CALL PAR_GET0L( 'USEBAD', USEBAD, STATUS )

*          check for error before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             see if bad pixel handling was requested
               IF ( USEBAD ) THEN

*                try to get the bad pixel mask
                  CALL GETINP( 'BADMASK', LOCB, STATUS )

*                check for error before mapping
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   map the bad pixel mask
                     CALL NDF_MAP( LOCB, 'DATA', '_REAL', 'READ',
     :                              PNTRB, NELEMENTS, STATUS )

                     CALL NDF_DIM( LOCB, NDIMS, BDIMS, NDIM, STATUS)

*                   check for error before accessing working subroutine
                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      call bad pixel handling version of working
*                      subroutine that calculates
*                      the x,y spatial offset
                        CALL MOFFXYBAD( %VAL( PNTRI1 ), IDIMS1(1),
     :                          IDIMS1(2), %VAL( PNTRI2 ), IDIMS2(1),
     :                          IDIMS2(2), NXOFF, NYOFF, BOXSIZE,
     :                          %VAL( PNTRB ), BDIMS(1), BDIMS(2),
     :                          BESTXOFF, BESTYOFF, STATUS )

*                      next call the bad pixel version of the subroutine
*                      that calculates the d.c. sky offset from the first
*                      to second image
                        CALL MOFFDCBAD( %VAL( PNTRI1 ), IDIMS1(1),
     :                           IDIMS1(2), %VAL( PNTRI2 ), IDIMS2(1),
     :                           IDIMS2(2), BESTXOFF, BESTYOFF,
     :                           %VAL( PNTRB ), BDIMS(1), BDIMS(2),
     :                           DCOFF, STATUS )

*                      write out the results on return
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                        CALL MSG_SETI( 'BXOFF', BESTXOFF )
                        CALL MSG_SETI( 'BYOFF', BESTYOFF )
                        CALL MSG_OUT( 'MOFF_RES1',
     :                   ' Best fit x,y offsets were ^BXOFF, ^BYOFF',
     :                    STATUS )
                        CALL MSG_SETR( 'DCOFF', DCOFF )
                        CALL MSG_OUT( 'MOFF_RES2',
     :                    ' Sky offset using these offsets is ^DCOFF',
     :                    STATUS )
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                      put offsets into parameter system
                        CALL PAR_PUT0I( 'XOFF', BESTXOFF, STATUS)
                        CALL PAR_PUT0I( 'YOFF', BESTYOFF, STATUS)
                        CALL PAR_PUT0R( 'DCOFF', DCOFF, STATUS)

*                   end of
*               if-no-error-before-calling-bad-pixel-subroutine check
                     END IF

*                end of if-no-error-before-mapping-bad-pixel-mask check
                  END IF

*                tidy bad pixel mask
                  CALL NDF_ANNUL( LOCB, STATUS )

*             else no bad pixel handling was requested
               ELSE

*                call the subroutine that does
*                the x,y spatial offset work
                  CALL MOFFXYSUB( %VAL( PNTRI1 ), IDIMS1(1),
     :               IDIMS1(2), %VAL( PNTRI2 ), IDIMS2(1), IDIMS2(2),
     :               NXOFF, NYOFF, BOXSIZE, BESTXOFF, BESTYOFF,
     :               STATUS )

*                calculate the d.c. sky offset using this x,y offset
                  CALL MOFFDCSUB( %VAL( PNTRI1 ), IDIMS1(1),
     :               IDIMS1(2), %VAL( PNTRI2 ), IDIMS2(1), IDIMS2(2),
     :               BESTXOFF, BESTYOFF, DCOFF, STATUS )

*                write out the results on return
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_SETI( 'BXOFF', BESTXOFF )
                  CALL MSG_SETI( 'BYOFF', BESTYOFF )
                  CALL MSG_OUT( 'MOFF_RES1',
     :               ' Best fit x,y offsets were ^BXOFF, ^BYOFF',
     :                STATUS )
                  CALL MSG_SETR( 'DCOFF', DCOFF )
                  CALL MSG_OUT( 'MOFF_RES2',
     :               ' Sky offset using these offsets is ^DCOFF',
     :                STATUS )
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                put offsets into parameter system
                  CALL PAR_PUT0I( 'XOFF', BESTXOFF, STATUS)
                  CALL PAR_PUT0I( 'YOFF', BESTYOFF, STATUS)
                  CALL PAR_PUT0R( 'DCOFF', DCOFF, STATUS)

*             end of if-bad-pixel-handling-wanted check
               END IF

*          end of if-no-error-after-getting-nominal-offsets check
            END IF

*       end of if-no-error-after-mapping-input-images check
         END IF

*    end of if-no-error-after-getting-input-images check
      END IF

*    tidy the input images
      CALL NDF_ANNUL( LOCI1, STATUS )
      CALL NDF_ANNUL( LOCI2, STATUS )


*    end
      END
