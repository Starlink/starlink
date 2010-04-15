*+  SHSIZE - output the dimensions of an image (upto 3-d)

      SUBROUTINE  SHSIZE ( STATUS )

*    Description :
*
*     This routine merely outputs the dimensions of an image
*     via the message system.
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           Input image to be tested
*
*    Method :
*
*     Check for error on entry - return if not ok
*     Get locator to structure containing image
*     If no error then
*        See if there is a data array component in structure
*        If there is
*           Get its shape
*           Output dimensions to user
*        Else
*           Output error message
*        Endif
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean (REVA::MJM)
*
*    History :
*
*     12-11-1985 : First implementation (REVA::MJM)
*     27-06-1986 : Revised to use CMP_SHAPE (REVA::MJM)
*     13-04-1989 : Added setting of globals with image size (JACH::CAA)
*     12-AApr-1994 Changed DAT, CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE parameters
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local constants :

      INTEGER
     :    NDIMS                 ! dimensionality of input images
      PARAMETER ( NDIMS  =  3 ) ! upto 3-d arrays allowed

*    Local variables :

      INTEGER
     :    LOCI,                 ! locator for input IMAGE structure
     :    DIMS( NDIMS ),        ! dimensions of input DATA_ARRAY
     :    ACTDIM                ! actual number of dims in DATA_ARRAY

      LOGICAL                   ! true if :
     :    THERE                 ! a DATA_ARRAY component exists in structure


*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       see if there is a DATA_ARRAY component inside the structure
         CALL NDF_STATE( LOCI, 'DATA', THERE, STATUS )

*       if there is then continue
         IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN

*          get the shape of the DATA_ARRAY component
            CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*          write out the dimensionality to the environment
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_SETI( 'ACTDIM', ACTDIM )
            CALL MSG_OUT( 'ACT_DIMS',
     : ' Image is ^ACTDIM dimensional : ', STATUS )

*          write out first dimension
            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_OUT( 'X_DIM',
     : ' First  dimension has ^XDIM pixels', STATUS )

*          set first dimension value in global
	    CALL PAR_PUT0I( 'XDIM', DIMS( 1), STATUS)

*          if more than one dimension
            IF ( ACTDIM .GT. 1 ) THEN

*             write out second dimension
               CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
               CALL MSG_OUT( 'Y_DIM',
     : ' Second dimension has ^YDIM pixels', STATUS )

*          set second dimension value in global
	    CALL PAR_PUT0I( 'YDIM', DIMS( 2), STATUS)

*             if more than two dimensions
               IF ( ACTDIM .GT. 2 ) THEN

*                write out third dimension
                  CALL MSG_SETI( 'ZDIM', DIMS( 3 ) )
                  CALL MSG_OUT( 'Z_DIM',
     : ' Third  dimension has ^ZDIM pixels', STATUS )

*             end of if-more-than-two-dimensions check
               END IF

*          end of if-more-than-one-dimension check
            END IF

            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       else no DATA_ARRAY component in structure
         ELSE
*          report this
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP( 'NO_DATA',
     : ' No DATA_ARRAY in given structure - sorry !', STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE
               CALL MSG_OUT('NO_DATA',
     : ' No DATA_ARRAY in given structure - sorry !', STATUS )
            END IF
*       end of if-there-is-a-DATA_ARRAY-in-structure check
         END IF

*    end of if-no-error-after-getting-input-structure check
      END IF

*    tidy up the structure
      CALL NDF_ANNUL( LOCI, STATUS )


*    end
      END
