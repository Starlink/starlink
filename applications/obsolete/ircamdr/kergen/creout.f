*+  CREOUT - creates and returns a locator to an IMAGE type structure
      SUBROUTINE CREOUT( PARNAM, TLENAM, NDIM, DIMS, LOCAT, STATUS )
*    Description :
*     An IMAGE type data structure, associated with the parameter name in
*     PARNAM, is created. A locator for this structure is returned in LOCAT.
*     A TITLE component, associated with the parameter name in TLENAM, is
*     created within the structure and a character value, up to 72 characters
*     long, is obtained from the parameter system and written to the TITLE
*     component. A DATA_ARRAY component is created, this has dimensionality
*     NDIM and dimensions DIMS( NDIM ) and is of type _REAL.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL CREOUT( PARNAM, LABNAM, NDIM, DIMS, LOCAT, STATUS )
*    Parameters :
*     PARNAM = CHAR*(*)( READ )
*           Parameter name associated with the IMAGE type structure to be
*           created.
*     TLENAM = CHAR*(*)( READ )
*           Parameter name associated with the TITLE component created in the
*           new IMAGE type structure.
*     NDIM   = INTEGER( READ )
*           Dimensionality of the DATA_ARRAY component of the new IMAGE type
*           structure.
*     DIMS( NDIM ) = INTEGER( READ )
*           Dimensions of the DATA_ARRAY component of the new IMAGE type
*           structure.
*     LOCAT  = INTEGER( WRITE )
*           Locator to the new IMAGE type structure.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Create a new IMAGE type data structure associated with the parameter
*          name PARNAM.
*        Get a locator, LOCAT, to this IMAGE type structure.
*        If no errors so far then
*           Get a character string associated with parameter name TLENAM.
*           If reponse was not null then
*              Write this character string to the structure as TITLE component.
*           Endif
*           Create a DATA_ARRAY component of dimensions DIMS( NDIM ).
*           If error has occured while creating TITLE or DATA_ARRAY then
*              Annul the locator LOCAT
*           Endif
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     01/12/1983 : Original version                       (ROE::ASOC5)
*     20/02/1983 : Handles null response to TITLE request (ROE::ASOC5)
*     10-Mar-94    Changed DAT_ and CMP_ calls to NDF_ (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Import :
      CHARACTER*(*)
     :  PARNAM, ! parameter name associated with new data structure
     :  TLENAM  !     "       "       "       "  label for new structure
      INTEGER
     :  NDIM, ! dimensionality of DATA_ARRAY component of new data structure
     :  DIMS( NDIM ) ! dimensions of DATA_ARRAY component

*    Export :
      INTEGER
     :  LOCAT ! locator for the new data structure
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  LBND( NDF__MXDIM) ! Lower bound dimensions, set to 1

      DATA  LBND / NDF__MXDIM * 1 /
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       create the new IMAGE type structure

         CALL NDF_CREAT( PARNAM, '_REAL', NDIM, LBND, DIMS, LOCAT,
     :                   STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          create the TITLE component and write TITLE to it
*          A null value will be interpreted as
*          'don't change value', status will not be set

            CALL NDF_CINP( TLENAM, LOCAT, 'TITLE', STATUS )

         ENDIF

*       if either an abort was given in response to the request for a TITLE
*       or something else has gone wrong should annul the locator
         IF( STATUS .NE. SAI__OK ) THEN

            CALL NDF_ANNUL( LOCAT, STATUS )

         ENDIF

      ENDIF

      END
