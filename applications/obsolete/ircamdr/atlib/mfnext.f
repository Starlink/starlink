
*+  MFNEXT - gets next image and offsets in sequence for mosaic from file

      SUBROUTINE MFNEXT ( LUN, PARAM, LOC, OFFSET, STATUS )

*    Description :
*
*     This routine reads from an already opened file, the next image
*     name and its associated offsets for use by mosaic software. The
*     image name is associated with a parameter, and the locator to
*     that image is returned along with the offsets.
*
*     The mosaic file should have the format :
*
*        Mosaic information for Orion K map      ! header
*        central_image                           ! name of central image
*        125                                     ! total no. frames
*        345  229                                ! max x,y offsets
*        -356  -232                              ! min x,y offsets
*        image_2                                 ! subsequent image and
*        35  34                                  ! its offsets
*        image_3
*        36  -33
*        .
*        .
*        .
*        .
*
*    Invocation :
*
*     CALL MFNEXT( LUN, PARAM; LOC, OFFSET, STATUS )
*
*    Parameters :
*
*     LUN  =  INTEGER( READ )
*         Unit number associated with open file
*     PARAM  =  CHAR( READ )
*         Parameter that next image is to be associated with
*     LOC  =  INTEGER( WRITE )
*         Locator pointing to next image structure
*     OFFSET( 2 )  =  INTEGER( WRITE )
*         x,y offsets of next image from central image
*     STATUS  =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Read next image name from file
*     Read offsets from file
*     Get image code associated with the image parameter
*     Push the next image name into that code
*     Call DAT_EXIST to get a locator to the next image
*     Return
*
*    Deficiencies :
*
*     Uses Fortran i/o
*     Uses SUBPAR_ calls direct
*     Uses Vax/VMS LIB$_ calls
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     29-12-1986 : First implementation (REVA::MJM)
*     10-Mar-94    Changed DAT_ calls to NDF_ (SKL@JACH)
*     14-JUL-1994  Changed LIB$ to FIO_ (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'
      INCLUDE  'FIO_PAR'

*    Import :

      INTEGER
     :    LOC,                    ! locator to image structure
     :    LUN                     ! unit number associated with open file

      CHARACTER*80
     :    PARAM                   ! parameter to be associated with image

*    Export :

      INTEGER
     :    OFFSET( 2 )             ! x,y offsets to image from central image


*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    IMCODE                  ! code associated with parameter

      CHARACTER*80
     :    IMNAME                  ! name of next image

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    read the next record - should be the image name
      READ( LUN, '(A)', END=999, ERR=999 ) IMNAME

*    read the next record - should be the offsets
      READ( LUN, *, END=999, ERR=999 ) OFFSET( 1 ), OFFSET( 2 )

*    get the image code associated with the input parameter
      CALL SUBPAR_FINDPAR( 'CURPIC', IMCODE, STATUS )

*    push the next image name at this code
      CALL SUBPAR_PUTNAME( IMCODE, IMNAME, STATUS )

*    call NDF_EXIST to get a locator to the next image - if NDF_ASSOC
*    were used, a failure would reprompt you
      CALL NDF_EXIST( 'CURPIC', 'READ', LOC, STATUS )

*    check for error on return
      IF ( (LOC .EQ. NDF__NOID) .OR.
     :      (STATUS .NE. SAI__OK) ) GOTO 999

*    return
      RETURN


*    come here if there was a problem
999   STATUS  =  SAI__ERROR
      CLOSE( UNIT=LUN )
      CALL FIO_PUNIT( LUN, STATUS )
      END
