*+  MFNEXT - gets next data array and offsets in sequence for mosaic
*            from file

      SUBROUTINE MFNEXT ( FD, PNINPI, LOC, OFFSET, STATUS )

*    Description :
*
*     This routine reads from an already opened ASCII file, the next
*     IMAGE-structure name and its associated offsets for use by mosaic
*     applications. The IMAGE-structure name is associated with a
*     parameter, and the locator to that IMAGE structure is returned
*     along with the offsets.  MFOPEN should be used prior to this
*     routine to open the ASCII file, and read the first five records
*     therein.
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
*     CALL MFNEXT( FD, PNINPI; LOC, OFFSET, STATUS )
*
*    Parameters :
*
*     FD   =  INTEGER( READ )
*         Descriptor associated with the ASCII open file
*     PNINPI  =  CHAR( READ )
*         Parameter with which the next IMAGE structure is to be
*           associated
*     LOC  =  CHAR( WRITE )
*         Locator pointing to next IMAGE structure containing the
*           data array
*     OFFSET( 2 )  =  INTEGER( WRITE )
*         x,y offsets of the next data array from the central data array
*     STATUS  =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Read next image name from file
*     If error then
*        Report context and abort
*     Endif
*     Read offsets from file
*     If error then
*        Report context and abort
*     Endif
*     Get image code associated with the image parameter
*     Push the next image name into that code
*     Call DAT_EXIST to get a locator to the next image
*     If status not ok then tidy, report error and return
*     Close file if error occurred reading the file
*     End
*
*    Deficiencies :
*
*     Uses SUBPAR_ calls directly.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK ( RAL::CUR )
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     29-12-1986 : First implementation (REVA::MJM)
*     1988 May 30: Largely rewritten using FIO package and more error
*                  reporting. Fixed bug (2nd argument was not being
*                  used) (RAL::CUR).
*     1990 Feb 22: Replaced SUBPAR calls by AIF_PTFNM (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants

*    Import :

      INTEGER
     :  FD                     ! Descriptor associated with open file

      CHARACTER*(*)
     :  PNINPI                 ! Parameter to be associated with IMAGE
                               ! structure

*    Export :

      INTEGER
     :  OFFSET( 2 )            ! x,y offsets to array from central array

      CHARACTER*(DAT__SZLOC)   ! locator to :
     :  LOC                    ! image structure

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local Constants :

      INTEGER
     :  NCHLIN                 ! maximum number of characters in a
                               ! an input record
      PARAMETER ( NCHLIN = 132 )

*    Local variables :

      INTEGER
     :  NCHAR,                 ! number of characters in a record
     :  NCO,                   ! Character column counter
     :  NCV,                   ! number of characters in value
     :  SHRIEK                 ! column location of comment indicator

      CHARACTER*80
     :  BUFFER*(NCHLIN),       ! buffer to hold records read
     :  IMNAME                 ! name of next image

      REAL
     :  VR( 2 )                ! Dummy array to hold the values read
                               ! from the file

*-
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Read name of the next IMAGE structure

      CALL FIO_READ( FD, IMNAME, NCHAR, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', IMNAME( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFNEXT_IMNAME',
     :     'MFNEXT: Error whilst getting the name of the next '/
     :     /'IMAGE structure. Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Ignore any comments.

      SHRIEK = INDEX( IMNAME, '!' )
      IF ( SHRIEK .EQ. 0 ) THEN
         NCV = 80
      ELSE
         NCV = SHRIEK - 1
      END IF

*    Read the next record - should be the offsets

      CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*    Extract integer offsets from the string

      NCO = 1
      CALL KPG1_FFRR( BUFFER, 2, NCO, VR, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*       Report the error and context.

         CALL MSG_SETC( 'BUFFER', BUFFER( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFNEXT_OFFSET',
     :     'MFNEXT: Error whilst getting x-y offsets. '/
     :     /'Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Convert the floating-point values to nearest integers as the
*    mosaicking only permits pixel offsets.

      OFFSET( 1 )= NINT( VR( 1 ) )
      OFFSET( 2 )= NINT( VR( 2 ) )

*    Write the image name to the input parameter.

      CALL AIF_PTFNM( PNINPI, IMNAME( :NCV ), STATUS )

*    Call DAT_EXIST to get a locator to the next image - if DAT_ASSOC
*    were used, a failure would reprompt you

      CALL DAT_EXIST( PNINPI, 'READ', LOC, STATUS )

*    Check status on return and jump out if there is an error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_MFNEXT_IMLOC',
     :     'MFNEXT: Error whilst trying to get a locator to the '/
     :     /'next IMAGE structure', STATUS )
         GOTO 990
      END IF

      GOTO 999

*    Come here if there has been any error, close file and flag the
*    error if it came from an internal read.

 990  CONTINUE
      IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
      CALL FIO_CLOSE( FD, STATUS )

 999  CONTINUE

      END

