      SUBROUTINE PSA1_NDFIN4( IPOINT, NX, NY, ORIGX, ORIGY, BAD,
     :                        STATUS )
*+
*  Name:
*     PSA1_NDFIN4

*  Purpose:
*     To access and map in an INTEGER NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_NDFIN4( IPOINT, NX, NY, ORIGX, ORIGY, BAD, STATUS )

*  Description:
*     The routine accesses a _INTEGER 2D NDF. If required the input can be
*     sectioned to a subset of the input data. PISA specific as sets up
*     PISA common block entries.

*  Arguments:
*     IPOINT = INTEGER (Returned)
*        Pointer to the accessed data.
*     NX = INTEGER (Returned)
*        Size of the first NDF dimension (PISA format)
*     NY = INTEGER (Returned)
*        Size of the second NDF dimension (PISA format)
*     ORIGX = INTEGER (Returned)
*        X Origin of pixel array (STARLINK coords)
*     ORIGY = INTEGER (Returned)
*        Y Origin of pixel array (STARLINK coords)
*     BAD = LOGICAL (Returned)
*        True if BAD pixels are present.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-SEP-1991 (PDRAPER):
*        Original version.
*     17-JUN-1995 (PDRAPER):
*        Changed to access _INTEGER data and not complain about BAD pixels.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF buffer size constants

*  Global Variables:
      INTEGER ISTART, ISTOP, NWORD, IXL, IXH
      COMMON /FIL/ ISTART, ISTOP, NWORD, IXL, IXH
*      ISTART,ISTOP - y extent of array
*      NWORD - number of pixels in y direction ?
*      IXL,IXH - x extent of array


*  Arguments Returned:
      INTEGER IPOINT
      INTEGER NX
      INTEGER NY
      INTEGER ORIGX
      INTEGER ORIGY
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL PRALL
      CHARACTER * ( NDF__SZTYP ) NTYPE
      INTEGER DIML( 2 ), DIMU( 2 ), IDIM( 2 ), IDIN, IDINS, ITMP,
     :        IVAL( 2 ), LBND( 2 ), UBND( 2 ), NDIM, NEL, NVAL

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the input NDF
      CALL NDF_ASSOC( 'IN', 'READ', IDIN, STATUS )

*  Warn users if input is NOT INTEGER*2
      CALL NDF_TYPE( IDIN, 'Data', NTYPE, STATUS )
      IF ( NTYPE .NE. '_INTEGER' .AND. NTYPE .NE. '_WORD' ) THEN
         CALL MSG_SETC( 'NTYPE', NTYPE )
         CALL MSG_OUT( ' ',
     :' Input NDF is of type ^NTYPE - this application can only'//
     :' process using INTEGER values; significance may be lost',
     :  STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*   Inquire the size of the data array
      CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
      IDIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      IDIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*   Assign the sizes to the output variables
      NX = IDIM( 1 )
      NY = IDIM( 2 )
      ORIGX = LBND( 1 )
      ORIGY = LBND( 2 )
      NWORD = NX

*   Does the user want to process all the area
      CALL PAR_GET0L( 'PRALL', PRALL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*   If a sub-array is required then get the pixel coordinates
      IF ( .NOT. PRALL ) THEN

*   Inform the user of the image size and inquire the limits for
*   analysis
         CALL MSG_SETI( 'LBND', LBND( 1 ) )
         CALL MSG_SETI( 'UBND', UBND( 1 ) )
         CALL MSG_OUT( ' ', ' X-extent of image is ^LBND:^UBND',
     :                 STATUS )
         CALL MSG_SETI( 'LBND', LBND( 2 ) )
         CALL MSG_SETI( 'UBND', UBND( 2 ) )
         CALL MSG_OUT( ' ', ' Y-extent of image is ^LBND:^UBND',
     :                 STATUS )

*   X-direction. Give the full array size as dynamic defaults
         IVAL( 1 ) = LBND( 1 )
         IVAL( 2 ) = UBND( 1 )
         CALL PAR_DEF1I( 'XPIXS', 2, IVAL, STATUS )
         CALL PAR_GET1I( 'XPIXS', 2, IVAL, NVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 99
         ENDIF
         IXL = IVAL( 1 )
         IXH = IVAL( 2 )

*   Y-direction. Give the full array size as dynamic defaults
         IVAL( 1 ) = LBND( 2 )
         IVAL( 2 ) = UBND( 2 )
         CALL PAR_DEF1I( 'YPIXS', 2, IVAL, STATUS )
         CALL PAR_GET1I( 'YPIXS', 2, IVAL, NVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
             GO TO 99
         ENDIF
         ISTART = IVAL( 1 )
         ISTOP = IVAL( 2 )

*   Check the limits
         IF ( IXL .LT. LBND( 1 ) ) IXL = LBND( 1 )
         IF ( IXH .GT. UBND( 1 ) ) IXH = UBND( 1 )
         IF ( ISTART .LT. LBND( 2 ) ) ISTART = LBND( 2 )
         IF ( ISTOP .GT. UBND( 2 ) ) ISTOP = UBND( 2 )


*   Calculate size of subarray
         NX = IXH - IXL + 1
         NY = ISTOP - ISTART + 1

*   Get an identifier to the subarray
         DIML( 1 ) = IXL
         DIML( 2 ) = ISTART
         DIMU( 1 ) = IXH
         DIMU( 2 ) = ISTOP
         CALL NDF_SECT( IDIN, 2, DIML, DIMU, IDINS, STATUS )

      ELSE
*   Use all the frame

         CALL MSG_OUT( ' ', ' Analysing whole image', STATUS )
         IXL = LBND( 1 )
         IXH = UBND( 1 )
         ISTART = LBND( 2 )
         ISTOP = UBND( 2 )
         IDINS = IDIN
      ENDIF

*   Now Map in the data
      CALL NDF_MAP( IDINS, 'Data', '_INTEGER', 'READ', IPOINT, NEL,
     :              STATUS )

*  Check for BAD pixels - need to remove these at some point.
      CALL NDF_BAD( IDINS, 'Data', .TRUE., BAD, STATUS )
      IF ( BAD ) THEN
         CALL MSG_OUT( 'HAS_BAD_PIX ',
     :                 ' Warning: input data contain "bad" pixels.',
     :                 STATUS )
      END IF

*   Reverse the order of NX and NY for subsequent processing
      ITMP = NX
      NX = NY
      NY = ITMP

 99   CONTINUE
      END
