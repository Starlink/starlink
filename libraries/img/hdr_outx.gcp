      SUBROUTINE HDR_OUT<T>( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )
*+
*  Name:
*    HDR_OUTx

*  Purpose:
*     Writes a header item using a specific type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_OUTx( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )

*  Description:
*     This routine writes the value of a header item in a specified
*     type. Header items include both FITS header records and package
*     specific extension information. The values of FITS header records
*     are written by setting the XNAME argument to the value
*     'FITS' (or ' ').

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension ('FITS' or ' ' for FITS).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the header item.
*     COMMEN = CHARACTER * ( * ) (Given)
*        If XNAME is 'FITS' then this is used as a comment to enter
*        with the record. Otherwise this is not used.
*     VALUE = <COMM> (Given)
*        The value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a version of this routine for writing header items
*     of various types. Replace the "x" in the routine name by C, L, D,
*     R, or I as appropriate.
*
*     - Item names for any extension type may be hierarchical
*     (i.e. ING.DETHEAD writes the FITS header "ING DETHEAD";
*     BOUNDS.MAXX the value of the MAXX component of the BOUNDS
*     structure in a non-FITS extension). Writing hierarchical records
*     in FITS records is strongly discouraged.
*
*     - This routine may be used to write the value of items in the
*     same extension of more than one image dataset at a time by using
*     multiple parameter names. Multiple parameter names are provided
*     as a comma separated list (i.e. 'IN1,IN2,IN3'). Note that the
*     argument VALUE must be declared as a dimension of size at least
*     the number of parameters in the list, if this option is used.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1-SEP-1994 (PDRAPER):
*        Original version.
*     15-NOV-1994 (PDRAPER):
*        Added changes to allow NDF access using this routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) COMMEN
      <TYPE> VALUE( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      CHARACTER * ( DAT__SZNAM ) EXNAM ! Extension name
      INTEGER ESLOT              ! Extension slot number
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character positiong
      INTEGER NPAR               ! Number of parameters
      INTEGER SLOT               ! Parameter slot number
      LOGICAL CANMOD             ! Can modify NDF
      LOGICAL WASNEW             ! Dummy

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the parameter count.
      NPAR = 0

*  Get a local copy of the extension name. If this is ' ' then assume
*  the user meant 'FITS'
      IF ( XNAME .EQ. ' ' ) THEN
         EXNAM = 'FITS'
      ELSE
         EXNAM = XNAME
         CALL CHR_UCASE( EXNAM )
      END IF

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the list.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. LEN( PARAM ) ) )
     :     THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
         I2 = INDEX( PARAM( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( PARAM )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( PARAM( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the parameter count.
               NPAR = NPAR + 1

*  Validate the parameter and its slot number.
               CALL IMG1_VPAR( PARAM( F: L ), VPAR, STATUS )
               CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency reasons.
                  IF ( WASNEW ) CALL IMG1_ASSOC( VPAR, 'UPDATE', SLOT, 
     :                                           STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN 

*  Check that WRITE access can be used on this extension.
                     CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', CANMOD,
     :                               STATUS )
                     IF ( CANMOD ) THEN 

*  Initialise IMG to write to the extension (if not already doing so).
                        CALL IMG1_EXINI( SLOT, EXNAM, .TRUE., ESLOT,
     :                                   STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS and "normal" extensions require their own methods.
                        IF ( EXNAM .EQ. 'FITS' ) THEN

*  Need to write the item into the FITS character array.
                           CALL IMG1_WRFT<T>( SLOT, ITEM, COMMEN,
     :                                        VALUE( NPAR ), STATUS )
                        ELSE

*  Need to write the named item into a "normal" (primitive) object, this
*  may be hierarchical.
                           CALL IMG1_WREX<T>( SLOT, ESLOT, ITEM,
     :                                        VALUE( NPAR ), STATUS )
                        END IF
                     ELSE IF ( STATUS .EQ. SAI__OK ) THEN 

*  Cannot write to this NDF's extension.
                        STATUS = IMG__NOACC
                        CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                        CALL MSG_SETC( 'EXNAM', EXNAM )
                        CALL ERR_REP( ' ', 'Cannot write header ' //
     :                       'items to the ''^EXNAM'' extension of ' //
     :                       'the image ''^NDF'' (write access is ' //
     :                       'not allowed).', STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the parameter list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank parameter names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'HDR_OUTX<T>_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* $Id$
