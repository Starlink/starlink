      SUBROUTINE HDR_IN<T>( PARAM, XNAME, ITEM, COMP, VALUE, STATUS )
*+
*  Name:
*    HDR_INx

*  Purpose:
*     Reads a header item using a specific type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_INx( PARAM, XNAME, ITEM, COMP, VALUE, STATUS )

*  Description:
*     This routine returns the value of a header item. Header items
*     include both FITS header records and package specific extension
*     information. The values of FITS header records are extracted by
*     setting the XNAME argument to the value 'FITS' (or ' ').

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension ('FITS' or ' ' for FITS headers).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the header item.
*     COMP = INTEGER (Given)
*        The component of a multiple FITS header item ('HISTORY' and
*        'COMMENT' items often have many occurrences). The number of
*        components may be queried using the HDR_NUMB routine.
*     VALUE = <COMM> (Given and Returned)
*        The value. This is  unmodified if the item doesn't exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a version of this routine for accessing header items
*     of various types. Replace the "x" in the routine name by C, L, D,
*     R, or I as appropriate. If the requested item isn't of the
*     required type automatic conversion will be performed as
*     appropriate.
*
*     - Item names for any extension type may be hierarchical
*     (i.e. ING.DETHEAD gets the FITS header "ING DETHEAD"; BOUNDS.MAXX
*     gets the value of the MAXX component of the BOUNDS structure in a
*     non-FITS extension).
*
*     - This routine may be used to read the value of an item in
*     the same extension of more than one image dataset at a time by
*     using multiple parameter names. Multiple parameter names are
*     provided as a comma separated list (i.e. 'IN1,IN2,IN3'). Note the
*     extension must exist in all images and that the argument VALUE
*     must be declared as a dimension of size at least the number of
*     parameters in the list, if this option is used.
*
*     - If a header item is not found its associated element of the
*     VALUE argument will remain unchanged. It is therefore important
*     that suitable defaults are assigned to VALUE before calling this
*     routine. The extension, however, must exist.
*
*     - If the image is not already open then this routine will open it
*     for read only access. If you intend to modify any extension items
*     then you should call HDR_MOD before this routine.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-SEP-1994 (PDRAPER):
*        Original version.
*     15-NOV-1994 (PDRAPER):
*        Added changes to allow NDF access by this routine.
*     29-NOV-1995 (PDRAPER):
*        Converted back to readonly access of NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM
      INTEGER COMP

*  Arguments Returned:
      <TYPE> VALUE( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
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
                  IF ( WASNEW ) CALL IMG1_ASSOC( VPAR, 'READ', SLOT, 
     :                                           STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN 

*  Initialise IMG to read the extension (if not already doing so).
                     CALL IMG1_EXINI( SLOT, EXNAM, .FALSE., ESLOT, 
     :                                STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
                     IF ( EXNAM .EQ. 'FITS' ) THEN

*  Need to extract the required item from the FITS character array.
                        CALL IMG1_RDFT<T>( SLOT, ITEM, COMP, 
     :                                     VALUE( NPAR ), STATUS )
                     ELSE

*  Need to locate the named item (which may be hierarchical).
                        CALL IMG1_RDEX<T>( SLOT, ESLOT, ITEM, 
     :                                     VALUE( NPAR ), STATUS )
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
         CALL ERR_REP( 'HDR_INX<T>_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* $Id$
