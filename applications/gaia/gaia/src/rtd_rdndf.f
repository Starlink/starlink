      SUBROUTINE RTD_RDNDF( NAME, TYPE, ID, NX, NY, IPHEAD, NHEAD,
     :                      STATUS )
*+
*  Name:
*     RTD_RDNDF

*  Purpose:
*     Return size, type and header information about an NDF

*  Language:
*     Fortran-77

*  Invocation:
*     CALL RTD_RDNDF( NAME, TYPE, ID, NX, NY, IPHEAD, NHEAD, STATUS )

*  Description:
*     This routine attempts to open an NDF using its full name (this
*     avoids the parameter system and still allows NDF to perform on the
*     fly conversions). The routine returns the NDF identifier,
*     the type of the data as a known rtd code (these approximately
*     match those of FITS), the size of the image and a FITS header that
*     describes the NDF (this includes the origin information and the
*     NDF identifier). If present any AST FrameSet information (stored
*     as part of a WCS component) in the NDF will be added to the FITS
*     headers as a native encoding. This should then be accessed using a
*     FITS channel with appropriate filters. The image data is not
*     returned (this should be accessed using the NDF identifier by
*     another routine). 

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the NDF to be opened.
*     TYPE = INTEGER (Returned)
*        The data type of the image that is returned. This will be one
*        of the codes:
*            8 = byte image
*           16 = signed word
*          -16 = unsiged word
*           32 = integer
*          -32 = floating point
*        Obviously these types limit those that an NDF can have. Note
*        that type -8, which RTD interprets as X image data (mapped to
*        unsigned byte) is returned as _WORD (16) data to avoid display
*        problems and _DOUBLE is returned as _REAL (-32) (if possible).
*     ID = INTEGER (Returned)
*        The NDF identifier. Use this to access the data and annul
*        access when required.
*     NX = INTEGER (Returned)
*        The X dimnesion of the NDF.
*     NY = INTEGER (Returned)
*        The Y dimension of the NDF.
*     IPHEAD = INTEGER (Returned)
*        Pointer to a character array containing the FITS items. There
*        are NHEAD elements, each of which is 80 characters long.
*     NHEAD = INTEGER (Returned)
*        The maximum number of items in IPHEAD.
*     STATUS = INTEGER (Write)
*        The global status on exit.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1996 (PDRAPER):
*        Original version.
*     7-MAR-1996 (PDRAPER):
*        Added FITS header code.
*     28-JUN-1996 (PDRAPER):
*        Changed to return pointer to image, rather then a copy. RTD now
*        makes a copy of its own. Will need to deal with BAD pixels in
*        this code now.
*     22-NOV-1996 (PDRAPER):
*        Converted to return a pointer to the FITS information. Also
*        more sophisticated FITS headers.
*     16-JAN-1997 (PDRAPER):
*        Removed the ability to return a pointer to the image data. Now
*        returns the NDF identifier instead. This is now used to copy the
*        image data by chunking at a later time.
*     16-SEP-1997 (PDRAPER):
*        Added code to deal with NDF slice information.
*     15-NOV-1997 (PDRAPER):
*        Added changes to support AST WCS systems.
*     18-DEC-1997 (PDRAPER):
*        Converted to return _UBYTE data as _WORD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'         ! NDF parameters
      INCLUDE 'PRM_PAR'         ! Primitive constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER TYPE
      INTEGER ID
      INTEGER NX
      INTEGER NY
      INTEGER IPHEAD
      INTEGER NHEAD

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Type of NDF data array
      INTEGER AVAIL             ! Number of cards available in header
      INTEGER DIM( NDF__MXDIM ) ! NDF dimensions
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position of .sdf in name
      INTEGER IWCS              ! NDF WCS component identifer
      INTEGER NAMLEN            ! Length of name string
      INTEGER NDIM              ! Number of NDF dimensions
      INTEGER PLACE             ! NDF placeholder (not used)
      LOGICAL EXISTS            ! WCS system exists
      LOGICAL FIRST             ! TRUE when seeking first significant
                                ! dimension. 
*.

*  Set the global status.
      STATUS = SAI__OK

*  Remove the .sdf part of the name (if it exists), being careful to
*  retain the slice information.
      NAMLEN = CHR_LEN( NAME )
      IAT = INDEX( NAME, '.sdf' )
      IF ( IAT .NE. 0 ) THEN

*  Special case is .sdf.something (like a compressed foreign
*  datatype). Need to preserve the whole name in this case.
         IF ( INDEX( NAME, '.sdf.' ) .EQ. 0 ) THEN 
            DO 1 I = IAT, MAX( IAT + 4, NAMLEN - 4 )
               NAME( I : I ) = NAME( I + 4  : I + 4 )
 1          CONTINUE
            NAME( I : ) = ' '
            IAT = I
         ELSE
            IAT = NAMLEN
         END IF
      ELSE 
         IAT = NAMLEN
      END IF
      IF ( IAT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'No name given', STATUS )
      END IF

*  Attempt to open the NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME( :IAT ), 'READ', 'OLD', ID,
     :               PLACE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the size of the NDF.
         CALL NDF_DIM( ID, NDF__MXDIM, DIM, NDIM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set NX and NY (note these are the first significant dimension,
*  followed by the product of the others.
            NX = 1
            NY = 1
            FIRST = .TRUE.
            DO 2 I = 1, NDIM
               IF ( DIM( I ) .GT. 1 .AND. FIRST ) THEN 
                  NX = DIM( I )
                  FIRST = .FALSE.
               ELSE
                  NY = NY * DIM( I )
               END IF
 2          CONTINUE

*  Success. Now get the data type of the NDF.
            CALL NDF_TYPE( ID, 'DATA', DTYPE, STATUS )

*  Need to change to an RTD known type.
            IF ( DTYPE .EQ. '_REAL' ) THEN
               TYPE = -32
            ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
               TYPE = -32
               DTYPE = '_REAL'
            ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
               TYPE = 32
            ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
               TYPE = 16
            ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
               TYPE = -16
            ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
               TYPE = 8
            ELSE IF ( DTYPE .EQ. '_UBYTE' ) THEN
               TYPE = 16
               DTYPE = '_WORD'
            END IF

*  Now set up the FITS information to describe the world coordinate
*  system.
            CALL RTD1_CRFIT( ID, TYPE, NHEAD, IPHEAD, AVAIL, STATUS )
         END IF
      END IF

*  Note on exit and errors are pending and these should be dealt with by
*  the calling routine, which should also set ERR_MARK and ERR_RLSE.
      END
