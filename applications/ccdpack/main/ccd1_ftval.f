      SUBROUTINE CCD1_FTVAL( FTSKEY, INDF, FTSVAL, STATUS )
*+
*  Name:
*     CCD1_FTVAL

*  Purpose:
*     Get FITS value for given keyword from FITS extension of an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FTVAL( FTSKEY, INDF, FTSVAL, STATUS )

*  Description:
*     This routine attempts to retrieve the value of a FITS header with
*     a specified FITS keyword, from the FITS extension of an NDF.
*     The first card which matches the given keyword is used.
*     If no FITS extension is found, or no matching header is found then
*     an error message is output and STATUS is set.
*
*     The value returned is a string representation of the value 
*     requested.

*  Arguments:
*     FTSKEY = CHARACTER * ( * ) (Given)
*        Name of the FITS header keyword.
*     INDF = INTEGER (Given)
*        ID of the NDF containing the FITS extension.
*     FTSVAL = CHARACTER * ( * ) (Returned)
*        Character representation of the value of the keyword requested.
*        If the card's value is of character type, it is returned 
*        surrounded by single quotes.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     02-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data system constants
      
*  Arguments Given:
      CHARACTER * ( * ) FTSKEY
      INTEGER INDF
      
*  Arguments Returned:
      CHARACTER * ( * ) FTSVAL 

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( 70 ) CVAL     ! Character value of selected FITS card
      INTEGER ICARD               ! Index of target card
      INTEGER IPFITS              ! Pointer to FITS array
      INTEGER LENGTH              ! Length of FITS array elements
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator for FITS extension
      LOGICAL LVAL                ! Logical value of selected FITS card
      REAL RVAL                   ! Real value of selected FITS card
      INTEGER NCARD               ! Number of cards in mapped FITS array
      INTEGER NCHAR               ! Number of characters in conversion
      LOGICAL THERE               ! Whether requested item is present
      
*.

*  Set default return value.
      FTSVAL = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if FITS extension does not exist.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL NDF_ACMSG( 'NDF', INDF )
         CALL CCD1_ERREP( ' ', '  No FITS extension in ^NDF', STATUS )
         GO TO 99
      END IF

*  Find and map FITS extension.
      CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
      CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', IPFITS, NCARD, STATUS )
      LENGTH = 80
      IF ( STATUS .NE. SAI__OK ) GO TO 99
   
*  Defer delivery of error messages.
      CALL ERR_MARK

*  In the following calls to FTS1_GKEY<T> look out for fortran magic 
*  getting the FITS character array lengths passed. 

*  Attempt to get numerical value for named keyword.
      CALL FTS1_GKEYR( NCARD, %VAL( IPFITS ), 1, FTSKEY, THERE, RVAL,
     :                 ICARD, STATUS, %VAL( LENGTH ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         CALL CHR_RTOC( RVAL, FTSVAL, NCHAR )
         GO TO 1
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Attempt to get logical value for named keyword.
      CALL FTS1_GKEYL( NCARD, %VAL( IPFITS ), 1, FTSKEY, THERE, LVAL,
     :                 ICARD, STATUS, %VAL( LENGTH ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         CALL CHR_LTOC( LVAL, FTSVAL, NCHAR )
         GO TO 1
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Attempt to get character value for named keyword.
      CALL FTS1_GKEYC( NCARD, %VAL( IPFITS ), 1, FTSKEY, THERE, CVAL,
     :                 ICARD, STATUS, %VAL( LENGTH ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         FTSVAL = '''' // CVAL
         FTSVAL( CHR_LEN( FTSVAL ) + 1: ) = ''''
         GO TO 1 
      ELSE IF ( STATUS .NE. SAI__OK ) THEN 
         CALL ERR_ANNUL( STATUS )
      END IF

*  No conversion could be made.
      STATUS = SAI__ERROR
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_SETC( 'HEAD', FTSKEY )
      CALL ERR_REP( 'CCD1_FTVAL', 
     :'  Failed to find value for FITS header ^HEAD in file ^NDF', 
     :                 STATUS )

*  Release error context.
 1    CONTINUE
      CALL ERR_RLSE 

*  Tidy up and exit.
 99   CONTINUE

      CALL DAT_ANNUL( LOC, STATUS )

      END
* $Id$
