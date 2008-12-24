      SUBROUTINE DGCRA3( NDFID, PXNAME, PXTYPE, PQNAME, PCOMNT, QNAME,
     :                   LOCS, QADD, STATUS )
*+
*  Name:
*     DGCRA3

*  Purpose:
*     Return locators to quality information of input NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DGCRA3( NDFID, PXNAME, PXTYPE, PQNAME, PCOMNT, QNAME,
*                  LOCS, QADD, STATUS )

*  Description:
*     This subroutine first searches the input NDF to see if it has
*     an extension containning the quality information of its samples.
*     If such extension does not exit in the NDF, a new one will be
*     created with its name and type obtained from the environment. Once
*     the extension has been located, the supplied quality name is added
*     into the extansion (if not there yet ).

*  Arguments:
*     NDFID = INTEGER (Given)
*        The ID of the input NDF.
*     PXNAME = CHARACTER (Given)
*        The name of the parameter used to get the name of the extension
*        containning quality information about the samples in the input
*        NDF.
*     PXTYPE = CHARACTER (Given)
*        The name of the parameter used to get the type of the extension
*        containning quality information about the samples in the inut
*        NDF.
*     PQNAME = CHARACTER (Given)
*        The name of the parameter used to get the name of the quality
*        to be added to the quality information extension.
*     PCOMNT = CHARACTER (Given)
*        The name of the parameter used to get the comment associated
*        with the added quality name.
*     QNAME = CHARACTER (Returned)
*        Quality name added to the quality information extension of the
*        NDF.
*     LOCS( 5 ) = CHARACTER (Returned)
*        Locators to the quality information of the NDF.
*     QADD = LOGICAL (Returned)
*        The flag showing that the given quality name has added to the
*        input NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT package constants
      INCLUDE 'MSG_PAR'          ! MSG package constants
      INCLUDE 'IRQ_PAR'          ! IRQ package constants

*  Arguments Given:
      INTEGER NDFID
      CHARACTER*( * ) PXNAME
      CHARACTER*( * ) PXTYPE
      CHARACTER*( * ) PQNAME
      CHARACTER*( * ) PCOMNT

*  Arguments Returned:
      CHARACTER*( * ) QNAME
      CHARACTER*( * ) LOCS
      LOGICAL QADD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! Bit used to storing quality
      CHARACTER*( IRQ__SZCOM ) COMNT ! Comment of quality name
      LOGICAL FIXED              ! True if quality is fixed for all 
      LOGICAL FOUND              ! Found the extension flag
      LOGICAL VALUE              ! Value of a fixed quality
      CHARACTER*( DAT__SZLOC ) XLOC  ! Locator to the extension
      CHARACTER*( DAT__SZNAM ) XNAME ! Name of the extension
      CHARACTER*( DAT__SZTYP ) XTYPE ! Type of the extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search the input NDF for an extension containning quality information
      CALL IRQ_FIND( NDFID, LOCS, XNAME, STATUS )

*  If such quality information was found, tell the user conditionally.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL MSG_SETC( 'X', XNAME )
         CALL MSG_OUTIF( MSG__VERB, 'DGCRA3_MSG1',
     :      '   Quality information will be put in NDF extension "^X".',
     :                   STATUS )

*  If no such extension was found, create one with the name and type
*  specified by the user.
      ELSE
         CALL ERR_ANNUL( STATUS )
         CALL PAR_GET0C( PXNAME, XNAME, STATUS )

*  See there is any extension in NDF has this name.
         CALL NDF_XSTAT( NDFID, XNAME, FOUND, STATUS )

*  If not, create it.
         IF ( .NOT.FOUND ) THEN
            CALL PAR_DEF0C( PXTYPE, XNAME, STATUS )
            CALL PAR_GET0C( PXTYPE, XTYPE, STATUS )
            CALL NDF_XNEW( NDFID, XNAME, XTYPE, 0, 0, XLOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
         END IF

*  Convert this structrue to hold quality information.
         CALL IRQ_NEW( NDFID, XNAME, LOCS, STATUS )

      END IF

*  If error happened, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain a quality name from the user.
      CALL PAR_GET0C( PQNAME, QNAME, STATUS )
      
*  See if it has been there in the quality information extension of the
*  input NDF.
      CALL IRQ_GETQN( LOCS, QNAME, FIXED, VALUE, BIT, COMNT, STATUS )

*  If it is not there yet, add it to the extension with a comment.
      IF ( STATUS .NE. SAI__OK ) THEN
         QADD = .TRUE.
         CALL ERR_ANNUL( STATUS )
         CALL PAR_GET0C( PCOMNT, COMNT, STATUS )
         CALL IRQ_ADDQN( LOCS, QNAME, .FALSE., COMNT, STATUS )

*  Conditionally report to the user about this new quality name.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_SETC( 'N', QNAME )
         CALL NDF_MSG( 'NDF', NDFID )
         CALL MSG_OUTIF( MSG__NORM, 'DGCRA3_MSG2',
     :        '   Quality "^N" added to the CRDD file to flag '/
     :          /'samples in glitches', STATUS )

*  Set flag to show quality name is added to the NDF.
         QADD = .TRUE.
      
*  If the name was there. report to the user.
      ELSE
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_SETC( 'N', QNAME )
         CALL MSG_SETC( 'COM', COMNT )
         CALL MSG_OUTIF( MSG__NORM, 'DGCRA3_MSG2',
     :        '   Use pre-defined quality "^N" - ^COM, to flag samples'/
     :       /' in glitches.', STATUS )

*  Set the flag show no new quality name is added to the NDF.
         QADD = .FALSE.
      
      END IF
      
 999  CONTINUE

      END
