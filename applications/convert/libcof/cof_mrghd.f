      SUBROUTINE COF_MRGHD( FUNITE, FILE, FUNITM, STATUS )
*+
*  Name:
*     COF_MRGHD

*  Purpose:
*     Merge current and saved primary FITS headers

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_MRGHD( FUNITE, FILE, FUNITM, STATUS )

*  Description:
*     This creates a primary FITS header in memory from the given FITS header
*     and any inherited primary header cards saved in dynamic memory, with
*     a count and pointer in COMMON.
*
*     Header inheritance will only occur if keyword INHERIT is T for the given
*     HDU and there are some save primary header cards.  It is assumed that
*     unwanted primary cards, such as HISTORY and COMMENT will not have been
*     saved.
*
*     A mask is maintained for the saved primary header keywords and the
*     corresponding element set FALSE if a current header keyword overrides
*     the  primary header.  Any primary header keywords which have not been
*     overridden are then added to the new header.

*  Arguments:
*     FUNITE = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        error messages.
*     FUNITM = INTEGER (Returned)
*        FITS unit number of the merged header
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The Given FITS file must already be opened with the FITSIO library.

*  Copyright:
*     Copyright (C) 2000, 2002 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     25-AUG-2000 (AJC):
*        Original version
*     20-DEC-2002 (AJC):
*        Add an END card (required by ffgphd).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF definitions

*  Arguments Given:
      INTEGER FUNITE
      CHARACTER * ( * ) FILE

*  Arguments Returned:
      INTEGER FUNITM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks
*  Global Variables:
      INCLUDE 'F2NDF2_CMN'

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

      INTEGER COMLEN             ! FITS header comment length
      PARAMETER( COMLEN = 80 )

*  Local Variables:
      INTEGER CARD               ! Count of airlock headers written
      INTEGER FSTAT              ! FITSIO status
      INTEGER PMASK              ! Pointer to primary header mask
      INTEGER NHEAD              ! Number of header cards on FUNITE
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name
      LOGICAL THERE              ! If KEYWORD is there
      LOGICAL INHERIT            ! INHERIT keyword value
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      CHARACTER * ( COMLEN ) COMENT ! Header card comment

* Local Data:
      CHARACTER*(HEDLEN) SIMPLE
      DATA SIMPLE/
     :'SIMPLE  =                    T / File conforms to FITS standard'/
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Set default to merged header same as extension header.
      FUNITM = FUNITE

*  See if there's anything to inherit.
      IF ( NPHEAD .GT. 0 ) THEN

*  See if we need to inherit i.e.  INHERIT=T is present.
         CALL COF_GKEYL( FUNITE, 'INHERIT', THERE, INHERIT, COMENT,
     :                   STATUS )

         IF ( (STATUS .EQ. SAI__OK ) .AND. THERE .AND. INHERIT ) THEN

*  Get memory for the primary header mask.
            CALL PSX_CALLOC( NPHEAD, '_LOGICAL', PMASK, STATUS )
            CALL CON_CONSL( .TRUE., NPHEAD, %VAL( CNF_PVAL( PMASK ) ),
     :                      STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Get a free unit number and open a  FITS file in memory.
               CALL FTGIOU( FUNITM, FSTAT )
               CALL FTINIT( FUNITM, 'mem://', 1, FSTAT )
               IF ( FSTAT .NE. FITSOK ) THEN

*  Failed to open new unit.
                  CALL COF_FIOER( FSTAT, 'COF_MRGHD_GHEAD', 'FTGREC',
     :              'Error opening temporary FITS for merged header',
     :              STATUS )

               ELSE

*  Allocate space for the maximum size of the merged header.
                  CALL COF_NHEAD( FUNITE, FILE, NHEAD, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  The first card must be a SIMPLE card as the airlock should look like
*  a primary header.
                     CARD = 1
                     CALL FTPREC( FUNITM, SIMPLE, FSTAT )

                     DO IHEAD = 1, NHEAD

*  Obtain the header.
                        CALL FTGREC( FUNITE, IHEAD, HEADER, FSTAT )

*  Report if anything went wrong.  Go to the point before the end where
*  the extension is tidied.
                        IF ( FSTAT .NE. FITSOK ) THEN
                           CALL MSG_SETI( 'NH', NHEAD + 1 )
                           CALL MSG_SETI( 'IH', IHEAD )
                           CALL MSG_SETC( 'FILE', FILE( :NCF ) )
                           CALL COF_FIOER( FSTAT, 'COF_MRGHD_GHEAD',
     :                       'FTGREC', 'Error obtaining a FITS header '/
     :                       /'(^IH of ^NH) from FITS file ^FILE.',
     :                       STATUS )
                           GOTO 980

*  Ignore extension-only keywords
                        ELSE
                           IF ( .NOT. ( ( HEADER(1:8) .EQ. 'XTENSION' )
!     :                  .OR. ( HEADER(1:8) .EQ. 'EXTNAME' )
!     :                  .OR. ( HEADER(1:8) .EQ. 'EXTVER' )
!     :                  .OR. ( HEADER(1:8) .EQ. 'EXTLEVEL' )
     :                  .OR. ( HEADER(1:8) .EQ. 'INHERIT' )
     :                  .OR. ( HEADER(1:8) .EQ. 'CHECKSUM' )
     :                  .OR. ( HEADER(1:8) .EQ. 'PCOUNT' )
     :                  .OR. ( HEADER(1:8) .EQ. 'GCOUNT' ) ) ) THEN

*  Eliminate similar keywords from the primary header...
                              IF ( HEADER .NE. ' ' ) THEN
                                 CALL COF_CHKP( HEADER(1:8),
     :                                          %VAL(CNF_PVAL(H0_PTR)),
     :                                          NPHEAD,
     :                                          %VAL(CNF_PVAL(PMASK)),
     :                                          STATUS )
                              END IF

*  and write the header to the merged header.  Keep a count of the
*  number of cards written.
                              CARD = CARD + 1
                              CALL FTPREC( FUNITM, HEADER, FSTAT )

                           END IF  ! Not extension-only keyword

                        END IF  ! Header card got OK

                     END DO  ! For each header card

*  Now add anything not eliminated from the primary header.
                     CALL COF_ADDP( FUNITM, %VAL( CNF_PVAL( H0_PTR ) ),
     :                              NPHEAD, %VAL( CNF_PVAL( PMASK) ),
     :                              CARD, STATUS )

*  Free the PMASK memory.
                     CALL PSX_FREE( PMASK )

*  Add the END card.
                     HEADER = 'END'
                     CALL FTPREC( FUNITM, HEADER, FSTAT )


                  END IF  ! Got number of header cards OK

               END IF  ! Created header for merging OK

*  Use a temporary status if something has gone wrong using FITSIO.
*  This lets the END card be written, and other tidying operations.
  980          CONTINUE
               IF ( FSTAT .GT. FITSOK ) STATUS = SAI__OK

            END IF  ! Got memory for mask OK

         END IF  ! Need to merge

      END IF  ! Primary saved

*  Restore the bad status when something went wrong with the FITSIO calls.
      IF ( FSTAT .GT. FITSOK ) STATUS = SAI__ERROR

      END
