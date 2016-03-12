      SUBROUTINE SHOWQUAL( STATUS )
*+
*  Name:
*     SHOWQUAL

*  Purpose:
*     Displays the quality names defined in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SHOWQUAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays a list of all the quality names currently
*     defined within a supplied NDF (see Task SETQUAL). The descriptive
*     comments which were stored with the quality names when they were
*     originally defined are also displayed. An option exists for also
*     displaying the number of pixels which hold each quality.

*  Usage:
*     showqual ndf [count]

*  ADAM Parameters:
*     COUNT = _LOGICAL (Read)
*        If true, then the number of pixels in each NDF which holds
*        each defined quality is displayed. These figures are shown
*        in parentheses between the quality name and associated
*        comment.  This option adds significantly to the run time.  [NO]
*     NDF = NDF (Read)
*        The NDF whose quality names are to be listed.
*     QNAMES( ) = LITERAL (Write)
*        The quality names associated with each bit, starting from the
*        lowest significant bit.  Unassigned bits have blank strings.

*  Examples:
*     showqual "m51,cena" yes
*        This example displays all the quality names currently defined
*        for the two NDFs "m51" and "cena" together with the number of
*        pixels holding each quality.

*  Related Applications:
*     KAPPA: REMQUAL, QUALTOBAD, SETQUAL.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     Copyright (C) 2010, 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     17-JAN-2002 (DSB):
*        Brought into KAPPA.
*     2010-10-04 (TIMJ):
*        SHOWQUAL has no reason to open the input file in UPDATE mode.
*     2011 February 24 (MJC):
*        Add QNAMES output parameter.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! If FIXED is false, then BIT holds
                                 ! the bit number (LSB=1) within the
                                 ! QUALITY component, at which the
                                 ! current quality is stored. If FIXED
                                 ! is true, then BIT is indeterminate.
      LOGICAL CDONE              ! True if a count of pixels holding
                                 ! each quality has been performed.
      CHARACTER COMMNT*(IRQ__SZCOM)! A comment describing the current
                                 ! quality.
      INTEGER CONTXT             ! Search context for IRQ_NXTQN.
      LOGICAL COUNT              ! True if a count of pixels holding
                                 ! each quality is required.
      LOGICAL DONE               ! True if all quality names have been
                                 ! obtained.
      LOGICAL FIXED              ! True if the the current quality is
                                 ! either held or not held by all
                                 ! pixels.
      INTEGER I                  ! Loop counter
      CHARACTER LQNAME( IRQ__QBITS )*(IRQ__SZQNM) ! Quality-name list
      CHARACTER LOCS( 5 )*(DAT__SZLOC) ! Locators for quality name
                                 ! information.
      INTEGER NAMES              ! No. of quality names defined in the
                                 ! NDF.
      INTEGER NDFIN              ! Identifier for input NDF.
      INTEGER NPIX               ! The number of pixels in the input NDF.
      INTEGER NPXSET( IRQ__QBITS )! No. of set pixels in each bit plane
                                 ! of the QUALITY component.
      INTEGER QCOUNT             ! The number of pixels which hold the
                                 ! current quality.
      LOGICAL QI                 ! True if quality names information was
                                 ! found in the input NDF.
      CHARACTER QNAME*(IRQ__SZQNM)! The next quality name.
      LOGICAL VALUE              ! If FIXED is true, then VALUE is true
                                 ! if all pixels hold the quality, and
                                 ! false if no pixels hold the quality.
                                 ! If FIXED is false, then VALUE is
                                 ! indeterminate.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension in whcih quality
                                 ! names are stored.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', NDFIN, STATUS )

*  See if the numbers of pixels which hold each quality are to be
*  displayed.
      CALL PAR_GET0L( 'COUNT', COUNT, STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOC is returned holding a set of
*  5 HDS locators which identify the NDF and various components of the
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  If quality names information was found, find the number of defined
*  quality names.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL IRQ_NUMQN( LOCS, NAMES, STATUS )
         QI = .TRUE.

*  If no quality names information was found, annul the error and set
*  the number of defined names to zero.
      ELSE IF( STATUS .EQ. IRQ__NOQNI ) THEN
         CALL ERR_ANNUL( STATUS )
         NAMES = 0
         QI = .FALSE.
      END IF

*  Initialise returned list of quality names.
      DO I = 1, IRQ__QBITS
         LQNAME( I ) = ' '
      END DO

*  If no quality names are defined give a message.
      IF( NAMES .EQ. 0 ) THEN
         CALL MSG_OUT( 'SHOWQUAL_MSG1', '  No quality name '//
     :                 'definitions found', STATUS )

*  Otherwise...
      ELSE

*  Indicate that no explicit count has yet been made of the number of
*  pixels set in each bit plane of the QUALITY component.
         CDONE = .FALSE.

*  If a count is required, get the total number of pixels in the input
*  NDF.
         IF( COUNT ) CALL NDF_SIZE( NDFIN, NPIX, STATUS )

*  Initialise the context for the IRQ_NXTQN routine and get
*  the first defined quality name.
         CONTXT = 0
         CALL IRQ_NXTQN( LOCS, CONTXT, QNAME, FIXED, VALUE, BIT,
     :                   COMMNT, DONE, STATUS )

         IF ( STATUS .EQ. SAI__OK ) LQNAME( BIT ) = QNAME

*  Loop round displaying each quality name in turn.
         DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  If a count of pixels with each quality is required...
            IF( COUNT ) THEN

*  ...fixed quality names refer to all or none of the pixel in the NDF.
               IF( FIXED ) THEN
                  IF( VALUE ) THEN
                     QCOUNT = NPIX
                  ELSE
                     QCOUNT = 0
                  END IF

*  Variable quality names can differ from pixel to pixel. Therefore an
*  explicit count needs to be made through the QUALITY component. If
*  this has not already been done, do it now.
               ELSE
                  IF( .NOT. CDONE ) THEN
                     CALL IRQ_CNTQ( LOCS, IRQ__QBITS, NPXSET,
     :                              STATUS )
                     CDONE = .TRUE.
                  END IF

*  Find the number of pixels which hold the current quality.
                  QCOUNT = NPXSET( BIT )
               END IF

*  Display the quality name, comment, and count.
               CALL MSG_SETI( 'CNT', QCOUNT )
               CALL MSG_SETC( 'COM', COMMNT )

               IF( .NOT. FIXED ) THEN
                  CALL MSG_SETI( 'B', BIT )
                  CALL MSG_OUT( 'SHOWQUAL_MSG2', '  '//QNAME//
     :                          ' (bit ^B) - "^COM" (^CNT)', STATUS )
               ELSE
                  CALL MSG_OUT( 'SHOWQUAL_MSG3', '  '//QNAME//
     :                          ' - "^COM" (^CNT)', STATUS )
               END IF

*  If no count is required, just display the quality name and comment.
            ELSE
               CALL MSG_SETC( 'COM', COMMNT )
               IF( .NOT. FIXED ) THEN
                  CALL MSG_SETI( 'B', BIT )
                  CALL MSG_OUT( 'SHOWQUAL_MSG4', '  '//QNAME//
     :                          ' (bit ^B) - "^COM"', STATUS )
               ELSE
                  CALL MSG_OUT( 'SHOWQUAL_MSG5', '  '//QNAME//
     :                          ' - "^COM"', STATUS )
               END IF
            END IF

*  Get the next defined quality name and related information.
            CALL IRQ_NXTQN( LOCS, CONTXT, QNAME, FIXED, VALUE, BIT,
     :                      COMMNT, DONE, STATUS )

            IF ( STATUS .EQ. SAI__OK ) LQNAME( BIT ) = QNAME
         END DO
      END IF

*  Release the quality name information.
      IF( QI ) CALL IRQ_RLSE( LOCS, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Write the output paramter.
      CALL PAR_PUT1C( 'QNAMES', IRQ__QBITS, LQNAME, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SHOWQUAL_ERR1', 'SHOWQUAL: Unable to '//
     :                 'display quality names.', STATUS )
      END IF

      END
