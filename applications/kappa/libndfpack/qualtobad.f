      SUBROUTINE QUALTOBAD( STATUS )
*+
*  Name:
*     QUALTOBAD

*  Purpose:
*     Sets selected NDF pixels bad on the basis of Quality.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL QUALTOBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine produces a copy of an input NDF in which selected
*     pixels are set bad. The selection is based on the values in the
*     QUALITY  component of the input NDF; any pixel which holds a set
*     of qualities  satisfying the quality expression given for
*     parameter QEXP is set bad in the output NDF. Named qualities can
*     be associated with specified pixels using the SETQUAL task.

*  Usage:
*     qualtobad in out qexp

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF.
*     OUT = NDF (Write)
*        The output NDF.
*     QEXP = LITERAL (Read)
*        The quality expression.
*     TITLE = LITERAL (Read)
*        Title for the output NDF. A null (!) value will cause the input
*        title to be used.  [!]

*  Examples:
*     qualtobad m51* *_clean saturated.or.glitch
*        This example copies all NDFs starting with the string "m51" to
*        a set of corresponding output NDFs. The name of each output
*        NDF is formed by extending the name of the input NDF with the
*        string "_clean". Any pixels which hold either of the qualities
*        "saturated" or "glitch" are set to the bad value in the output
*        NDFs.

*  Related Applications:
*     KAPPA: REMQUAL, SETBB, SETQUAL, SHOWQUAL.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2002, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2008, 2012 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     17-JAN-2002 (DSB):
*        Brought into KAPPA.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 10 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error constants.
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Full data type name
      CHARACTER ITYPE * ( NDF__SZTYP ) ! HDS Data type name
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to quality information
      CHARACTER QEXP*(IRQ__SZQEX)! Quality expression
      CHARACTER UNDEF(IRQ__QNREF)*(IRQ__SZQNM)! List of undefined
                                 ! quality names referenced in the
                                 ! quality expression
      CHARACTER XNAME*(DAT__SZNAM)! Name of NDF extension containing
                                 ! quality name information
      INTEGER ERRPNT             ! Offset to error within quality exprs.
      INTEGER IDQ                ! Identifier for compiled quality exprs
      INTEGER IPNT               ! Pointer: mapped array in the o/p NDF
      INTEGER J                  ! Index to list of undef quality names
      INTEGER NDFIN              ! Identifier for the input NDF
      INTEGER NDFOUT             ! Identifier for the output NDF
      INTEGER NEL                ! No. of mapped elements
      INTEGER NUNDEF             ! Number of undefined quality names
                                 ! referenced in the quality expression
      LOGICAL ALLBAD             ! All output data pixels are bad?
      LOGICAL NOBAD              ! No output data pixels are bad?
      LOGICAL THERE              ! VARIANCE is in a defined state?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF.
      CALL LPG_ASSOC( 'IN', 'UPDATE', NDFIN, STATUS )

*  Get the output NDF, propagating all components from the input to the
*  output (the HISTORY, LABEL, TITLE and all extensions are propagated
*  by default).
      CALL LPG_PROP( NDFIN, 'UNITS,DATA,VARIANCE,QUALITY,AXIS,WCS',
     :               'OUT', NDFOUT, STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOCS is returned holding a set of
*  five HDS locators which identify the NDF and the various components
*  of the quality information. XNAME is returned holding the name of the
*  NDF extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  Get a syntactically correct quality expression from the environment.
      CALL IRQ_GETQX( 'QEXP', QEXP, STATUS )

*  Attempt to compile the quality expression. An IRQ identifier is
*  returned for the compiled expression if it compiles succesfully.
      CALL IRQ_COMP( LOCS, IRQ__QNREF, .FALSE., QEXP, UNDEF, NUNDEF,
     :               ERRPNT, IDQ, STATUS )

*  Get the data type in which to map the DATA array.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', 1, NDFIN, 'DATA', ITYPE, DTYPE,
     :                STATUS )

*  Map the DATA array in the output NDF.
      CALL NDF_MAP( NDFOUT, 'DATA', ITYPE, 'UPDATE', IPNT, NEL,
     :              STATUS )

*  Set bad all output DATA pixels which satisfy the given quality
*  expression.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL IRQ_SBADB( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL IRQ_SBADUB( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                    ALLBAD, NOBAD,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL IRQ_SBADW( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL IRQ_SBADUW( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                    ALLBAD, NOBAD,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL IRQ_SBADI( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL IRQ_SBADK( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL IRQ_SBADR( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL IRQ_SBADD( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                   ALLBAD, NOBAD,
     :                   STATUS )

      END IF

*  If the output contains no valid data, give a warning message.
      IF( ALLBAD ) THEN
         CALL NDF_MSG( 'NDF', NDFOUT )
         CALL MSG_OUT( 'QUALTOBAD_MSG1', 'WARNING: ^NDF contains '//
     :                 'no valid Data values', STATUS )
      END IF

*  Set the bad-pixel flag for the DATA array of the output NDF.
      CALL NDF_SBAD( .NOT.NOBAD, NDFOUT, 'DATA', STATUS )

*  Unmap the DATA component.
      CALL NDF_UNMAP( NDFOUT, 'DATA', STATUS )

*  See if the VARIANCE component of the output NDF is in a defined
*  state.
      CALL NDF_STATE( NDFOUT, 'VARIANCE', THERE, STATUS )

*  If it is, process the VARIANCE array in the same way that the DATA
*  array was processed.
      IF( THERE ) THEN
         CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                   '_REAL, _DOUBLE', 1, NDFIN, 'VARIANCE', ITYPE,
     :                   DTYPE, STATUS )
         CALL NDF_MAP( NDFOUT, 'VARIANCE', ITYPE, 'UPDATE', IPNT, NEL,
     :                 STATUS )

         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL IRQ_SBADB( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL IRQ_SBADUB( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                       ALLBAD,
     :                       NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL IRQ_SBADW( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL IRQ_SBADUW( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                       ALLBAD,
     :                       NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL IRQ_SBADI( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL IRQ_SBADK( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL IRQ_SBADR( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL IRQ_SBADD( IDQ, .TRUE., NEL, %VAL( CNF_PVAL( IPNT ) ),
     :                      ALLBAD,
     :                      NOBAD, STATUS )

         END IF

         IF( ALLBAD ) THEN
            CALL NDF_MSG( 'NDF', NDFOUT )
            CALL MSG_OUT( 'QUALTOBAD_MSG2', 'WARNING: ^NDF '//
     :                    'contains no valid Variances values', STATUS )
         END IF

         CALL NDF_SBAD( .NOT.NOBAD, NDFOUT, 'VARIANCE', STATUS )
         CALL NDF_UNMAP( NDFOUT, 'VARIANCE', STATUS )

      END IF

*  Release the quality name information in the input NDF.
      CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the identifier for the compiled quality expression.
      CALL IRQ_ANNUL( IDQ, STATUS )

*  Close down the IRQ identifier system.
      CALL IRQ_CLOSE( STATUS )

*  Get a title for the new NDF from the parameter system.
      CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Annul the error and give a more friendly report if some quality names
*  were not defined.
      IF( STATUS .EQ. IRQ__NOQNM ) THEN
         CALL ERR_ANNUL( STATUS )

         DO J = 1, NUNDEF
            CALL MSG_SETC( 'C', ' ' )
            CALL MSG_SETC( 'C', UNDEF( J ) )
         END DO
         CALL NDF_MSG( 'NDF', NDFIN )

         CALL MSG_OUT( 'QUALTOBAD_MSG3', 'The following quality '//
     :                 'names are undefined in ''^NDF'': ^C',
     :                 STATUS )

*  Annul the error and give a more friendly report if no quality names
*  information was found.
      ELSE IF( STATUS .EQ. IRQ__NOQNI ) THEN
         CALL ERR_ANNUL( STATUS )

         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUT( 'QUALTOBAD_MSG4', 'No quality name '//
     :                 'definitions found in ''^NDF''.', STATUS )

      END IF

*  If an error has occurred, delete the output NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( NDFOUT, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'QUALTOBAD_ERR1', 'QUALTOBAD: Unable to '//
     :                 'convert quality information into bad pixels.',
     :                 STATUS )
      END IF

      END
