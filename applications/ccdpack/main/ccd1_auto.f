      SUBROUTINE CCD1_AUTO( STYPE, FD, GIDIN, FTYPES, NNDF, VALID,
     :                      MKBIAS, BIAS, HVBIAS, DEBICR, DODEBI,
     :                      DEBTYP, INTERP, MKDARK, DARK, HVDARK,
     :                      DARKCR, DODARK, DRKTIM, MKFLAS, FLASH,
     :                      HVFLAS, DOFLAS, FLASCR, FLSTIM, MKFLAT,
     :                      FLAT, HVFLAT, DOFLAT, FILNMS, NFILS, DEBEXT,
     :                      DRKEXT, FLSEXT, FLTEXT, SAVER, IRFLAT,
     :                      PTEMP1, PTEMP2, STATUS )
*+
*  Name:
*     CCD1_AUTO

*  Purpose:
*     Writes a procedure to perform an automated CCDPACK reduction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_AUTO( STYPE, FD, GIDIN, FTYPES, NNDF, VALID, MKBIAS,
*                     BIAS, HVBIAS, DEBICR, DODEBI, DEBTYP, INTERP,
*                     MKDARK, DARK, HVDARK, DARKCR, DODARK, DRKTIM,
*                     MKFLAS, FLASH, HVFLAS, DOFLAS, FLASCR, FLSTIM,
*                     MKFLAT, FLAT, HVFLAT, DOFLAT, FILNMS, NFILS,
*                     DEBEXT, DRKEXT, FLSEXT, FLTEXT, SAVER, PTEMP1,
*                     PTEMP2, STATUS )

*  Description:
*     This routine writes a CCDPACK automated reduction sequence.  The
*     information passed to this routine describes the frames types and
*     filters, which are to be considered, whether master_flats or
*     biases are already present etc. (this is produced by the
*     CCD1_SCHED routine). All this information is used to write a
*     reduction sequence. Ancilliary information concerning the nature
*     of the CCD device etc. should either be in the NDF extensions or
*     available via global parameters.

*  Arguments:
*     STYPE = CHARACTER * ( * ) (Given)
*        The type of script file to be generated. Must be one of
*        "csh" or "icl".
*     FD = INTEGER (Given)
*        FIO file descriptor for output script.
*     GIDIN = INTEGER (Given)
*        GRP input group identifier.
*     NNDF = INTEGER (Given)
*        Number of input NDFs.
*     VALID( NNDF ) = LOGICAL (Given)
*        Array of flags indicating which NDFs are to be used.
*     MKBIAS = LOGICAL (Given)
*        Whether a MASTER_BIAS frame is to be produced.
*     BIAS = CHARACTER * ( * ) (Given)
*        The name of the master bias NDF, if created.
*     HVBIAS = LOGICAL (Given)
*        If a MASTER_BIAS exists already or not.
*     DEBICR( NNDF )  = LOGICAL (Given)
*        Flags indicating if an NDF has already been debiassed.
*     DODEBI = LOGICAL (Given)
*        Do debiassing.
*     DEBTYP = INTEGER (Given)
*        The debiassing type one of
*          1 = producing master and offsetting to strips
*          2 = producing master and not offsetting to strips
*              (either none exist or not using)
*          3 = using interpolation (no biases but need strips)
*          4 = direct subtraction of constant
*     INTERP = INTEGER (Given)
*        If DEBTYP=3 then the value of this argument specifies the
*        type of interpolation which is used.
*           1 = constant for each line (SMODE=CONSTANT, FMODE=LINE)
*           2 = single value for whole NDF (SMODE=CONSTANT, FMODE=PLANE)
*           3 = linear fits for each line (SMODE=LINEAR, FMODE=LINE)
*           4 = planar fit (SMODE=LINEAR, FMODE=PLANE)
*         The value for this argument should be consistent with the
*         extension information of the NDFs, i.e. only one bias strip
*         means can only do 1 or 2.
*     MKDARK = LOGICAL (Given)
*        Whether a MASTER_DARK frames is to be produced.
*     DARK = CHARACTER * ( * ) (Given)
*        The name of the master dark NDF, if created.
*     HVDARK = LOGICAL (Given)
*        If a MASTER_DARK exists already or not.
*     DARKCR( NNDF )  = LOGICAL (Given)
*        Flags indicating if an NDF has already been dark count
*        corrected.
*     DODARK = LOGICAL (Given)
*        Do dark correction.
*     DRKTIM( NNDF ) = LOGICAL (Given)
*        The dark exposure time of the NDF.
*     MKFLAS = LOGICAL (Given)
*        Whether a MASTER_FLASH frame is to be produced.
*     FLASH = CHARACTER * ( * ) (Given)
*        The name of the master pre-flash NDF, if created.
*     HVFLAS = LOGICAL (Given)
*        If a MASTER_FLASH exists already or not.
*     FLASCR( NNDF )  = LOGICAL (Given)
*        Flags indicating if an NDF has already been pre-flash
*        corrected.
*     DOFLAS = LOGICAL (Given)
*        Do pre-flash correction.
*     FLSTIM( NNDF ) = LOGICAL (Given)
*        The pre-flash exposure time of the NDF.
*     MKFLAT( NNDF ) = LOGICAL (Given)
*        Whether a MASTER_FLAT for the corresponding FILTER type
*        (recorded in FILNMS) is required.
*     FLAT = CHARACTER * ( * ) (Given)
*        The prefix of the names of any master flat NDFs, if created.
*        The true names have the filter type appended to this.
*     HVFLAT( NNDF ) = LOGICAL (Given)
*        Whether FLATs corresponding to the FILTER type (recorded in
*        FILNMS) are available.
*     DOFLAT( NNDF ) = LOGICAL (Given)
*        Do flatfielding for this filter.
*     FILNMS( NNDF ) = CHARACTER * ( * )
*        The FILTER types of the flatfields.
*     NFILS = INTEGER (Given)
*        Number of entries in FILNMS (i.e. number of possible
*        flatfields).
*     DEBEXT = CHARACTER * ( * ) (Given)
*        The extension used to modify the names output from DEBIAS.
*     DRKEXT = CHARACTER * ( * ) (Given)
*        The extension used to modify the names output from CALCOR
*        when performing dark correction.
*     FLSEXT = CHARACTER * ( * ) (Given)
*        The extension used to modify the names output from CALCOR
*        when performing dark correction.
*     FLTEXT = CHARACTER * ( * ) (Given)
*        The extension used to modify the names output from FLATCOR.
*     SAVER = CHARACTER * ( * ) (Given)
*        How much disk space to save when performing reduce.
*        Should be one of "NONE", "SOME" or "LOTS". NONE saves no disk
*        space and all intermediary files are not deleted. SOME deletes
*        all files except pre-debiassing NDFs and MASTERs. LOTS delete
*        the pre-debiassinf NDFs as well.
*     IRFLAT = LOGICAL (Given)
*        Whether or not TARGET frames may be used to flatfield.  This
*        is an IR option and will only be used if no flatfields of an
*        appropriate colour exist.
*     PTEMP1( NNDF ) = INTEGER (Given and Returned)
*        Workspace.
*     PTEMP2( NNDF ) = LOGICAL (Given and Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1994 Science & Engineering Research Council.
*     Copyright (C) 1995-1997, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1992 (PDRAPER):
*        Original version.
*     16-SEP-1993 (PDRAPER):
*        Now writes a reduction script.
*     27-JAN-1994 (PDRAPER):
*        Working version from above.
*     27-JAN-1994 (PDRAPER):
*        Added DEBTYP and INTERP arguments.
*     1-FEB-1994 (PDRAPER):
*        Added lots of HVxxx arguments to control use of masters.
*     4-FEB-1994 (PDRAPER):
*        Added DOxxx arguments.
*     4-FEB-1994 (PDRAPER):
*        Added character extension to names output from applications.
*     4-FEB-1994 (PDRAPER):
*        Added SAVER options.
*     10-FEB-1994 (PDRAPER):
*        Now uses the names of MASTERS from parameters.
*     14-FEB-1994 (PDRAPER):
*        Added checks for dark and pre-flash times.
*     23-AUG-1994 (PDRAPER):
*        Now checks if NDFs have already been processed.
*     10-NOV-1995 (PDRAPER):
*        Added IRFLAT for IR data reductions. Now keeps input to
*        MAKEFLAT if IRFLAT is TRUE (regardless, this could be more
*        elegant).
*     4-OCT-1996 (PDRAPER):
*        Correctly typed PTEMP2 as LOGICAL.
*     16-APR-1997 (PDRAPER):
*        Modified to deal with foreign data formats correctly. These
*        need special care when modifying the names of any products.
*     19-JUN-1997 (PDRAPER):
*        PROTEC now correctly defined as an array of size 2
*        (stopped working on Linux).
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'MSG_PAR'          ! Message system constants
      INCLUDE 'GRP_PAR'          ! GRP system constants

*  Arguments Given:
      CHARACTER * ( * ) STYPE
      INTEGER FD
      INTEGER GIDIN
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      LOGICAL VALID( NNDF )
      LOGICAL MKBIAS
      CHARACTER * ( * ) BIAS
      LOGICAL HVBIAS
      LOGICAL DEBICR( NNDF )
      LOGICAL DODEBI
      INTEGER DEBTYP
      INTEGER INTERP
      LOGICAL MKDARK
      CHARACTER * ( * ) DARK
      LOGICAL HVDARK
      LOGICAL DARKCR( NNDF )
      LOGICAL DODARK
      DOUBLE PRECISION DRKTIM( NNDF )
      LOGICAL MKFLAS
      CHARACTER * ( * ) FLASH
      LOGICAL HVFLAS
      LOGICAL FLASCR( NNDF )
      LOGICAL DOFLAS
      DOUBLE PRECISION FLSTIM( NNDF )
      LOGICAL MKFLAT( NNDF )
      CHARACTER * ( * ) FLAT
      LOGICAL HVFLAT( NNDF )
      LOGICAL DOFLAT ( NNDF )
      INTEGER NFILS
      CHARACTER * ( * ) FILNMS( NNDF )
      CHARACTER * ( * ) DEBEXT
      CHARACTER * ( * ) DRKEXT
      CHARACTER * ( * ) FLSEXT
      CHARACTER * ( * ) FLTEXT
      CHARACTER * ( * ) SAVER
      LOGICAL IRFLAT

*  Arguments Given and Returned
      INTEGER PTEMP1( NNDF )
      LOGICAL PTEMP2( NNDF )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( 1 ) PREFIX  ! Command-line prefix
      CHARACTER * ( 1 ) QUOTE   ! String quotation character
      CHARACTER * ( 2 ) COMMEN  ! Comment line character
      CHARACTER * ( 2 ) PROTEC( 2 )  ! Protection quote strings
      CHARACTER * ( 20 ) SAY    ! Write to user command
      CHARACTER * ( 21 ) DEL    ! File deletion command
      CHARACTER * ( 3 ) CONTIN  ! Continuation string
      CHARACTER * ( GRP__SZNAM ) TEMP ! Name of temporary file.
      CHARACTER * ( GRP__SZNAM ) MBIAS ! Name of existing master bias
      CHARACTER * ( GRP__SZNAM ) MDARK ! Name of existing master bias
      CHARACTER * ( GRP__SZNAM ) MFLASH ! Name of existing master bias
      CHARACTER * ( GRP__SZNAM ) MFLAT ! Name of existing master bias
      CHARACTER * ( MSG__SZMSG ) MESS ! Message buffer
      INTEGER I                 ! Loop variable
      INTEGER LCONT             ! Used length of continuation string
      INTEGER IAT               ! Position in string
      INTEGER J                 ! Loop variable
      INTEGER NFRMS             ! Number of frames located
      INTEGER NMAST             ! Number of master_frames
      LOGICAL USEPRO            ! Strict protection of values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report intentions to user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Writing reduction script.', STATUS )
      CALL CCD1_MSG( ' ', '    -------------------------', STATUS )

*  Define characterisation of the script we're going to use.
      CALL CCD1_SCRCH( STYPE, COMMEN, PREFIX, USEPRO, PROTEC, CONTIN,
     :                 QUOTE, SAY, DEL, STATUS )
      LCONT = CHR_LEN(CONTIN)

*=======================================================================
*  Produce a MASTER_BIAS if required.
*=======================================================================
      IF ( HVBIAS .AND. DODEBI ) THEN

*  Should have a master bias already. Get a pointer to it.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_BIAS',
     :                    PTEMP1, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Ok found it (again) record its name.
            CALL GRP_GET( GIDIN, PTEMP1( 1 ), 1, MBIAS, STATUS )

*  Flag all the frame with a type of master bias as no longer valid.
            DO 1 I = 1, NMAST
               VALID( PTEMP1( I ) ) = .FALSE.
 1          CONTINUE

*  Report the selection of the first master. Actually we will not use
*  this if the debiassing type does need it, but we do need to get
*  rid of any MASTER_BIASes in the list so have proceeded this far.
            IF ( DEBTYP .EQ. 1 .OR. DEBTYP .EQ. 2 ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL MSG_SETC( 'MBIAS', MBIAS )
               CALL CCD1_MSG( ' ',
     :         '  Using NDF: ^MBIAS as master bias.', STATUS )
            END IF
         ELSE IF ( DEBTYP .EQ. 1 .OR. DEBTYP .EQ. 2 ) THEN

*  There is no master bias frame, should be one as we have been told
*  there is one and we need it to debias.
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ',
     : '  CCD1_AUTO: Cannot re-locate master bias NDF', STATUS )
             GO TO 99
         END IF
      ELSE IF ( MKBIAS .AND. ( DEBTYP .EQ. 1 .OR. DEBTYP .EQ. 2 ) ) THEN
         MBIAS = BIAS
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'MBIAS', MBIAS )
         CALL CCD1_MSG( ' ',
     :'  Producing master bias: ^MBIAS, using NDFs:', STATUS )

*  Generate the command to make a master bias frame.
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WMMC( .TRUE., 'makebias', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, 'BIAS', ' ', FD, FTYPES, GIDIN,
     :                   NNDF, MBIAS, IRFLAT, VALID, PTEMP1, TEMP,
     :                   STATUS )

*  Make sure extension information is used and terminate it.
         IF ( SAVER .EQ. 'LOTS' ) THEN
            MESS = '  KEEPIN=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
         END IF
         IF ( DEBTYP .EQ. 2 ) THEN
            MESS = '  ZERO=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :12+LCONT ), STATUS )
         ELSE
            MESS = '  ZERO=TRUE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :11+LCONT ), STATUS )
         END IF
         MESS = '  USEEXT'//CONTIN
         CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
         CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
         MESS = PREFIX
         IAT = 1
         CALL CHR_APPND( DEL, MESS, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( TEMP, MESS, IAT )
         CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
      ELSE

*  Make sure there are no NDFs of type BIAS left in the lists.
         DO 21 I = 1, NNDF
            IF ( FTYPES( 1, I ) .EQ. 'BIAS' ) THEN
               VALID( I ) = .FALSE.
            END IF
 21      CONTINUE
      END IF

*=======================================================================
*  Sort out debiassing
*=======================================================================
      IF ( DODEBI ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Debiassing NDFs:', STATUS )

*  Get the frames that we require. This includes all TARGET, FLAT,
*  DARK and FLASH frames. Write the command to perform this task.
         DO 33 I = 1, NNDF
            PTEMP2( I ) = VALID( I )
            IF ( PTEMP2( I ) ) THEN
               IF ( DEBICR( I ) ) THEN
                  PTEMP2( I ) = .FALSE.
               END IF
            END IF
 33      CONTINUE
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WDPC( .TRUE., 'debias', ' ', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, DEBEXT, FD, FTYPES, GIDIN, NNDF,
     :                   PTEMP2, PTEMP1, NFRMS, TEMP, STATUS )

*  Which sort of debiassing are we going to use?
         MESS = ' '
         IF ( DEBTYP .EQ. 1 ) THEN

*  Standard method. Use master bias and offset to strips.
            MESS = '  GETBIAS=TRUE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
            MESS = '  OFFSET=TRUE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :13+LCONT ), STATUS )
         ELSE IF ( DEBTYP .EQ. 2 ) THEN

*  Not offsetting,
            MESS = '  GETBIAS=TRUE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
            MESS = '  OFFSET=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
         ELSE IF ( DEBTYP .EQ. 3 ) THEN

*  Using interpolation, no master bias. Set the SMODE and FMODE
*  parameters.
            MESS = '  GETBIAS=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :15+LCONT ), STATUS )
            IF ( INTERP .EQ. 1 ) THEN
               MESS =  '  SMODE=CONSTANT'//CONTIN
               CALL FIO_WRITE( FD, MESS( :16+LCONT ), STATUS )
               MESS = '  FMODE=LINE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :12+LCONT ), STATUS )
            ELSE IF ( INTERP .EQ. 2 ) THEN
               MESS =  '  SMODE=CONSTANT'//CONTIN
               CALL FIO_WRITE( FD, MESS( :16+LCONT ), STATUS )
               MESS = '  FMODE=PLANE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :13+LCONT ), STATUS )
            ELSE IF ( INTERP .EQ. 3 ) THEN
               MESS =  '  SMODE=LINEAR'//CONTIN
               CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
               MESS = '  FMODE=LINE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :12+LCONT ), STATUS )
            ELSE
               MESS =  '  SMODE=LINEAR'//CONTIN
               CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
               MESS = '  FMODE=PLANE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :13+LCONT ), STATUS )
            END IF
         ELSE

*  Must be using a constant.
            MESS =  '  USECON=TRUE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :13+LCONT ), STATUS )
            MESS = '  GETBIAS=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :15+LCONT ), STATUS )
         END IF

*  Add the name of the master bias if we have one.
         IF ( MBIAS .NE. ' ' .AND. DEBTYP .EQ. 1 .OR. DEBTYP .EQ. 2 )
     :   THEN
            IAT = 3
            MESS = ' '
            CALL CCD1_ADKEY( 'BIAS', MBIAS, USEPRO, PROTEC, MESS, IAT,
     :                       STATUS )
             MESS = MESS(:IAT )//CONTIN
            CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )
         END IF

*  Make sure the extension information is used and terminate the
*  command.
         IF ( SAVER .EQ. 'LOTS' ) THEN
            MESS = '  KEEPIN=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
         END IF
         MESS = '  USEEXT'//CONTIN
         CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
         CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
         MESS = PREFIX
         IAT = 1
         CALL CHR_APPND( DEL, MESS, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( TEMP, MESS, IAT )
         CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
      END IF

*=======================================================================
*  Generate master DARK count frame.
*=======================================================================
      IF ( HVDARK .AND. DODARK ) THEN

*  Should have a master dark already. Get a pointer to it.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_DARK',
     :                    PTEMP1, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Ok found it (again) record its name.
            CALL GRP_GET( GIDIN, PTEMP1( 1 ), 1, MDARK, STATUS )

*  Flag all the frame with a type of master dark as no longer valid.
            DO 2 I = 1, NMAST
               VALID( PTEMP1( I ) ) = .FALSE.
 2          CONTINUE

*  Report the selection of the first master.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'MDARK', MDARK )
            CALL CCD1_MSG( ' ', '  Using NDF: ^MDARK as master dark.',
     :                     STATUS )
         ELSE

*  There is no master dark frame, should be one as we have been told.
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ',
     : '  CCD1_AUTO: Cannot re-locate master dark NDF', STATUS )
             GO TO 99
         END IF
      ELSE IF ( MKDARK ) THEN
         MDARK = DARK
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'MDARK', MDARK )
         CALL CCD1_MSG( ' ',
     :'  Producing master dark: ^MDARK, using NDFs:', STATUS )

*  Generate the command to make a master dark frame.
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WMMC( .TRUE., 'makecal', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, 'DARK', ' ', FD, FTYPES, GIDIN,
     :                   NNDF, MDARK, IRFLAT, VALID, PTEMP1, TEMP,
     :                   STATUS )

*  Make sure extension information is used and terminate it.
         IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME' ) THEN
            MESS = '  KEEPIN=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
         END IF
         MESS = '  TYPE=DARK'//CONTIN
         CALL FIO_WRITE( FD, MESS( :11+LCONT ), STATUS )
         MESS = '  USEEXT'//CONTIN
         CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
         CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
         MESS = PREFIX
         IAT = 1
         CALL CHR_APPND( DEL, MESS, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( TEMP, MESS, IAT )
         CALL FIO_WRITE( FD, MESS( :IAT ), STATUS )
      END IF

*=======================================================================
*  Do the dark counts corrections.
*=======================================================================
      IF ( DODARK ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Subtracting dark counts from NDFs:',
     :                  STATUS )

*  Write the command to subtract the dark counts. Will do dark count
*  subtraction from all valid frames. These are frames which do have a
*  dark time greater than zero.
         DO 34 I = 1, NNDF
            PTEMP2( I ) = .FALSE.
            IF ( VALID( I ) .AND. .NOT. DARKCR( I ) ) THEN
               IF ( DRKTIM( I ) .GT. 0.0D0 ) THEN
                  PTEMP2( I ) = .TRUE.
               END IF
            END IF
 34      CONTINUE
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WDPC( .TRUE., 'calcor', ' ', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, DRKEXT, FD, FTYPES, GIDIN, NNDF,
     :                   PTEMP2, PTEMP1, NFRMS, TEMP, STATUS )
         IF ( NFRMS .GT. 0 ) THEN

*  Add the name of the master dark frame.
            IAT = 3
            MESS = ' '
            CALL CCD1_ADKEY( 'CAL', MDARK, USEPRO, PROTEC, MESS, IAT,
     :                       STATUS )
            MESS = MESS( :IAT )//CONTIN
            CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )

*  Terminate the command.
            IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME' ) THEN
               MESS = '  KEEPIN=FALSE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
            END IF
            MESS = '  TYPE=DARK'//CONTIN
            CALL FIO_WRITE( FD, MESS( :11+LCONT ), STATUS )
            MESS = '  USEEXT'//CONTIN
            CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
            CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
            MESS = PREFIX
            IAT = 1
            CALL CHR_APPND( DEL, MESS, IAT )
            IAT = IAT + 2
            CALL CHR_APPND( TEMP, MESS, IAT )
            CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
         END IF
      END IF

*=======================================================================
*  Generate master pre-flash frame.
*=======================================================================
      IF ( HVFLAS .AND. DOFLAS ) THEN

*  Should have a master flash already. Get a pointer to it.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'MASTER_FLASH',
     :                    PTEMP1, NMAST, STATUS )
         IF ( NMAST .GT. 0 ) THEN

*  Ok found it (again) record its name.
            CALL GRP_GET( GIDIN, PTEMP1( 1 ), 1, MFLASH, STATUS )

*  Flag all the frame with a type of master flash as no longer valid.
            DO 3 I = 1, NMAST
               VALID( PTEMP1( I ) ) = .FALSE.
 3          CONTINUE

*  Report the selection of the first master.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'MFLASH', MFLASH )
            CALL CCD1_MSG( ' ', '  Using NDF: ^MFLASH as master flash.',
     :                     STATUS )
         ELSE

*  There is no master flash frame, should be one as we have been told.
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ',
     : '  CCD1_AUTO: Cannot re-locate master flash NDF', STATUS )
             GO TO 99
         END IF
      ELSE IF ( MKFLAS ) THEN
         MFLASH = FLASH
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'MFLASH', MFLASH )
         CALL CCD1_MSG( ' ',
     :'  Producing master flash: ^MFLASH, using NDFs:', STATUS )

*  Generate the command to make a master flash frame.
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WMMC( .TRUE., 'makecal', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, 'FLASH', ' ', FD, FTYPES, GIDIN,
     :                   NNDF, MFLASH, IRFLAT, VALID, PTEMP1, TEMP,
     :                   STATUS )

*  Make sure extension information is used and terminate it.
         IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME' ) THEN
            MESS = '  KEEPIN=FALSE'//CONTIN
            CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
         END IF
         MESS = '  TYPE=FLASH'//CONTIN
         CALL FIO_WRITE( FD, MESS( :12+LCONT ), STATUS )
         MESS = '  USEEXT'//CONTIN
         CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
         CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
         MESS = PREFIX
         IAT = 1
         CALL CHR_APPND( DEL, MESS, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( TEMP, MESS, IAT )
         CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
      END IF

*=======================================================================
*  Do the pre-flash corrections.
*=======================================================================
*  These are applied to all valid frames.
      IF ( DOFLAS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Subtracting pre-flash from NDFs:',
     :                  STATUS )

*  Generate the command to subtract the flash counts. Do not correct
*  frames with no pre-flash exposure time.
         DO 35 I = 1, NNDF
            PTEMP2( I ) = .FALSE.
            IF ( VALID( I ) ) THEN
               IF ( FLSTIM( I ) .GT. 0.0D0 .AND. .NOT. FLASCR( I ) )
     :         THEN
                  PTEMP2( I ) = .TRUE.
               END IF
            END IF
 35      CONTINUE
         CALL FIO_WRITE( FD, COMMEN, STATUS )
         CALL CCD1_WDPC( .TRUE., 'calcor', ' ', PREFIX, USEPRO, PROTEC,
     :                   CONTIN, FLSEXT, FD, FTYPES, GIDIN, NNDF,
     :                   PTEMP2, PTEMP1, NFRMS, TEMP, STATUS )
         IF ( NFRMS .GT. 0 ) THEN

*  Add the name of the master flash if we have one.
            IAT = 3
            MESS = ' '
            CALL CCD1_ADKEY( 'CAL', MFLASH, USEPRO, PROTEC, MESS, IAT,
     :                       STATUS )
            MESS = MESS( :IAT )//CONTIN
            CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )

*  Terminate the command.
            IAT = 1
            IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME' ) THEN
               MESS = '  KEEPIN=FALSE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
            END IF
            MESS = '  TYPE=FLASH'//CONTIN
            CALL FIO_WRITE( FD, MESS( :12+LCONT ), STATUS )
            MESS = '  USEEXT'//CONTIN
            CALL FIO_WRITE( FD, MESS( :8+LCONT ), STATUS )
            CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
            MESS = PREFIX
            IAT = 1
            CALL CHR_APPND( DEL, MESS, IAT )
            IAT = IAT + 2
            CALL CHR_APPND( TEMP, MESS, IAT )
            CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
         END IF
      END IF

*=======================================================================
*  Generate the appropriate flatfields.
*=======================================================================
*  What's going to happen on the flatfield front? Will need a different
*  flatfield for each filter type.
      DO 8 J = 1, NFILS

*  If we have master flats for this filter already just use those.
         MFLAT = ' '
         IF ( HVFLAT( J ) .AND. DOFLAT( J ) ) THEN

*  Have been told a master flat for this filter already exists. So
*  locate it.
            CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID,
     :                       'MASTER_FLAT', FILNMS( J ), PTEMP1,
     :                       NMAST, STATUS )
            IF ( NMAST .GT. 0 ) THEN

*  Flag any master flats with this filter as no longer valid.
               DO 4 I = 1, NMAST
                  VALID( PTEMP1( I ) ) = .FALSE.
 4             CONTINUE

*  If more than one master flat exists then report the selection of the
*  first.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL GRP_GET( GIDIN, PTEMP1( 1 ), 1, MFLAT, STATUS )
               CALL MSG_SETC( 'MFLAT', MFLAT )
               CALL MSG_SETC( 'FILTER', FILNMS( J ) )
               CALL CCD1_MSG( ' ',
     :'  Using master flat: ^MFLAT, for filter ^FILTER', STATUS )
            ELSE

*  No master flat for this filter, should be, issue an error and abort.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FILTER', FILNMS( J ) )
               CALL ERR_REP( ' ', '  CCD1_AUTO: cannot re-locate'//
     :         ' a master flatfield for filter ^FILTER', STATUS )
               GO TO 99
            END IF
         ELSE IF ( MKFLAT( J ) ) THEN

*  Will be making a flatfield for this filter.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            MFLAT = FLAT
            IAT = CHR_LEN( MFLAT )
            CALL CHR_APPND( FILNMS( J ), MFLAT, IAT )
            CALL MSG_SETC( 'FILTER', FILNMS( J ) )
            CALL MSG_SETC( 'MFLAT', MFLAT )
            CALL CCD1_MSG( ' ',
     :'  Producing master flat: ^MFLAT for filter ^FILTER using NDFs:',
     :      STATUS )

*  Generate the command to make a master flatfield frame.
            CALL FIO_WRITE( FD, COMMEN, STATUS )
            CALL CCD1_WMMC( .TRUE., 'makeflat', PREFIX, USEPRO, PROTEC,
     :                      CONTIN, 'FLAT', FILNMS( J ), FD, FTYPES,
     :                      GIDIN, NNDF, MFLAT, IRFLAT, VALID, PTEMP1,
     :                      TEMP, STATUS )

*  Terminate it. Only delete input files if IRFLAT is FALSE, otherwise
*  assume we've used TARGETS and want to hang on to these for a while
*  longer.
            IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME'
     :           .AND. .NOT. IRFLAT ) THEN
               MESS = '  KEEPIN=FALSE'//CONTIN
               CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
            END IF
            CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
            MESS = PREFIX
            IAT = 1
            CALL CHR_APPND( DEL, MESS, IAT )
            IAT = IAT + 2
            CALL CHR_APPND( TEMP, MESS, IAT )
            CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
         ELSE

*  No flatfielding possible for this FILTER.
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'FILTER', FILNMS( J ) )
            CALL CCD1_MSG( ' ', ' Warning - flatfielding cannot '//
     :'be performed for data with filter type ^FILTER', STATUS )
         END IF

*=======================================================================
*  Now Flat field data with this filter type.
*=======================================================================
*  Find all the frames with this filter type and frame type TARGET.
*  Then write the command to perform the input and output. The input
*  names will be a modification of the originals these should be stored
*  in the GIDIN group. If no TARGET frames exist this is not an error
*  just issue a warning.
         IF ( DOFLAT( J ) .AND. ( HVFLAT( J ) .OR. MKFLAT( J ) ) ) THEN
            CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, 'TARGET',
     :                       PTEMP1, NFRMS, STATUS )
            IF ( NFRMS .GT. 0 ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL MSG_SETC( 'MFLAT', MFLAT )
               CALL CCD1_MSG( ' ', '  Using ^MFLAT to flatfield NDFs:',
     :                        STATUS )
               CALL FIO_WRITE( FD, COMMEN, STATUS )
               CALL CCD1_WFFC( .TRUE., PREFIX, USEPRO, PROTEC, CONTIN,
     :                         FLTEXT, FILNMS( J ), FD, FTYPES, GIDIN,
     :                         NNDF, VALID, PTEMP1, NFRMS, TEMP,
     :                         STATUS )

*  Add the appropriate flatfield.
               IAT = 3
               MESS = ' '
               CALL CCD1_ADKEY( 'FLAT', MFLAT, USEPRO, PROTEC, MESS,
     :                          IAT, STATUS )
               MESS = MESS( :IAT )//CONTIN
               CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )

*  Terminate command.
               IF ( SAVER .EQ. 'LOTS' .OR. SAVER .EQ. 'SOME' ) THEN
                  MESS = '  KEEPIN=FALSE'//CONTIN
                  CALL FIO_WRITE( FD, MESS( :14+LCONT ), STATUS )
               END IF
               CALL FIO_WRITE( FD, '  ACCEPT', STATUS )

*  Arrange to delete the temporary file.
               MESS = PREFIX
               IAT = 1
               CALL CHR_APPND( DEL, MESS, IAT )
               IAT = IAT + 2
               CALL CHR_APPND( TEMP, MESS, IAT )
               CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
            ELSE
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL MSG_SETC( 'FILTER', FILNMS( J ) )
               CALL CCD1_MSG( ' ', ' Warning - unable to flatfield'//
     :         ' any TARGET frames with filter type ^FILTER', STATUS )
            END IF
         END IF

*  End of flatfielding loop.
 8    CONTINUE

*  Write the exit command.
      MESS = PREFIX // 'exit'
      CALL FIO_WRITE( FD, MESS(:5), STATUS )

*  Exit on error label.
 99   CONTINUE
      END
* $Id$
