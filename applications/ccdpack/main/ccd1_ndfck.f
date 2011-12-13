      SUBROUTINE CCD1_NDFCK( SIMPLE, MENTRY, MODIFY, NNDF, NTYPES,
     :                       GID, FTYPES, FILT, DARK, FLASH, ADDDRK,
     :                       ADDFLS, ONEFIL, ONEDRK, ONEFLS, STATUS )
*+
*  Name:
*     CCD1_NDFCK

*  Purpose:
*     Checks the extension of an NDF and enters suitable items.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFCK( SIMPLE, MENTRY, MODIFY, NNDF, GID, FTYPES, FILT,
*                      DARK, FLASH, ADDDRK, ADDFLS, ONEFIL, ONEDRK,
*                      ONEFLS, STATUS )

*  Description:
*     This routine checks all the CCDPACK extension items in the NDFs in
*     the input groups GID. The groups can be previously separated
*     according to frame type (see notes). The CCDPACK global parameters
*     are added to the frame extension if they have been set (using
*     CCDSETUP) and MODIFY is true, or if no equivalent item exists.
*     The progress of the checking process is reported to the user and
*     the user is prompted for any missing/modifyable NDF specific items
*     such as the FTYPE, FILTER and the exposure times (if ADDDRK and/or
*     ADDFLS are true).

*  Arguments:
*     SIMPLE = LOGICAL (Given)
*        If true then the input NDFs are specified within one NDG group.
*        Otherwise the NDFs are separated into data type related
*        groups.
*     MENTRY = LOGICAL (Given)
*        True if the input NDF types are given in the GRP group FTYPES
*        and any filter types or exposures in FILFAC. Also assumes
*        SIMPLE is TRUE.
*     MODIFY = LOGICAL (Given)
*        If true then values which already exist in the extension may be
*        overwritten. If false then they may not.
*     NNDF( NTYPES ) = INTEGER (Given)
*        The number of NDFs in each input group, this should be at least 9.
*     NTYPES = INTEGER (Given)
*        The number of input NDF types.
*     GID( NTYPES ) = INTEGER (Given)
*        NDG group identifiers for input NDF groups.
*     FTYPES = INTEGER (Given)
*        If MENTRY is TRUE then this GRP group contains the frame types
*        of the input NDFS -- also assumes SIMPLE is TRUE.
*     FILT = INTEGER (Given)
*        If MENTRY and SIMPLE are TRUE then this GRP group contains any
*        filter types for the NDFs. The special symbol ! indicates a
*        null entry and is not used.
*     DARK = INTEGER (Given)
*        If MENTRY and SIMPLE are TRUE then this GRP group contains any
*        dark times for the NDFs. The special symbol ! indicates a
*        null entry and is not used.
*     FLASH = INTEGER (Given)
*        If MENTRY and SIMPLE are TRUE then this GRP group contains any
*        pre-flash times for the NDFs. The special symbol ! indicates a
*        null entry and is not used.
*     ADDDRK = LOGICAL (Given)
*        If true then the user will be prompted for DARK times to enter
*        into into the .TIMES structure.
*     ADDFLS = LOGICAL (Given)
*        If true then the user will be prompted for FLASH times to enter
*        into into the .TIMES structure.
*     ONEFIL = LOGICAL (Given)
*        If TRUE then only prompt for a filter name once.
*     ONEDRK = LOGICAL (Given)
*        If TRUE then only prompt for a dark exposure once.
*     ONEFLS = LOGICAL (Given)
*        If TRUE then only prompt for a pre-flash exposure once.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The input groups should be organised so that the following is
*     true.
*          If SIMPLE then
*            NNDF( 1 ) should contain the total number of input NDFs.
*            GID( 1 ) should contain the GRP group identifier for the
*                     above
*             If MENTRY then
*                FTYPES is a group of the input NDF types. FILT, DARK and
*                FLASH are any filter types and exposure factors
*          Else if not SIMPLE
*            The groups should be organised with the data types in the
*            following order.
*               1 = Bias frames
*               2 = Target frames
*               3 = Dark frames
*               4 = Flash frames
*               5 = Flatfields.
*               6 = Master bias.
*               7 = Master flats.
*               8 = Master dark.
*               9 = Master flash.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 2000 Central Laboratory of the Research
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
*     11-FEB-1992 (PDRAPER):
*        Original version.
*     3-FEB-1994 (PDRAPER):
*        Added ORIGINAL file name extension item.
*     7-FEB-1994 (PDRAPER):
*        Added SATURATION value.
*     9-FEB-1994 (PDRAPER):
*        Changed to use ADDDRK and ADDFLS.
*     23-MAY-1994 (PDRAPER):
*        Now also handles pre-specification of the NDF frame types,
*        dark and pre-flash times.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     8-NOV-1995 (PDRAPER):
*        Added ONEFIL parameter.
*     11-NOV-1995 (PDRAPER):
*        Added MASTER frame foreign/native import/reimport.
*     15-NOV-1995 (PDRAPER):
*        Added ONEDRK and ONEFLS to support IR scripts.
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument from IRG_NDFEX call. Added call to
*        release dangling LOCTMP.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_ERR'         ! PAR error constants
      INCLUDE 'DAT_PAR'         ! DAT/HDS constants
      INCLUDE 'FIO_PAR'         ! FIO constants
      INCLUDE 'MSG_PAR'         ! Message system

*  Arguments Given:
      LOGICAL SIMPLE
      LOGICAL MENTRY
      LOGICAL MODIFY
      INTEGER NTYPES
      INTEGER NNDF( NTYPES )
      INTEGER GID( NTYPES )
      INTEGER FTYPES
      INTEGER FILT
      INTEGER DARK
      INTEGER FLASH
      LOGICAL ADDDRK
      LOGICAL ADDFLS
      LOGICAL ONEFIL
      LOGICAL ONEDRK
      LOGICAL ONEFLS

*  Status:
      INTEGER STATUS            ! Global status

*  External References
      LOGICAL CCD1_MATCH
      EXTERNAL CCD1_MATCH       ! Checks string against list of
                                ! possible strings
*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Constants:
      INTEGER IBIAS, ITARG, IDARK, IFLASH, IFLAT,
     :     IMBIAS, IMFLAT, IMDARK, IMFLAS ! Possible FRAME TYPES
      PARAMETER ( IBIAS = 1, ITARG = 2, IDARK = 3 , IFLASH = 4 ,
     :            IFLAT = 5, IMBIAS = 6, IMFLAT = 7, IMDARK = 8,
     :            IMFLAS =9 )

*  Local Variables:
      CHARACTER * ( 12 ) FLIST( 9 ) ! List of possible frame types
      CHARACTER * ( 12 ) FTYPE  ! Frame type
      CHARACTER * ( 2 ) BUFF    ! Local character value buffer
      CHARACTER * ( 80 ) BUFFER ! Local character value buffer
      CHARACTER * ( 9 * 12 ) CLIST ! List of possible frame types separated by commas.
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! HDS locator to extension
      CHARACTER * ( DAT__SZLOC ) LOCTMP ! HDS locator to extension sub-structure
      CHARACTER * ( FIO__SZFNM ) NDFNAM ! Name of the current NDF
      CHARACTER * ( MSG__SZMSG ) MESS ! Output message buffer
      DOUBLE PRECISION DVAL     ! DBLE value
      INTEGER BOUNDS( 4 )       ! Local integer buffer
      INTEGER DUMMY             ! Dummy variable
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER IBUFF             ! Integer buffer
      INTEGER ID                ! Dummy NDF identifier
      INTEGER INDEX             ! Count of NDFs processed
      INTEGER J                 ! Loop variable
      INTEGER LBND( 2 )         ! Lower bounds of NDF
      INTEGER LBND2( 2 )        ! Lower bounds of NDF
      INTEGER MAXNDF            ! Total number of NDFs to process
      INTEGER MSGLEV            ! Message system report level
      INTEGER NAMLEN            ! Length of NDF name string
      INTEGER NBOUND            ! Number of enties in BOUNDS
      INTEGER NDFID             ! NDF identifier
      INTEGER NDIM              ! Number of input NDF dimensions
      INTEGER ORIG              ! Dummy
      INTEGER UBND( 2 )         ! Upper bounds of NDF
      INTEGER UBND2( 2 )        ! Upper bounds of NDF
      LOGICAL FRMEXT            ! Value is from extension
      LOGICAL NEED              ! Flag
      LOGICAL OK                ! Flag
      LOGICAL THERE             ! Flag
      LOGICAL LVAL              ! A logical value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisations.
      FLIST( IBIAS ) = 'BIAS'
      FLIST( ITARG ) = 'TARGET'
      FLIST( IDARK ) = 'DARK'
      FLIST( IFLASH ) = 'FLASH'
      FLIST( IFLAT ) = 'FLAT'
      FLIST( IMBIAS ) = 'MASTER_BIAS'
      FLIST( IMFLAT ) = 'MASTER_FLAT'
      FLIST( IMDARK ) = 'MASTER_DARK'
      FLIST( IMFLAS ) = 'MASTER_FLASH'
      CLIST = 'BIAS,TARGET,DARK,FLASH,FLAT,MASTER_BIAS,'//
     :        'MASTER_FLAT,MASTER_DARK,MASTER_FLASH'

*  Quick count of how many NDFs we're going to process.
      MAXNDF = 0
      DO 3 I = 1, NTYPES
         IF ( NNDF( I ) .GT. 0 ) THEN
            MAXNDF = MAXNDF + NNDF( I )
         END IF
 3    CONTINUE

*  Loop over all possible NDF names.
      INDEX = 0
      DO 1 J = 1, NTYPES
         IF ( NNDF( J ) .GT. 0 ) THEN
            DO 2 I = 1, NNDF( J )

*  Get the NDF.
               CALL NDG_NDFAS( GID( J ), I, 'UPDATE', NDFID, STATUS )

*  Inform user that this NDF is being processed log this information to
*  the log file.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL NDF_MSG( 'NDF', NDFID )
               CALL CCD1_MSG( ' ', '  Processing NDF : ^NDF ', STATUS )

*  Inform user how many NDFs we've processed out of the total number.
               INDEX = INDEX + 1
               CALL MSG_SETI( 'CURRENT_NUM', INDEX )
               CALL MSG_SETI( 'MAX_NUM', MAXNDF )
               CALL CCD1_MSG( ' ',
     :              '  (Number ^CURRENT_NUM of ^MAX_NUM)', STATUS )

*  Write out the header for the information messages about the items
*  which are written/exist.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               MESS = '      Item                Value'
               CALL CCD1_MSG( ' ', MESS, STATUS )
               MESS = '      ----                -----'
               CALL CCD1_MSG( ' ', MESS, STATUS )

*  Look for the CCDPACK extension, if one is not found then create one.
               CALL NDF_XSTAT( NDFID, 'CCDPACK', THERE, STATUS )

*  If the extension does not exist then create it.
               IF ( .NOT. THERE ) THEN
                  CALL NDF_XNEW( NDFID, 'CCDPACK', 'CCDPACK_EXT', 0, 0,
     :                           LOCEXT, STATUS )
               ELSE

*  Just get a locator to it.
                  CALL NDF_XLOC( NDFID, 'CCDPACK', 'UPDATE', LOCEXT,
     :                           STATUS )
               END IF

*  Get the bounds for performing checks.
               CALL NDF_BOUND( NDFID, 2, LBND, UBND, NDIM, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 99

*=======================================================================
*  Look for the FTYPE.
*=======================================================================
               FRMEXT = .FALSE.
               CALL DAT_THERE( LOCEXT, 'FTYPE', THERE, STATUS )
               IF ( .NOT. THERE )  THEN

*  Has this NDF an assumed type? I.e. is simple false?
                  IF ( .NOT. SIMPLE ) THEN

*  The file type has been specified by the caller.
                     FTYPE = FLIST( J )
                  ELSE

*  No frame type so has a value been given in the FTYPES list?
                     OK = .TRUE.
                     IF ( MENTRY ) THEN
                        CALL GRP_GET( FTYPES, I, 1, FTYPE, STATUS )

*  Check the type against the list of those possible.
                        OK = CCD1_MATCH( FTYPE, FLIST, NTYPES, STATUS )
                        IF ( .NOT. OK ) THEN

*  Issue a warning.
                           CALL MSG_SETC( 'FTYPE', FTYPE )
                           CALL MSG_OUT( ' ',
     :' Warning - NDF has an unidentified frame type (^FTYPE)', STATUS )
                        END IF
                     END IF
                     IF ( .NOT. OK ) THEN

*  Need to prompt the user.
                        CALL PAR_CHOIC( 'FTYPE', ' ', CLIST, .FALSE.,
     :                                  FTYPE, STATUS )

*  Cancel this parameter associon - do not want to use it next time.
                        CALL PAR_CANCL( 'FTYPE', STATUS )
                     END IF
                  END IF
               ELSE

*  Have a type -- extract the value.
                  CALL CMP_GET0C( LOCEXT, 'FTYPE', FTYPE, STATUS )

*  Check the type against the list of those possible.
                  OK = CCD1_MATCH( FTYPE, FLIST, NTYPES, STATUS )
                  IF ( .NOT. OK ) THEN

*  Issue a warning.
                     CALL MSG_SETC( 'FTYPE', FTYPE )
                     CALL MSG_OUT( ' ',
     :' Warning - NDF has an unidentified frame type (^FTYPE)', STATUS )
                  END IF

*  If modify is set or the value is invalid then allow user to
*  redefine the value.
                  IF ( MODIFY .OR. .NOT. OK ) THEN
                     IF ( .NOT. SIMPLE ) THEN

*  The user has already specified a new value - use it.
                        FTYPE = FLIST( J )
                     ELSE

*  Is SIMPLE and a valid value doesn't exist or we have been asked to
*  modify existing values so has a value been given in the FTYPES list?
                        OK = .FALSE.
                        IF ( MENTRY ) THEN
                           CALL GRP_GET( FTYPES, I, 1, FTYPE, STATUS )

*  Check the type against the list of those possible.
                           OK = CCD1_MATCH( FTYPE, FLIST, NTYPES,
     :                                      STATUS )
                           IF ( .NOT. OK ) THEN

*  Issue a warning.
                              CALL MSG_SETC( 'FTYPE', FTYPE )
                              CALL MSG_OUT( ' ',
     :' Warning - NDF has an unidentified frame type (^FTYPE)',
     :                             STATUS )
                           END IF
                        END IF

*  If not OK then havn't gotten a value so far so prompt the user.
                        IF ( .NOT. OK ) THEN
                           CALL PAR_CHOIC( 'FTYPE', ' ', CLIST,
     :                                     .FALSE., FTYPE, STATUS )

*  Cancel this parameter association - do not want to use it next
*  time.
                           CALL PAR_CANCL( 'FTYPE', STATUS )
                        END IF
                     END IF
                  ELSE

*  Value is that in the extension.
                     FRMEXT = .TRUE.
                  END IF
               END IF

*  End of the line for FTYPE must have a value write it to the
*  extension.
               CALL CMP_MODC( LOCEXT, 'FTYPE', 12, 0, 0, STATUS )
               CALL CMP_PUT0C( LOCEXT, 'FTYPE', FTYPE, STATUS )

*  And write it out to the user.
               MESS = ' '
               MESS( 5 : )  = 'FTYPE'
               IAT = CHR_LEN( FTYPE )
               MESS( 28 : ) = FTYPE ( :IAT )
               IAT = IAT + 28
               IF ( FRMEXT ) MESS( IAT: IAT ) = '*'
               CALL CCD1_MSG( ' ', MESS, STATUS )

*=======================================================================
*  Check the FILTER type.
*=======================================================================
*  This value just needs to be present for flatfield, target and
*  master flatfield data any old string will do.
               IF ( FTYPE .EQ. FLIST( IFLAT ) .OR.
     :              FTYPE .EQ. FLIST( ITARG ) .OR.
     :              FTYPE .EQ. FLIST( IMFLAT ) ) THEN

*  Need a filter type.
                  FRMEXT = .FALSE.
                  CALL DAT_THERE( LOCEXT, 'FILTER', THERE, STATUS )
                  IF ( .NOT. THERE .OR. MODIFY ) THEN

*  Has one already been given?
                     IF ( SIMPLE .AND. MENTRY ) THEN
                        CALL GRP_GET( FILT, I, 1, BUFFER, STATUS )
                        IF ( BUFFER( 1: 1 ) .EQ. '!' ) BUFFER = 'NONE'
                     ELSE

*  Ask for one from the user.
                        CALL PAR_GET0C( 'FILTER', BUFFER, STATUS )

*  Cancel the parameter association if allowed.
                        IF ( .NOT. ONEFIL ) THEN
                           CALL PAR_CANCL( 'FILTER', STATUS )
                        END IF
                     END IF

*  Write the value out to the extension. Allow for a blank return.
                     IAT = CHR_LEN( BUFFER )
                     IAT = MAX( IAT, 1 )
                     CALL CMP_MODC( LOCEXT, 'FILTER', IAT, 0, 0,
     :                              STATUS )
                     CALL CMP_PUT0C( LOCEXT, 'FILTER', BUFFER( :IAT ),
     :                               STATUS )
                  ELSE IF ( THERE ) THEN

*  Get value from the extension.
                     FRMEXT = .TRUE.
                     CALL CMP_GET0C( LOCEXT, 'FILTER', BUFFER, STATUS )
                     IAT = CHR_LEN( BUFFER )
                  ELSE

*  Make a value up.
                     BUFFER = 'NONE'
                     IAT = 4
                  END IF

*  And write it out to the user.
                  MESS = ' '
                  MESS( 5 : )  = 'FILTER'
                  MESS( 28 : ) = BUFFER( : IAT )
                  IAT = IAT + 28
                  IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                  CALL CCD1_MSG( ' ', MESS, STATUS )
               END IF

*=======================================================================
*  Check that the necessary exposure times are present.
*=======================================================================
*  At present only the FLASH and DARK times are required for each input
*  NDF (and these only if DARK counts or pre-FLASH corrections are to
*  be made).

*  Get a locator to a .TIMES structure, creating it if it does not
*  exist already.
               FRMEXT = .FALSE.
               CALL CMP_MOD( LOCEXT, 'TIMES', 'CCDPACK_XITEM', 0, 0,
     :                       STATUS )

*  Get a locator to this.
               CALL DAT_FIND( LOCEXT, 'TIMES', LOCTMP, STATUS )

*  If we need to add times then look for a DARK times value. This is
*  required by all except biases,flash frames and any masters.
               IF ( FTYPE .EQ. FLIST( ITARG ) .OR.
     :              FTYPE .EQ. FLIST( IFLAT ) .OR.
     :              FTYPE .EQ. FLIST( IDARK ) ) THEN
                  IF ( ADDDRK ) THEN

*  Check that a value hasn't already been given.
                     IF ( SIMPLE .AND. MENTRY ) THEN

*  Extract value from GRP group and enter if allowed.
                        IF ( MODIFY ) THEN
                           FRMEXT = .FALSE.
                           CALL GRP_GET( DARK, I, 1, BUFFER, STATUS )
                           IF ( BUFFER( 1: 1 ) .NE. '!' ) THEN
                              CALL CHR_CTOD( BUFFER, DVAL, STATUS )
                              CALL CMP_MOD( LOCTMP, 'DARK', '_DOUBLE',
     :                                      0, 0, STATUS )
                              CALL CMP_PUT0D( LOCTMP, 'DARK', DVAL,
     :                                        STATUS )
                              OK = .TRUE.
                           ELSE
                              OK = .FALSE. ! No dark time added
                           END IF
                        ELSE
                           CALL DAT_THERE( LOCTMP, 'DARK', OK, STATUS )
                           IF ( OK ) THEN
                              CALL CMP_GET0D( LOCTMP, 'DARK', DVAL,
     :                                        STATUS )
                              FRMEXT = .TRUE.
                           END IF
                        END IF
                     ELSE

*  Get value from user if required, or just check etc.
                        LVAL = .NOT. ONEDRK
                        CALL CCD1_XADD( 'DARKTIME', LOCTMP, 'DARK',
     :                                  MODIFY, LVAL, DVAL, OK,
     :                                  FRMEXT, STATUS )
                     END IF
                  ELSE
                     CALL DAT_THERE( LOCTMP, 'DARK', OK, STATUS )
                     IF ( OK ) THEN
                        CALL CMP_GET0D( LOCTMP, 'DARK', DVAL, STATUS )
                        FRMEXT = .TRUE.
                     END IF
                  END IF
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'TIMES.DARK'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT), STATUS )
                  END IF
               END IF

*  Finally look for a FLASH exposure time (least likely correction).
*  Not performed for BIASes and masters.
               IF ( FTYPE .EQ. FLIST( ITARG ) .OR.
     :              FTYPE .EQ. FLIST( IFLASH ) .OR.
     :              FTYPE .EQ. FLIST( IFLAT ) .OR.
     :              FTYPE .EQ. FLIST( IDARK ) ) THEN
                  IF ( ADDFLS ) THEN

*  Check that a value hasn't already been given.
                     IF ( SIMPLE .AND. MENTRY ) THEN

*  Extract value from GRP group.
                        IF ( MODIFY ) THEN
                           FRMEXT = .FALSE.
                           CALL GRP_GET( FLASH, I, 1, BUFFER, STATUS )
                           IF ( BUFFER( 1: 1 ) .NE. '!' ) THEN
                              CALL CHR_CTOD( BUFFER, DVAL, STATUS )
                              CALL CMP_MOD( LOCTMP, 'FLASH', '_DOUBLE',
     :                                      0, 0, STATUS )
                              CALL CMP_PUT0D( LOCTMP, 'FLASH', DVAL,
     :                                        STATUS )
                              OK = .TRUE.
                            ELSE
                              OK = .FALSE. ! No flash-time added
                            END IF
                        ELSE
                           CALL DAT_THERE( LOCTMP, 'FLASH', OK, STATUS )
                           IF ( OK ) THEN
                              CALL CMP_GET0D( LOCTMP, 'FLASH', DVAL,
     :                                        STATUS )
                              FRMEXT = .TRUE.
                           END IF
                        END IF
                     ELSE

*  Get value from user if required, or just check etc.
                        LVAL = .NOT. ONEFLS
                        CALL CCD1_XADD( 'FLASHTIME', LOCTMP, 'FLASH',
     :                                  MODIFY, LVAL, DVAL, OK,
     :                                  FRMEXT, STATUS )
                     END IF
                  ELSE
                     CALL DAT_THERE( LOCTMP, 'FLASH', OK, STATUS )
                     IF ( OK ) THEN
                        CALL CMP_GET0D( LOCTMP, 'FLASH', DVAL, STATUS )
                        FRMEXT = .TRUE.
                     END IF
                  END IF
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'TIMES.FLASH'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF
               END IF

*  Release locator to .TIMES structure.
               CALL DAT_ANNUL( LOCTMP, STATUS )

*======================================================================
*  Branch for normal frames and masters at this point.
*======================================================================
               IF ( FTYPE .EQ. FLIST( IMBIAS ) .OR.
     :              FTYPE .EQ. FLIST( IMFLAT ) .OR.
     :              FTYPE .EQ. FLIST( IMDARK ) .OR.
     :              FTYPE .EQ. FLIST( IMFLAS ) ) THEN

*  Only requirement for masters is a check whether the master bias
*  is zeroed or not.
                  IF ( FTYPE .EQ. FLIST( IMBIAS ) ) THEN

                     IF ( SIMPLE .AND. MENTRY ) THEN

*  If value can be found as the darktime! This is just an aid for
*  writing scripts. Extract value from GRP group and enter if allowed.
                        IF ( MODIFY ) THEN
                           FRMEXT = .FALSE.
                           CALL GRP_GET( DARK, I, 1, BUFFER, STATUS )
                           IF ( BUFFER( 1: 1 ) .NE. '!' ) THEN

*  Have a value and can modify the extension, so do so.
                              CALL CHR_CTOL( BUFFER, LVAL, STATUS )
                              CALL CMP_MOD( LOCEXT, 'ZEROED',
     :                                      '_LOGICAL', 0, 0, STATUS )
                              CALL CMP_PUT0L( LOCEXT, 'ZEROED', LVAL,
     :                                        STATUS )
                              OK = .TRUE.
                           ELSE
                              OK = .FALSE.
                           END IF
                        ELSE

*  May not modify the value. Just get its value to report.
                           CALL DAT_THERE( LOCEXT, 'ZEROED', OK, STATUS)
                           IF ( OK ) THEN
                              CALL CMP_GET0L( LOCEXT, 'ZEROED', LVAL,
     :                                        STATUS )
                              FRMEXT = .TRUE.
                           END IF
                        END IF
                     ELSE

*  Get value from user if required, or just check etc.
                        CALL DAT_THERE( LOCEXT, 'ZEROED', OK, STATUS )
                        IF ( OK ) THEN
                           CALL CMP_GET0L( LOCEXT, 'ZEROED', LVAL,
     :                                     STATUS )
                           FRMEXT = .TRUE.
                        END IF
                        IF ( MODIFY ) THEN
                           CALL PAR_GET0L( 'ZEROED', LVAL, STATUS )
                           CALL CMP_MOD( LOCEXT, 'ZEROED',
     :                                   '_LOGICAL', 0, 0, STATUS )
                           CALL CMP_PUT0L( LOCEXT, 'ZEROED', LVAL,
     :                                     STATUS )
                           FRMEXT = .FALSE.
                        END IF
                     END IF
                     IF ( OK ) THEN
                        IF ( LVAL ) THEN
                           IF ( FRMEXT ) THEN
                              CALL CCD1_MSG( ' ',
     :                     '    ZEROED                 TRUE*', STATUS )
                           ELSE
                              CALL CCD1_MSG( ' ',
     :                     '    ZEROED                 TRUE', STATUS )
                           END IF
                        ELSE
                           IF ( FRMEXT ) THEN
                              CALL CCD1_MSG( ' ',
     :                     '    ZEROED                 FALSE*', STATUS)
                           ELSE
                              CALL CCD1_MSG( ' ',
     :                     '    ZEROED                 FALSE', STATUS )
                           END IF
                        END IF
                     END IF
                  END IF
               ELSE

*=======================================================================
*  Check the CCD parameters.
*=======================================================================
*  These values may or may not be needed unfortunately there is no way
*  to find out if this is so as later options affect this. Try to
*  access a value from the user (strictly using the GLOBAL parameter
*  only) if he has supplied one then look for presence of the actual
*  item in the extension. If it exists then if MODIFY is set, issue a
*  warning (if the user has supplied a value) and supercede the value.
*  If a value does not exist and the user has not supplied one then do
*  not set the item.  Finally if the user has supplied a value and a
*  extension item does not exist then write the new value. Check bounds
*  and extent against that actual values of the NDF - this would be
*  performed later anyway.
*=======================================================================
*
*=======================================================================
*  Try for the DIRECTION first.
*=======================================================================
*  Look for existing one.
                  BUFF = ' '
                  NEED = .TRUE.
                  FRMEXT = .FALSE.
                  CALL DAT_THERE( LOCEXT, 'DIRECTION', THERE, STATUS )
                  IF ( .NOT. THERE .OR. MODIFY ) THEN

*  Try for a value from the environment.
                     CALL CCD1_GTDIR( .FALSE., ID, IBUFF, OK, STATUS )
                     IF ( STATUS .EQ. PAR__NULL ) THEN

*  No value supplied cancel error and proceed.
                        CALL ERR_ANNUL( STATUS )
                     ELSE

*  Write the new value.
                        IF ( IBUFF .EQ. 1 ) THEN
                           BUFF = 'X'
                        ELSE
                           BUFF = 'Y'
                        END IF
                        CALL CMP_MODC( LOCEXT, 'DIRECTION', 1, 0, 0,
     :                                 STATUS )
                        CALL CMP_PUT0C( LOCEXT, 'DIRECTION', BUFF,
     :                                  STATUS )
                        NEED = .FALSE.
                     END IF
                  END IF

*  Get the current value if one exists and we have not been allowed
*  to modify it.
                  IF ( THERE .AND. NEED ) THEN
                     CALL CMP_GET0C( LOCEXT, 'DIRECTION', BUFF, STATUS )
                     NEED = .FALSE.
                     FRMEXT = .TRUE.
                  END IF
                  IF ( .NOT. NEED ) THEN
                     MESS = ' '
                     MESS( 5 : ) = 'DIRECTION'
                     MESS( 28 :28 ) = BUFF
                     IF ( FRMEXT ) MESS( 29 :29 ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :29 ), STATUS )
                  END IF

*=======================================================================
*  Now the Bias strip placements.
*=======================================================================
*  Note that checking of bounds depends on the readout direction being
*  known. Look for existing values.
                  NEED = .TRUE.
                  FRMEXT = .FALSE.
                  CALL DAT_THERE( LOCEXT, 'BOUNDS', THERE, STATUS )
                  IF ( MODIFY .OR. .NOT. THERE ) THEN

*  Try for values from the environment. Limits are applied if the
*  direction is known, otherwise the user must be trusted.
                     IF ( BUFF .EQ. 'X' ) THEN
                        DUMMY = UBND( 1 ) - LBND( 1 ) + 1
                        CALL CCD1_GTBDS( .FALSE., NDFID, DUMMY,
     :                                   LBND( 1 ), 4, BOUNDS, NBOUND,
     :                                   OK, STATUS )
                     ELSE IF ( BUFF .EQ. 'Y' ) THEN
                        DUMMY = UBND( 2 ) - LBND( 2 ) + 1
                        CALL CCD1_GTBDS( .FALSE., NDFID, DUMMY,
     :                                   LBND( 2 ), 4, BOUNDS, NBOUND,
     :                                   OK, STATUS )
                     ELSE
                        CALL CCD1_GTBDS( .FALSE., NDFID, -1, ORIG, 4,
     :                                   BOUNDS, NBOUND, OK, STATUS )
                     END IF
                     IF ( STATUS .EQ. PAR__NULL ) THEN

*  No value supplied cancel error and proceed.
                        CALL ERR_ANNUL( STATUS )
                     ELSE

*  Add the values to the extension
                        CALL CCG1_STO1I( NDFID, 'BOUNDS',
     :                                   'CCDPACK_XITEM',
     :                                   'START1,END1,START2,END2,',
     :                                   NBOUND, BOUNDS, STATUS )
                        NEED = .FALSE.
                     END IF
                  END IF

*  Get the current values if they exist and we have not been allowed
*  to modify them.
                  IF ( THERE .AND. NEED ) THEN
                     CALL DAT_FIND( LOCEXT, 'BOUNDS', LOCTMP, STATUS )
                     CALL DAT_NCOMP( LOCTMP, NBOUND, STATUS )
                     IF ( NBOUND .EQ. 2 ) THEN
                        CALL CCG1_FCH1I( NDFID, 'BOUNDS',
     :                                   'START1,END1,', 2, BOUNDS,
     :                                   OK, STATUS )
                     ELSE
                        CALL CCG1_FCH1I( NDFID, 'BOUNDS',
     :                                   'START1,END1,START2,END2,', 4,
     :                                   BOUNDS, OK, STATUS )
                     END IF
                     NEED = .FALSE.
                     FRMEXT = .TRUE.
                     CALL DAT_ANNUL( LOCTMP, STATUS )
                  END IF

*  If we have some values write them out to the user.
                  IF ( .NOT. NEED ) THEN
                     IF ( NBOUND .GE. 2 ) THEN

*  Lower bound of first strip.
                        MESS = ' '
                        MESS( 5 : )  = 'BOUNDS.START1'
                        CALL CHR_ITOC( BOUNDS( 1 ), MESS( 28: ), IAT )
                        IAT = IAT + 28
                        IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                        CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )

*  Upper bound of first strip.
                        MESS = ' '
                        MESS( 5 : )  = 'BOUNDS.END1'
                        CALL CHR_ITOC( BOUNDS( 2 ), MESS( 28: ), IAT )
                        IAT = IAT + 28
                        IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                        CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                     END IF
                     IF ( NBOUND .EQ. 4 ) THEN

*  Lower bound of second strip.
                        MESS = ' '
                        MESS( 5 : )  = 'BOUNDS.START2'
                        CALL CHR_ITOC( BOUNDS( 3 ), MESS( 28: ), IAT )
                        IAT = IAT + 28
                        IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                        CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )

*  Upper bound of second strip.
                        MESS = ' '
                        MESS( 5 : )  = 'BOUNDS.END2'
                        CALL CHR_ITOC( BOUNDS( 4 ), MESS( 28: ), IAT )
                        IAT = IAT + 28
                        IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                        CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                     END IF
                  END IF

*=======================================================================
*  Look for a bias value.
*=======================================================================
* This value is quite likely to be missing and not specified.
                  CALL CCD1_XADD( 'BIASVALUE', LOCEXT, 'ZERO', MODIFY,
     :                            .FALSE., DVAL, OK, FRMEXT, STATUS )
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'ZERO'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS ( IAT :IAT ) ='*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
*  Get a the useful CCD region section.
*=======================================================================
*  Look for existing values.
                  NEED = .TRUE.
                  FRMEXT = .FALSE.
                  CALL DAT_THERE( LOCEXT, 'EXTENT', THERE, STATUS )
                  IF ( MODIFY .OR. .NOT. THERE ) THEN

*  Try for values from the environment. Switch off message about the
*  size of the NDF. This is not usually needed in this routine.
                     CALL MSG_IFLEV( MSGLEV, ' ', STATUS )
                     CALL MSG_IFSET( MSG__QUIET, STATUS )
                     CALL CCD1_GTSEC( .FALSE., NDFID, LBND, UBND,
     :                                LBND2, UBND2, OK, STATUS )

                     IF ( STATUS .EQ. PAR__NULL ) THEN

*  No value supplied cancel error and proceed.
                        CALL ERR_ANNUL( STATUS )
                        CALL MSG_IFSET( MSGLEV, STATUS )
                     ELSE

*  Add the values to the extension
                        CALL MSG_IFSET( MSGLEV, STATUS )
                        BOUNDS( 1 ) = LBND2( 1 )
                        BOUNDS( 2 ) = UBND2( 1 )
                        BOUNDS( 3 ) = LBND2( 2 )
                        BOUNDS( 4 ) = UBND2( 2 )
                        CALL CCG1_STO1I( NDFID, 'EXTENT',
     :                                   'CCDPACK_XITEM',
     :                                   'MINX,MAXX,MINY,MAXY,', 4,
     :                                   BOUNDS, STATUS )
                        NEED = .FALSE.
                     END IF
                  END IF

*  Get the current values if they exist and we have not been allowed
*  to modify them.
                  IF ( THERE .AND. NEED ) THEN
                     CALL CCG1_FCH1I( NDFID, 'EXTENT',
     :                                'MINX,MAXX,MINY,MAXY,', 4, BOUNDS,
     :                                OK, STATUS )
                     NEED = .FALSE.
                     FRMEXT = .TRUE.
                  END IF

*  If we have some values write them out to the user.
                  IF ( .NOT. NEED ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'EXTENT.MINX'
                     CALL CHR_ITOC( BOUNDS( 1 ), MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) ='*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )

                     MESS = ' '
                     MESS( 5 : )  = 'EXTENT.MAXX'
                     CALL CHR_ITOC( BOUNDS( 2 ), MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) ='*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )

                     MESS = ' '
                     MESS( 5 : )  = 'EXTENT.MINY'
                     CALL CHR_ITOC( BOUNDS( 3 ), MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) ='*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )

                     MESS = ' '
                     MESS( 5 : )  = 'EXTENT.MAXY'
                     CALL CHR_ITOC( BOUNDS( 4 ), MESS( 28: ), IAT )
                     IAT = IAT + 28
                     IF ( FRMEXT ) MESS( IAT :IAT ) ='*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
* Get the ADC factor.
*=======================================================================
                  CALL CCD1_XADD( 'ADC', LOCEXT, 'ADC', MODIFY,
     :                            .FALSE., DVAL, OK, FRMEXT, STATUS )
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'ADC'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT +  28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
*  Get the readout noise.
*=======================================================================
                  CALL CCD1_XADD( 'RNOISE', LOCEXT, 'RNOISE', MODIFY,
     :                            .FALSE., DVAL, OK, FRMEXT, STATUS )
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'RNOISE'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT +  28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
*  Get the deferred charge value.
*=======================================================================
                  CALL CCD1_XADD( 'DEFERRED', LOCEXT, 'DEFERRED',
     :                            MODIFY, .FALSE., DVAL, OK, FRMEXT,
     :                            STATUS )
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'DEFERRED'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT +  28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
*  Get the saturation value.
*=======================================================================
                  CALL CCD1_XADD( 'SATURATION', LOCEXT, 'SATURATION',
     :                            MODIFY, .FALSE., DVAL, OK, FRMEXT,
     :                            STATUS )
                  IF ( OK ) THEN
                     MESS = ' '
                     MESS( 5 : )  = 'SATURATION'
                     CALL CHR_DTOC( DVAL, MESS( 28: ), IAT )
                     IAT = IAT +  28
                     IF ( FRMEXT ) MESS( IAT :IAT ) = '*'
                     CALL CCD1_MSG( ' ', MESS( :IAT ), STATUS )
                  END IF

*=======================================================================
*  Add an item identifying this NDF as the "ORIGINAL" it one doesn't
*  exist already.
*=======================================================================
                  CALL DAT_THERE( LOCEXT, 'ORIGINAL', OK, STATUS )
                  IF ( .NOT. OK ) THEN
                     CALL NDF_MSG( 'NDFNAM', NDFID )
                     CALL MSG_LOAD( ' ', '^NDFNAM', NDFNAM, NAMLEN,
     :                              STATUS )
                     CALL DAT_NEW0C( LOCEXT, 'ORIGINAL', NAMLEN,
     :                               STATUS )
                     CALL CMP_PUT0C( LOCEXT, 'ORIGINAL',
     :                               NDFNAM( :NAMLEN ), STATUS )
                  END IF
               END IF      !  Of not being a master.

*=======================================================================
*  Close this NDF and release all locators.
*=======================================================================
               CALL DAT_ANNUL( LOCEXT, STATUS )
               CALL NDF_ANNUL( NDFID, STATUS )
 2          CONTINUE
         END IF
 1    CONTINUE

 99   CONTINUE
      END
* $Id$
