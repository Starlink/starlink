      SUBROUTINE
     :  CHU_SAMPLE(INPUT, OUTPUT, REJFLG, REJECTS, FREQ, STATUS)

*+
*  Name:
*     CHU_SAMPLE

*  Purpose:
*     Select every Nth object in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHU_SAMPLE(INPUT, OUTPUT, REJFLG, REJECTS, FREQ, STATUS)

*  Description :
*     Create an output catalogue containing an entry for every Nth
*     object in an input catalogue. Optionally a second output
*     catalogue containing the rejected objects can be created.
*     The user can select the value required for N.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue whose fields are to be correlated.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the output catalogue.
*     REJFLG = LOGICAL(Given)
*        Is a rejects catalogue required.
*     REJECTS = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the rejects catalogue.
*     FREQ = INTEGER(Given)
*        Frequency at which to sample.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'          ! Standard CHP constants
      INCLUDE 'CHP_ERR'          ! Standard CHP errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) REJECTS
      INTEGER FREQ
      LOGICAL REJFLG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER
     :  MAXFLD   ! Max. permissible number of fields.
      PARAMETER
     : (MAXFLD = 50)

*  Local Variables:
      integer RECSAMP  ! Number of records since last sample.
      integer NUMRECS  ! Number of records in the input cataloge
*      character*(chi__szpcmt) cvalue
*      character*(chi__szpcmt)  form
*      character*(chi__szpcmt)  newform
      integer nfields
      integer coldes(chp__numcols)
      integer intvals(chp__numcols)
      integer ptrvals(chp__numcols)
      real    realvals(chp__numcols)
      logical logvals(chp__numcols)
      logical nulls(chp__numcols)
      double precision doubvals(chp__numcols)
      character*(chp__szcval) charvals(chp__numcols)
      character*(chp__szcname) fnames(chp__numcols)
      character*(1) fieldtypes(chp__numcols)
      integer reccount
      logical retorder

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Create an empty output catalogue retaining the order..
*
      RETORDER = .TRUE.
      CALL CHP_CREDUP(INPUT, OUTPUT, 500, STATUS)
*
      IF (STATUS .EQ. SAI__OK) THEN
*
*   Get the number of entries.
*
        CALL CHP_GNENTS(INPUT, NUMRECS, STATUS)
*
*   Is a rejects catalogue required.
*
        IF (REJFLG) THEN
*
*   The rejects catalogue is required.
*
*   Create an empty output catalogue retaining the order..
*
          RETORDER = .TRUE.
          CALL CHP_CREDUP(INPUT, REJECTS, 500, STATUS)
*
*  Loop through the input catalogue placing the entries in the output or
*  rejects catalogue.
*
          RECSAMP = 0
          DO RECCOUNT = 1, NUMRECS
            CALL CHP_GDNAC(INPUT, NFIELDS, FNAMES, FIELDTYPES,
     :  COLDES, CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS,
     :  PTRVALS, NULLS, STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
              RECSAMP = RECSAMP + 1
               IF (RECSAMP .EQ. FREQ) THEN
                CALL CHP_PUTENT(OUTPUT,1, NFIELDS, FNAMES, FIELDTYPES,
     :  CHARVALS,
     :  DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLS, STATUS)
                RECSAMP = 0
               ELSE
                CALL CHP_PUTENT(OUTPUT,1, NFIELDS, FNAMES, FIELDTYPES,
     :  CHARVALS,
     :  DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLS, STATUS)
               ENDIF
             ENDIF
          ENDDO
        ELSE
*
*   The rejects catalogue is not required.
*
*  Find the next Nth row position. Read the entry and
*  write it to the output catalogue.
*
          RECSAMP = FREQ
          DO RECCOUNT = 1, NUMRECS
            CALL CHP_GDNAC(INPUT, NFIELDS, FNAMES, FIELDTYPES,
     :  COLDES, CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS,
     :  PTRVALS, NULLS, STATUS)
            IF (STATUS .EQ. SAI__OK .AND. RECCOUNT .EQ. RECSAMP ) THEN
                CALL CHP_PUTENT(OUTPUT,1, NFIELDS, FNAMES, FIELDTYPES,
     :  CHARVALS,
     :  DOUBVALS, INTVALS, LOGVALS, REALVALS, PTRVALS, NULLS, STATUS)
             RECSAMP = RECSAMP + FREQ
            ENDIF
          ENDDO
        ENDIF
      ENDIF
*
      END
