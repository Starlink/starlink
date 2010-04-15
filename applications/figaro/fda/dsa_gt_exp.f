      SUBROUTINE DSA_GET_EXPOSURE( DSAREF, DEFAULT, MAXINV,
     :   TIME, STATUS )
*+
*  Name:
*     DSA_GET_EXPOSURE

*  Purpose:
*     Return the exposure time associated with an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_EXPOSURE( DSAREF, DEFAULT, MAXINV, TIME, STATUS )

*  Description:
*     Some NDFs will have an exposure time associated
*     with them. This routine returns that time, if it exists, or a
*     supplied default if it does not. The time is by convention stored
*     in seconds, and that is returned by this routine without
*     conversion.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     DEFAULT = REAL (Given)
*        A default time that may be used if no time value is in fact
*        associated with the structure, or if the time is invalid.
*     MAXINV = REAL (Given)
*        The maximum exposure time that is to be treated as invalid.
*        (In plain but unprecise English the lower acceptable limit.)
*        Usually, this will be zero, since it is usually easy for
*        uninitialised values to look as if they are zero, and negative
*        times are generally invalid.
*     TIME = REAL (Returned)
*        The exposure time associated with the structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Set the time to an invalid value.
*     Begin an error context.
*     Attempt to look up the reference slot.
*     If ok then
*       Attempt to get a locator for the exposure time
*       If ok then
*         Attempt to get the exposure time.
*         If ok then
*           If the exposure time is longer than longest invalid time then
*             Adopt the exposure time.
*           else
*             Report a message: the exposure time is invalid.
*           end if
*         end if
*         Annul the locator.
*       end if
*       if status not ok then
*         Annul the error
*         Report a message: unable to get the exposure time.
*       end if
*     end if
*     if status ok then
*        Translate ADAM status to Figaro status.
*     else
*        flush the error
*        Translate ADAM status to Figaro status.
*     end if
*     If the time is still invalid then
*       Adopt the default time.
*       Report a message: adopting the default time.
*     end if
*     End the error context.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1987 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Now uses DSA__ routines rather than just assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     02 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     21 Dec 2000 (acd):
*        Removed unused variables.
*     17 May 2001 (acd):
*        Re-written to make the handling of error conditions and reporting
*        of error messages clearer and more elegant.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      REAL DEFAULT
      REAL MAXINV

*  Arguments Returned:
      REAL TIME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( DAT__SZLOC ) TLOC ! HDS locator
      REAL LTIME                 ! Local copy of exposure time.

*.

*
*    Set the return argument to the default in case the routine aborts
*    with a bad status on entry.

      TIME = DEFAULT

*
*    Check the inherited global status.

      IF (STATUS .NE. 0) RETURN

*
*    Set the time to an invalid value (exposure times cannot be negative).

      TIME = -2.0E0

*
*    Initialise the ADAM status and begin an error context.

      STATUS = SAI__OK
      CALL ERR_MARK

*
*    Attempt to look up the reference slot and proceed if ok.

      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get a locator for the exposure time and proceed if ok.

         CALL NDF_XLOC (DSA__REFID1(SLOT), 'FIGARO', 'READ', TLOC,
     :     STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to get the exposure time and proceed if ok.

            CALL CMP_GET0R (TLOC, 'TIME', LTIME, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN

*
*             Check whether the exposure time is longer than longest
*             the invalid time. If so then adopt it, otherwise report a
*             message.

               IF (LTIME .GT. MAXINV) THEN
                  TIME = LTIME

               ELSE
                  CALL MSG_SETR ('LTIME', LTIME)
                  CALL MSG_SETC ('FDA_T001', DSA__REFNAM(SLOT))

                  CALL MSG_OUT( 'FDA_M012', 'Warning: invalid '/
     :              /'exposure time of ^LTIME obtained from reference '/
     :              /'^FDA_T001.', STATUS)

                  CALL MSG_SETR ('MAXINV', MAXINV)

                  CALL MSG_OUT( 'FDA_M019', 'Warning: exposure times '/
     :              /'must be longer than ^MAXINV.', STATUS)
               END IF
            END IF

*
*          Annul the locator to the exposure time.

            CALL DAT_ANNUL (TLOC, STATUS)
         END IF

*
*       If any error occurred getting the exposure time then annul the
*       error and report a message.  Here we are basically handling the
*       case where the exposure time is not present in the structure.
*
*       Note that the case where DSA is unable to look up a slot for the
*       given reference frame is deliberately excluded.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
            CALL MSG_OUT( 'FDA_M011', 'Warning: unable to obtain '/
     :        /'an exposure time from reference ^FDA_T001.', STATUS)
         END IF
      END IF

*
*    Translate the ADAM status to a Figaro status and flush any error.
*    If the attempt to look up the reference slot failed then the error
*    messages generated within DSA1_RFND will be reported at this point.

      IF (STATUS .EQ. SAI__OK) THEN
         STATUS = 0
      ELSE
         CALL ERR_FLUSH (STATUS)
         STATUS = 1
      END IF

*
*    Check whether a valid exposure time has been obtained.  If not then
*    adopt the default and report a message.

      IF (TIME .LE. -1.0E0) THEN
         TIME = DEFAULT

         CALL MSG_SETR ('DEFAULT', DEFAULT)
         CALL MSG_OUT( 'FDA_M020', 'Warning: adopting a default '/
     :     /'exposure time of ^DEFAULT.', STATUS)
      END IF

*
*    End the error context.

      CALL ERR_RLSE

*
*    Return.

      END
