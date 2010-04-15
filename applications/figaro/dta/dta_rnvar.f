C+
      SUBROUTINE DTA_RNVAR (OLD,NEW,STATUS)
C
C     D T A _ R N V A R
C
C     Renames a data structure object.  This routine can
C     also be used to modify the dimensions of an object.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) OLD      (Character) Current name of the object to
C                  be renamed.  Any dimensional information
C                  in OLD is ignored.  A top-level name cannot
C                  be changed by this routine.
C     (>) NEW      (Character) The new name for the object. The first
C                  level of NEW should be the same as for OLD (ie they
C                  should be in the same file).  The name can
C                  contain dimensional information if this is to be
C                  changed.  If arrays are made smaller, some of the
C                  contents will be lost, of course - this applies
C                  to structure arrays too.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  DTA_NOTFND => Object does not exist
C                  DTA_DIFENV => OLD and NEW differ in their
C                                environments (ie are in different
C                                files).
C                  DTA_RENTOP => Attempt to rename a top level object.
C                  DTA_NOENV  => No environment for new object.
C                  DTA_EXIST  => New object already exists.
C-
C     Subroutines / functions used -
C
C     DTA_SPLITN  (DTA_ package) Analyse object name
C     DTA_LOCATE  ( "     "    ) Locate data object
C     DTA_CACHEO  ( "     "    ) Remove object from name cache
C     DTA_FILL    ( "     "    ) Fill an array with a single byte
C     DTA_COPY    ( "     "    ) Fast copy of a byte array
C     DTA_TRNAME  ( "     "    ) Tidy a name to conform to HDS
C     DTA_DLVAR   ( "     "    ) Delete a data object
C     DTA_HDSTYP  ( "     "    ) Get HDS type name from a DTA type
C     DTA_DTATYP  ( "     "    ) Get DTA type name from an HDS type
C     DTA_HDSERC  ( "     "    ) Convert HDS error code to a DTA code
C     DAT_TYPE    (HDS    "    ) Get type of an HDS object
C     DAT_SHAPE   ( "     "    ) Get dimensions of an HDS object
C     DAT_PRIM    ( "     "    ) See if an HDS object is primitive
C     DAT_CLONE   ( "     "    ) Clone an HDS locator
C     DAT_ALTER   ( "     "    ) Change last dimension of an HDS object
C     DAT_MOVE    ( "     "    ) Move an HDS object
C     DAT_STATE   ( "     "    ) See if an object is defined
C     DAT_ANNUL   ( "     "    ) Annul an HDS locator
C     DAT_NEW     ( "     "    ) Create a new HDS object
C     DAT_FIND    ( "     "    ) Get locator to an existing HDS object
C     DAT_BASIC   ( "     "    ) Map an HDS object as a byte array
C     DAT_UNMAP   ( "     "    ) Unmap an HDS object
C     DAT_MOULD   ( "     "    ) Change shape of an HDS object
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C
C                                    KS / CIT 24th Nov 1982
C     Modified:
C
C     2nd  Apr 1986  KS / AAO. Re-written for the new HDS based
C                    version of the DTA routines.  The limitations
C                    on the allowed dimensional changes have been
C                    removed (note that most changes in dimension
C                    have to be achieved through the creation of a
C                    new data object).  OLD and NEW may now differ
C                    in their immediate environments, so long as
C                    they are still part of the same file.
C     12th Mar 1987  KS / AAO. Modified to make use of the new routine
C                    DAT_MOULD, available with the 'C' version of HDS.
C                    This increases the range of size/shape changes
C                    that may be done with copying.  This routine can
C                    no longer be used with the original Bliss version
C                    of HDS. (Or any version prior to 3.4)
C     10th Jun 1988  KS / AAO.  No longer refuses to operate on arrays
C                    of structures.
C     20th Jun 1988  KS / AAO.  Check for non-empty structure elements
C                    and delete them prior to reduction in size of a
C                    structure array.
C     10th Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN. Unused variable ADIM
C                    removed.
C     20th Jan 1992  KS / AAO. DAT_MOVE annuls the locator of the moved
C                    object. With the new EMS-using version of HDS,
C                    any subsequent call to DAT_ANNUL for the same locator
C                    will fail, and now, even if the HDS status is ignored,
C                    the fact that an EMS error has been signalled stores up
C                    trouble for later.  Such calls to DAT_ANNUL are now
C                    avoided.
C     24th Jan 1992  KS / AAO. Calls to EMS added to control error reporting.
C     14th Oct 1992  HME / UoE, Starlink.  Locating one level up is
C                    not as easy as calling DTA_LOCATE with OLEVELS-1.
C                    One must make sure that OLASTC(OLEVELS-1) points
C                    in front of any array index.
C     12th Mar 1993  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C     2005 May 31    MJC/Starlink Use CNF_PVAL for pointers to mapped
C                    data.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) OLD,NEW
      INTEGER STATUS
C
C     DTA system error codes
C
      INCLUDE 'DTACODES'
C
C     DTA system definitions
C
C     DST_MAXDIM   Maximum number of object dimensions
C     DST_MAXLEV   Maximum number of object levels
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      LOGICAL FOUND,MOVE,ALTER,PRIM,SAME,DEFINED
      INTEGER OLASTC(DST_MAXLEV),NLASTC(DST_MAXLEV),LASTC2(DST_MAXLEV),
     :   ADIMS(DST_MAXDIM)
      INTEGER DIMS(DST_MAXDIM),DIMS2(DST_MAXDIM),EMSTAT,OLEVELS,LEVEL2,
     :   NLEVELS,NDIM,NDIM2,STAT2
      INTEGER LEV,I,POINT1,POINT2,SIZE1,SIZE2,ACTDIM,NCHAR,DUMMY
      INTEGER TMPDIM(DST_MAXDIM),NELM,ACTELM,ICOMP,NCOMP,IELM
      CHARACTER*(DAT__SZLOC) LOC,TLOC,ENVLOC,OBJLOC,TLOC2,TLOC3
      CHARACTER*16 OBJNAM,HDSTYPE,TYPE,NEWNAM,CNAME
      CHARACTER*80 FOLD,FNEW
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert OLD and NEW into upper case
C
      CALL DTA_TRNAME(OLD,FOLD)
      CALL DTA_TRNAME(NEW,FNEW)
C
C     Analyse both names.
C
      CALL DTA_SPLITN(FOLD,DST_MAXLEV,DST_MAXDIM,OLEVELS,OLASTC,
     :                                             NDIM,DIMS,STATUS)
      IF (STATUS.NE.0)  GO TO 600
      CALL DTA_SPLITN(FNEW,DST_MAXLEV,DST_MAXDIM,NLEVELS,NLASTC,
     :                                             NDIM,DIMS,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     See if they are in the same file, and make sure that neither
C     are top level names.
C
      IF (FOLD(:OLASTC(1)).NE.(FNEW(:NLASTC(1)))) THEN
         STATUS=DTA_DIFENV
         GO TO 600
      END IF
      IF ((NLEVELS.EQ.1).OR.(OLEVELS.EQ.1)) THEN
         STATUS=DTA_RENTOP
         GO TO 600
      END IF
C
C     We analyse the new name up to one level higher, because we want to
C     locate the parent (environment) or the object to delete.
C
      CALL DTA_SPLITN(FOLD(:OLASTC(OLEVELS-1)),
     :   DST_MAXLEV,DST_MAXDIM,LEVEL2,LASTC2,NDIM2,DIMS2,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     See if it's the same object - ie just a re-dimensioning
C
      SAME=FOLD(:OLASTC(OLEVELS)).EQ.FNEW(:NLASTC(NLEVELS))
C
C     Locate the object and its new environment (Note: the use of
C     two calls to DTA_LOCATE assumes that the name cache is large
C     enough that the locator found in the first call will not
C     have to be annulled as a result of the search involved in
C     the second call.  It might be safer to clone that 1st locator.)
C
      CALL DTA_LOCATE(FOLD,OLEVELS,OLASTC,OBJLOC,FOUND,
     :                                  ENVLOC,LEV,STATUS)
      IF (STATUS.NE.0)  GO TO 600
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOTFND
         GO TO 600
      END IF
      IF (.NOT.SAME) THEN
C
C        Objects are different, so look for the new object.  We
C        don't expect it to exist, so we expect the locator to
C        its environment to be returned.
C
         CALL DTA_LOCATE(FNEW,NLEVELS,NLASTC,LOC,FOUND,ENVLOC,
     :                                              LEV,STATUS)
         IF (STATUS.NE.0) GO TO 600
         IF (FOUND) THEN
            STATUS=DTA_EXIST
            GO TO 600
         END IF
         IF (LEV.NE.NLEVELS) THEN
            STATUS=DTA_NOENV
            GO TO 600
         END IF
      ELSE
C
C        The objects are the same, so we will need the
C        environment locator for the object, in case we have
C        to copy it within its own environment.
C        We have to use LASTC2 here. One might think that's the same as
C        LASTC, but sometimes it is not. If you want to delete
C        REFNAME.AXIS[3].DATA_ARRAY, then LASTC(2) points to the ']',
C        while LASTC2(2) points in front of the '[', as needed by
C        DTA_LOCATE.
C
         CALL DTA_LOCATE(FOLD,OLEVELS-1,LASTC2,ENVLOC,FOUND,
     :      LOC,LEV,STATUS)
         IF (STATUS.NE.0)  GO TO 600
      END IF
C
C     Get the lowest level name for the new object
C
      OBJNAM=FNEW(NLASTC(NLEVELS-1)+2:NLASTC(NLEVELS))
C
C     Can we do this as a simple call to DAT_MOVE, or is a change in
C     dimensions involved?  A change in dimensions that does not involve
C     an increase in the number of dimensions, may be done using a
C     combination of DAT_MOVE, DAT_ALTER, and DAT_MOULD.  An increase
C     in the number of dimensions requires that we make a copy of the object.
C
C     We start by assuming that a 'move 'will be OK, and it will not
C     need to be followed by an 'alter'.
C
      MOVE=.TRUE.
      ALTER=.FALSE.
C
C     See if dimensional information was included in the new name.
C
      IF (NDIM.GT.0) THEN
C
C        It was.  See what the current dimensions of the object are.
C        Also get its type and see if it is defined.
C
         CALL DAT_TYPE(OBJLOC,HDSTYPE,STATUS)
         CALL DAT_PRIM(OBJLOC,PRIM,STATUS)
         IF (PRIM) THEN
            CALL DAT_STATE(OBJLOC,DEFINED,STATUS)
         ELSE
            DEFINED=.FALSE.
         END IF
         CALL DAT_SHAPE(OBJLOC,DST_MAXDIM,ADIMS,ACTDIM,STATUS)
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
            GO TO 600
         END IF
C
C        Character arrays introduce an additional complication, because
C        they are held as arrays of strings rather than as arrays of
C        single characters.   See if this is a character array.
C
         CALL DTA_DTATYP(HDSTYPE,TYPE,NCHAR,STATUS)
         IF (STATUS.NE.0) GO TO 600
         IF (TYPE.EQ.'CHAR') THEN
C
C           The main differences between the way we deal with character
C           and other arrays is that any change to the first dimension
C           of a character array (which is the string length and comes
C           from the HDS type) has to be done by copying.  Apart from
C           that, each array dimension in HDS terms is the one higher
C           dimension in DTA terms.  We change the DIMS array and
C           modify the HDS type in case we have to create a new object.
C
            IF (DIMS(1).NE.NCHAR) MOVE=.FALSE.
            CALL DTA_HDSTYP(TYPE,DIMS(1),HDSTYPE,STATUS)
            IF (STATUS.NE.0) GO TO 600
            DO I=2,NDIM
               DIMS(I-1)=DIMS(I)
            END DO
            NDIM=NDIM-1
         END IF
C
C        Now we can check the specified dimensions against those that
C        the object has now.  We are looking to see if the change may be
C        made using DAT_MOVE, DAT_ALTER and DAT_MOULD and we will set
C        MOVE false if this is not the case.  If it can be done in this
C        way, we set ALTER true if there does in fact have to be a change
C        in size and/or shape.
C
         IF (NDIM.GT.ACTDIM) THEN
            MOVE=.FALSE.
         ELSE
C
C           The change does not involve an increase in dimensions, so may
C           be done in situ.  We want to find out if the new size (NELM)
C           is different to the actual size at present (ACTELM), because
C           things are simpler later if this is the case.
C
            NELM=1
            DO I=1,NDIM
               NELM=NELM*DIMS(I)
            END DO
            ACTELM=1
            DO I=1,ACTDIM
               ACTELM=ACTELM*ADIMS(I)
            END DO
            IF (NDIM.NE.ACTDIM) THEN
               ALTER=.TRUE.
            ELSE
               DO I=1,NDIM
                  IF (DIMS(I).NE.ADIMS(I)) ALTER=.TRUE.
               END DO
            END IF
         END IF
      END IF
C
      IF (MOVE) THEN
C
C        We can do it by a 'move'.  Any re-dimensioning may be done
C        in situ using DAT_MOULD and DAT_ALTER, Note that since the
C        'move' will annul the original locator we have to
C        use a clone of it.  This is so DTA_CACHEO can annul the
C        original locator as part of the cleaning up process.
C
         CALL DAT_CLONE(OBJLOC,TLOC,STATUS)
         IF (ALTER) THEN
            IF (NELM.EQ.ACTELM) THEN
C
C              A change in shape without a change in size may be done
C              using a simple call to DAT_MOULD.
C
               CALL DAT_MOULD(TLOC,NDIM,DIMS,STATUS)
            ELSE
C
C              If the size has to be changed, a trick is involved.
C              DAT_ALTER may change the size of an object, but it
C              can only change the last dimension.  DAT_MOULD may
C              change the dimensions of an object, but not its total
C              size.  So we mould the object into its desired number
C              of dimensions but with its current size, making its
C              dimensions [1,1,..,ACTELM], alter this so that it has
C              the desired number of dimensions and the desired size
C              by making it [1,1,...,NELM], and then mould it into
C              the desired shape.
C
               DO I=1,NDIM-1
                  TMPDIM(I)=1
               END DO
               TMPDIM(NDIM)=ACTELM
               CALL DAT_MOULD(TLOC,NDIM,TMPDIM,STATUS)
C
C              Now, if we are dealing with a structure array, and are
C              reducing its size, DAT_ALTER will refuse if any elements
C              that would be deleted are not empty.  So we have to check
C              for that and delete them first.
C
               IF (.NOT.PRIM) THEN
                  DO IELM=NELM+1,ACTELM
                     TMPDIM(NDIM)=IELM
                     CALL DAT_CELL(TLOC,NDIM,TMPDIM,TLOC2,STATUS)
                     CALL DAT_NCOMP(TLOC2,NCOMP,STATUS)
                     IF (NCOMP.NE.0) THEN
                        DO ICOMP=1,NCOMP
                           CALL DAT_INDEX(TLOC2,ICOMP,TLOC3,STATUS)
                           CALL DAT_NAME(TLOC3,CNAME,STATUS)
                           CALL DAT_ANNUL(TLOC3,STATUS)
                           CALL DAT_ERASE(TLOC2,CNAME,STATUS)
                        END DO
                        CALL DAT_ANNUL(TLOC2,STATUS)
                     END IF
                  END DO
               END IF
C
C              Finally, we can change the number of elements and set
C              the required shape.
C
               TMPDIM(NDIM)=NELM
               CALL DAT_ALTER(TLOC,NDIM,TMPDIM,STATUS)
               CALL DAT_MOULD(TLOC,NDIM,DIMS,STATUS)
            END IF
         END IF
C
C        Now, if necessary, change the name (which annuls TLOC). Otherwise
C        just annul TLOC.
C
         IF (.NOT.SAME) THEN
            CALL DAT_MOVE(TLOC,ENVLOC,OBJNAM,STATUS)
         ELSE
            DUMMY=0
            CALL DAT_ANNUL(TLOC,DUMMY)
         END IF
C
C        Check the status - which will be an HDS status code, and
C        if OK, use DTA_CACHEO to remove all traces of the original
C        object from the name cache.
C
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
         ELSE
            CALL DTA_CACHEO(FOLD,OLEVELS,OLASTC,STATUS)
         END IF
      ELSE
C
C        Has to be done by a copy.  First create the new object
C        and map the old.  If the new has the same name as the
C        old, we have to use a temporary name at this stage.
C        The actual copy will have to be bypassed if the data is
C        undefined.
C
         IF (SAME) THEN
            NEWNAM='__TEMP__'
         ELSE
            NEWNAM=OBJNAM
         END IF
         CALL DAT_NEW(ENVLOC,NEWNAM,HDSTYPE,NDIM,DIMS,STATUS)
         IF ((STATUS.EQ.0).AND.(DEFINED)) THEN
            CALL DAT_BASIC(OBJLOC,'READ',POINT1,SIZE1,STATUS)
            IF (STATUS.EQ.0) THEN
C
C              Get a locator to the new object and map it.
C
               CALL DAT_FIND(ENVLOC,NEWNAM,TLOC,STATUS)
               IF (STATUS.EQ.0) THEN
                  CALL DAT_BASIC(TLOC,'WRITE',POINT2,SIZE2,STATUS)
                  IF (STATUS.EQ.0) THEN
C
C                    Copy the old into the new, then fill up any
C                    extra space in the new object with blanks or zeros.
C
                     CALL DTA_COPY(MIN(SIZE1,SIZE2),
     :                             %VAL( CNF_PVAL(POINT1) ),
     :                             %VAL( CNF_PVAL(POINT2) ) )
                     IF (SIZE2.GT.SIZE1) THEN
                        IF (TYPE.EQ.'CHAR') THEN
                           CALL DTA_FILL(SIZE2-SIZE1,ICHAR(' '),%VAL(
     :                                   CNF_PVAL(POINT2+SIZE1) ) )
                        ELSE
                           CALL DTA_FILL(SIZE2-SIZE1,0,%VAL(
     :                                   CNF_PVAL(POINT2+SIZE1) ) )
                        END IF
                     END IF
C
C                    On the way out we unmap anything we managed to map,
C                    and annul any temporary locators.
C
                     CALL DAT_UNMAP(TLOC,STATUS)
                  END IF
                  DUMMY=0
                  CALL DAT_ANNUL(TLOC,DUMMY)
               END IF
               STAT2=0
               CALL DAT_UNMAP(OBJLOC,STAT2)
               IF (STATUS.EQ.0) STATUS=STAT2
            END IF
         END IF
C
C        Check status - which will be an HDS code, and if OK,
C        delete the original object.
C
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
            GO TO 600
         ELSE
            CALL DTA_DLVAR(FOLD(:OLASTC(OLEVELS)),STATUS)
            IF (STATUS.NE.0) GO TO 600
         END IF
C
C        With the original object deleted, we should now be able
C        to rename the temporarily named new object back to its
C        original name.  This is only necessary if we had to use a
C        temporary name, of course.
C
         IF (SAME) THEN
            DUMMY=0
            CALL DAT_FIND(ENVLOC,NEWNAM,TLOC,STATUS)
            CALL DAT_MOVE(TLOC,ENVLOC,OBJNAM,STATUS)
            IF (STATUS.NE.0) CALL DTA_HDSERC(STATUS)
         END IF
      END IF
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END
