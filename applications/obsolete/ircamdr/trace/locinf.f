*+  LOCINF - Get information about an object with a given locator.
      SUBROUTINE LOCINF( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
     :  STATUS )
*    Description :
*     Information regarding the data system object with locator OBJLOC
*     is obtained from the data system.
*    Invocation :
*      CALL LOCINF( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
*     :  STATUS )
*    Parameters :
*     OBJLOC = CHARACTER*(DAT__SZLOC)( READ )
*           Locator to the object for which information is to be obtained.
*     NAME = CHARACTER*(DAT__SZNAM)( WRITE )
*           The name associated with the object.
*     PRIM = LOGICAL( WRITE )
*           Set to .TRUE. if the object is of primitive type.
*     TYPE = CHARACTER*(DAT__SZTYP)( WRITE )
*           The type of the object.
*     SIZE = INTEGER( WRITE )
*           The total number of elements associated with the object,
*           i.e. if treated as a vector.
*     NDIM = INTEGER( WRITE )
*           The actual dimensionality of the object.
*     DIMS( DAT__MXDIM ) = INTEGER( WRITE )
*           The first NDIM elements will be the actual dimensions of the
*           object.
*     STATUS = INTEGER( UPDATE )
*           The global status, if an error occurs during the execution
*           of this routine then STATUS will be returned containing the
*           appropriate error message.
*    Method :
*     Use the DAT_ enquiry routines to find out the information.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     07/04/1984 : Original version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*( DAT__SZLOC )
     :  OBJLOC ! locator to the object to be examined
*    Export :
      CHARACTER*( DAT__SZNAM )
     :  NAME ! object name
      LOGICAL
     :  PRIM ! returned .TRUE. if object is of primitive type
      CHARACTER*( DAT__SZTYP )
     :  TYPE ! the type of the object
      INTEGER
     :  SIZE, ! number of elements for the object if treated as a vector
     :  NDIM, ! dimensionality of the object
     :  DIMS( DAT__MXDIM ) ! actual dimensions of the object
*    Status :
      INTEGER STATUS ! the global status
*-

*    get the name of the object associated with this locator
      CALL DAT_NAME( OBJLOC, NAME, STATUS )

*    find out if the object is primitive
      CALL DAT_PRIM( OBJLOC, PRIM, STATUS )

*    find out what type it is
      CALL DAT_TYPE( OBJLOC, TYPE, STATUS )

*    how many elements are associated with it
      CALL DAT_SIZE( OBJLOC, SIZE, STATUS )

*    what is its dimensionality and actual dimensions
      CALL DAT_SHAPE( OBJLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

      END
