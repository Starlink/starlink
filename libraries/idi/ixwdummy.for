************************************************************************

      SUBROUTINE IXWOPN( DEVNAM, LENAME, DISPID, STATUS )

*+
*  Name:
*     IXWOPN
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDOPN.
*
*  Invocation:
*     CALL IXWOPN( DEVNAM, DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DEVNAM = CHARACTER * ( * ) (Given)
*        Name of device to open
*     LENAME = INTEGER (Given)
*        Length of name string
*     DISPID = INTEGER (Returned)
*        Identifier for display
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) DEVNAM
      INTEGER LENAME

*  Arguments Returned:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWCLO( DISPID, STATUS )

*+
*  Name:
*     IXWCLO
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDCLO.
*
*  Invocation:
*     CALL IXWCLO( DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRST( DISPID, STATUS )

*+
*  Name:
*     IXWRST
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDRST.
*
*  Invocation:
*     CALL IXWRST( DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWUPD( DISPID, STATUS )

*+
*  Name:
*     IXWUPD
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDUPD.
*
*  Invocation:
*     CALL IXWUPD( DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWERR( STATUS, MESSAG, MESLEN )

*+
*  Name:
*     IXWERR
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDERR.
*
*  Invocation:
*     CALL IXWERR( STATUS, MESSAG, MESLEN )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.
*     MESSAG = CHARACTER * ( * ) (Returned)
*        Error message
*     MESLEN = INTEGER (Returned)
*        Length of error message
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER STATUS

*  Arguments Returned:
      CHARACTER * ( * ) MESSAG
      INTEGER MESLEN
*.

*   Subroutine not implemented
      MESSAG = ' '
      MESLEN = 0

      END

************************************************************************

      SUBROUTINE IXWQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
     :                   NITT, NCURS, STATUS )

*+
*  Name:
*     IXWQDV
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDQDV.
*
*  Invocation:
*     CALL IXWQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
*    :             NITT, NCURS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Returned)
*        Number of available configurations
*     XSIZE = INTEGER (Returned)
*        Display size in x ( pixels )
*     YSIZE = INTEGER (Returned)
*        Display size in y ( pixels )
*     DEPTH = INTEGER (Returned)
*        Display depth ( number of bits in DAC's )
*     NVLUT = INTEGER (Returned)
*        Number of VLUT's in device
*     NITT = INTEGER (Returned)
*        Number of ITT's per image memory
*     NCURS = INTEGER (Returned)
*        Number of cursors
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER NCONF
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER DEPTH
      INTEGER NVLUT
      INTEGER NITT
      INTEGER NCURS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IXWQCI
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDQCI.
*
*  Invocation:
*     CALL IXWQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CAPID = INTEGER (Given)
*        Capability
*     NARR = INTEGER (Given)
*        Size of output array
*     OUTARR( NARR ) = INTEGER (Returned)
*        Output array
*     NOUT = INTEGER (Returned)
*        Number of values returned
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CAPID
      INTEGER NARR

*  Arguments Returned:
      INTEGER OUTARR( NARR )
      INTEGER NOUT

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IXWQCr
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDQCr.
*
*  Invocation:
*     CALL IXWQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CAPID = INTEGER (Given)
*        Capability
*     NARR = INTEGER (Given)
*        Size of output array
*     OUTARR( NARR ) = REAL (Returned)
*        Output array
*     NOUT = INTEGER (Returned)
*        Number of values returned
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CAPID
      INTEGER NARR

*  Arguments Returned:
      REAL OUTARR( NARR )
      INTEGER NOUT

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON,
     :                   MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP,
     :                   NMEM, STATUS )

*+
*  Name:
*     IXWQDC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDQDC.
*
*  Invocation:
*     CALL IXWQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON, MEMID,
*    :             MEMSIX, MEMSIY, MEMDEP, ITTDEP, NMEM, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     MEMTYP = INTEGER (Given)
*        Memory type
*     NMEMAX = INTEGER (Given)
*        Maximum number of memories
*     MODCON = INTEGER (Returned)
*        Configuration mode
*     MEMID( * ) = INTEGER (Returned)
*        List of memory identifiers
*     MEMSIX( * ) = INTEGER (Returned)
*        Memory sizes in x
*     MEMSIY( * ) = INTEGER (Returned)
*        Memory sizes in y
*     MEMDEP( * ) = INTEGER (Returned)
*        Memory depths
*     ITTDEP( * ) = INTEGER (Returned)
*        Memory ITT depths
*     NMEM = INTEGER (Returned)
*        Number of memories
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF
      INTEGER MEMTYP
      INTEGER NMEMAX

*  Arguments Returned:
      INTEGER MODCON
      INTEGER MEMID( * )
      INTEGER MEMSIX( * )
      INTEGER MEMSIY( * )
      INTEGER MEMDEP( * )
      INTEGER ITTDEP( * )
      INTEGER NMEM

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSEL( DISPID, NCONF, STATUS )

*+
*  Name:
*     IXWSEL
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDSEL.
*
*  Invocation:
*     CALL IXWSEL( DISPID, NCONF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSMV( DISPID, MEMID, NMEM, LVIS, STATUS )

*+
*  Name:
*     IXWSMV
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMSMV.
*
*  Invocation:
*     CALL IXWSMV( DISPID, MEMID, NMEM, LVIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )

*+
*  Name:
*     IXWWSC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIZWSC.
*
*  Invocation:
*     CALL IXWWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     XOFF = INTEGER (Given)
*        X offset
*     YOFF = INTEGER (Given)
*        Y offset
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER XOFF
      INTEGER YOFF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )

*+
*  Name:
*     IXWWZM
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIZWZM.
*
*  Invocation:
*     CALL IXWWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     ZOOMF = INTEGER (Given)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IXWRSZ
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIZRSZ.
*
*  Invocation:
*     CALL IXWRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     XOFF = INTEGER (Returned)
*        X offset
*     YOFF = INTEGER (Returned)
*        Y offset
*     ZOOMF = INTEGER (Returned)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID

*  Arguments Returned:
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )

*+
*  Name:
*     IXWSLT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMSLT.
*
*  Invocation:
*     CALL IXWSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     LUTNUM = INTEGER (Given)
*        VLUT identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER LUTNUM
      INTEGER ITTNUM

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )

*+
*  Name:
*     IXWSDP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDSDP.
*
*  Invocation:
*     CALL IXWSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     LUTLIS( * ) = INTEGER (Given)
*        List of VLUT flags
*     ITTLIS( * ) = INTEGER (Given)
*        List of ITT flags
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER LUTLIS( * )
      INTEGER ITTLIS( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                   XSTART, YSTART, STATUS )

*+
*  Name:
*     IXWWMY
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMWMY.
*
*  Invocation:
*     CALL IXWWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
*    :             XSTART, YSTART, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     IMAGE( * ) = INTEGER (Given)
*        Image data
*     NPIX = INTEGER (Given)
*        Number of pixels
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     PACK = INTEGER (Given)
*        Packing factor. Number of pixels per longword
*     XSTART = INTEGER (Given)
*        Start position in X
*     YSTART = INTEGER (Given)
*        Start position in Y
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER IMAGE( * )
      INTEGER NPIX
      INTEGER DEPTH
      INTEGER PACK
      INTEGER XSTART
      INTEGER YSTART

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWCMY( DISPID, MEMID, NMEM, BACK, STATUS )

*+
*  Name:
*     IXWCMY
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMCMY.
*
*  Invocation:
*     CALL IXWCMY( DISPID, MEMID, NMEM, BACK, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     BACK = INTEGER (Given)
*        Background value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER BACK

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, ITTON, IMAGE, STATUS )

*+
*  Name:
*     IXWRMY
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMRMY.
*
*  Invocation:
*     CALL IXWRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
*    :             PACK, ITTON, IMAGE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NPIX = INTEGER (Given)
*        Number of pixels
*     XSTART = INTEGER (Given)
*        Start position in X
*     YSTART = INTEGER (Given)
*        Start position in Y
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     PACK = INTEGER (Given)
*        Packing factor. Number of pixels per longword
*     ITTON = LOGICAL (Given)
*        ITT flag
*     IMAGE( * ) = INTEGER (Returned)
*        Image data
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NPIX
      INTEGER XSTART
      INTEGER YSTART
      INTEGER DEPTH
      INTEGER PACK
      LOGICAL ITTON

*  Arguments Returned:
      INTEGER IMAGE( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
     :                   XOFF, YOFF, STATUS )

*+
*  Name:
*     IXWSTW
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMSTW.
*
*  Invocation:
*     CALL IXWSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
*    :             XOFF, YOFF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     DIRECN = INTEGER (Given)
*        Direction of load. 0 = up. 1 = down
*     XSIZE = INTEGER (Given)
*        Size of window in X
*     YSIZE = INTEGER (Given)
*        Size of window in Y
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     XOFF = INTEGER (Given)
*        Start position of window in X
*     YOFF = INTEGER (Given)
*        Start position of window in Y
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER DIRECN
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER DEPTH
      INTEGER XOFF
      INTEGER YOFF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                   STATUS )

*+
*  Name:
*     IXWPLY
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIGPLY.
*
*  Invocation:
*     CALL IXWPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     X( * ) = INTEGER (Given)
*        List of X positions
*     Y( * ) = INTEGER (Given)
*        List of Y positions
*     NXY = INTEGER (Given)
*        Number of ( x,y ) positions
*     COLOR = INTEGER (Given)
*        Colour
*     LSTYLE = INTEGER (Given)
*        Line style
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER X( * )
      INTEGER Y( * )
      INTEGER NXY
      INTEGER COLOR
      INTEGER LSTYLE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWTXT( DISPID, MEMID, TEXT, LENTXT, XPOS, YPOS, TPATH,
     :                   TANGLE, COLOR, TSIZE, STATUS )

*+
*  Name:
*     IXWTXT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIGTXT.
*
*  Invocation:
*     CALL IXWTXT( DISPID, MEMID, TEXT, LENTXT, XPOS, YPOS, TPATH,
*    :             TANGLE, COLOR, TSIZE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     TEXT = CHARACTER * ( * ) (Given)
*        Text
*     LENTXT = INTEGER (Given)
*        Length of text string
*     XPOS = INTEGER (Given)
*        X postion
*     YPOS = INTEGER (Given)
*        Y position
*     TPATH = INTEGER (Given)
*        Text path
*     TANGLE = INTEGER (Given)
*        Text orientation
*     COLOR = INTEGER (Given)
*        Pixel colour value
*     TSIZE = INTEGER (Given)
*        Text size
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      CHARACTER * ( * ) TEXT
      INTEGER LENTXT
      INTEGER XPOS
      INTEGER YPOS
      INTEGER TPATH
      INTEGER TANGLE
      INTEGER COLOR
      INTEGER TSIZE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IXWWIT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IILWIT.
*
*  Invocation:
*     CALL IXWWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     ITT( 3, NENT ) = REAL (Given)
*        Intensity transformation table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ITTNUM
      INTEGER START
      INTEGER NENT
      REAL ITT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IXWRIT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IILRIT.
*
*  Invocation:
*     CALL IXWRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     ITT( 3, NENT ) = REAL (Returned)
*        Intensity transformation table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ITTNUM
      INTEGER START
      INTEGER NENT

*  Arguments Returned:
      REAL ITT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IXWWLT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IILWLT.
*
*  Invocation:
*     CALL IXWWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LUTNUM = INTEGER (Given)
*        VLUT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     VLUT( 3, NENT ) = REAL (Given)
*        Video look-up table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LUTNUM
      INTEGER START
      INTEGER NENT
      REAL VLUT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IXWRLT
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IILRLT.
*
*  Invocation:
*     CALL IXWRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LUTNUM = INTEGER (Given)
*        VLUT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     VLUT( 3, NENT ) = REAL (Returned)
*        Video look-up table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LUTNUM
      INTEGER START
      INTEGER NENT

*  Arguments Returned:
      REAL VLUT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IXWWZP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIZWZP.
*
*  Invocation:
*     CALL IXWWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XOFF = INTEGER (Given)
*        X offset
*     YOFF = INTEGER (Given)
*        Y offset
*     ZOOMF = INTEGER (Given)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IXWRZP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIZRZP.
*
*  Invocation:
*     CALL IXWRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XOFF = INTEGER (Returned)
*        X offset
*     YOFF = INTEGER (Returned)
*        Y offset
*     ZOOMF = INTEGER (Returned)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR,
     :                   XC, YC, STATUS )

*+
*  Name:
*     IXWINC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IICINC.
*
*  Invocation:
*     CALL IXWINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC, YC, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     SHAPE = INTEGER (Given)
*        Cursor shape
*     COLOR = INTEGER (Given)
*        Cursor colour
*     XC = INTEGER (Given)
*        X cursor position
*     YC = INTEGER (Given)
*        Y cursor position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NUMCUR
      INTEGER SHAPE
      INTEGER COLOR
      INTEGER XC
      INTEGER YC

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSCV( DISPID, NUMCUR, LVIS, STATUS )

*+
*  Name:
*     IXWSCV
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IICSCV.
*
*  Invocation:
*     CALL IXWSCV( DISPID, NUMCUR, LVIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NUMCUR
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID,
     :                   STATUS )

*+
*  Name:
*     IXWRCP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IICRCP.
*
*  Invocation:
*     CALL IXWRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INMID = INTEGER (Given)
*        Input memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     XC = INTEGER (Returned)
*        X position
*     YC = INTEGER (Returned)
*        Y position
*     OUTMID = INTEGER (Returned)
*        Output memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INMID
      INTEGER NUMCUR

*  Arguments Returned:
      INTEGER XC
      INTEGER YC
      INTEGER OUTMID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )

*+
*  Name:
*     IXWWCP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IICWCP.
*
*  Invocation:
*     CALL IXWWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     XC = INTEGER (Given)
*        X position
*     YC = INTEGER (Given)
*        Y position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NUMCUR
      INTEGER XC
      INTEGER YC

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
     :                   ROIID, STATUS )

*+
*  Name:
*     IXWINR
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIRINR.
*
*  Invocation:
*     CALL IXWINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
*    :             ROIID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ROICOL = INTEGER (Given)
*        ROI marker colour
*     XMIN = INTEGER (Given)
*        Minimum X position
*     YMIN = INTEGER (Given)
*        Minimum Y position
*     XMAX = INTEGER (Given)
*        Maximum X position
*     YMAX = INTEGER (Given)
*        Maximum Y position
*     ROIID = INTEGER (Returned)
*        ROI identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ROICOL
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX

*  Arguments Returned:
      INTEGER ROIID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                   OUTMID, STATUS )

*+
*  Name:
*     IXWRRI
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIRRRI.
*
*  Invocation:
*     CALL IXWRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :             OUTMID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INMID = INTEGER (Given)
*        Input memory identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     XMIN = INTEGER (Returned)
*        Minimum X position
*     YMIN = INTEGER (Returned)
*        Minimum Y position
*     XMAX = INTEGER (Returned)
*        Maximum X position
*     YMAX = INTEGER (Returned)
*        Maximum Y position
*     OUTMID = INTEGER (Returned)
*        Output memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INMID
      INTEGER ROIID

*  Arguments Returned:
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX
      INTEGER OUTMID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                  STATUS )

*+
*  Name:
*     IXWWRI
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIRWRI.
*
*  Invocation:
*     CALL IXWWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :             STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     XMIN = INTEGER (Given)
*        Minimum X position
*     YMIN = INTEGER (Given)
*        Minimum Y position
*     XMAX = INTEGER (Given)
*        Maximum X position
*     YMAX = INTEGER (Given)
*        Maximum Y position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ROIID
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSRV( DISPID, ROIID, LVIS, STATUS )

*+
*  Name:
*     IXWSRV
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIRSRV.
*
*  Invocation:
*     CALL IXWSRV( DISPID, ROIID, LVIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER ROIID
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
     :                   EXTRN, STATUS )

*+
*  Name:
*     IXWENI
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIENI.
*
*  Invocation:
*     CALL IXWENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
*    :             EXTRN, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INTTY = INTEGER (Given)
*        Interactor type
*     INTID = INTEGER (Given)
*        Interactor identifier
*     OBJTY = INTEGER (Given)
*        Object type
*     OBJID = INTEGER (Given)
*        Object identifier
*     INTOP = INTEGER (Given)
*        Interactive operation
*     EXTRN = INTEGER (Given)
*        Exit trigger number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INTTY
      INTEGER INTID
      INTEGER OBJTY
      INTEGER OBJID
      INTEGER INTOP
      INTEGER EXTRN

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWEIW( DISPID, TRIGS, STATUS )

*+
*  Name:
*     IXWEIW
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIEIW.
*
*  Invocation:
*     CALL IXWEIW( DISPID, TRIGS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     TRIGS( * ) = LOGICAL (Returned)
*        Trigger status array
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      LOGICAL TRIGS( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSTI( DISPID, STATUS )

*+
*  Name:
*     IXWSTI
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIISTI.
*
*  Invocation:
*     CALL IXWSTI( DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )

*+
*  Name:
*     IXWQID
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIQID.
*
*  Invocation:
*     CALL IXWQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INTTY = INTEGER (Given)
*        Interactor type
*     INTID = INTEGER (Given)
*        Interactor number
*     MESSAG = CHARACTER * ( * ) (Returned)
*        Interactive device description
*     MESLEN = INTEGER (Returned)
*        Length of description string
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INTTY
      INTEGER INTID

*  Arguments Returned:
      CHARACTER * ( * ) MESSAG
      INTEGER MESLEN

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGLD( DISPID, LOCNUM, DX, DY, STATUS )

*+
*  Name:
*     IXWGLD
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIGLD.
*
*  Invocation:
*     CALL IXWGLD( DISPID, LOCNUM, DX, DY, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LOCNUM = INTEGER (Given)
*        Locator number
*     DX = INTEGER (Returned)
*        X displacement
*     DY = INTEGER (Returned)
*        Y displacement
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LOCNUM

*  Arguments Returned:
      INTEGER DX
      INTEGER DY

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGIE( DISPID, NEVAL, IVALUE, STATUS )

*+
*  Name:
*     IXWGIE
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIGIE.
*
*  Invocation:
*     CALL IXWGIE( DISPID, NEVAL, IVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     IVALUE = INTEGER (Returned)
*        Input integer value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      INTEGER IVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGRE( DISPID, NEVAL, RVALUE, STATUS )

*+
*  Name:
*     IXWGRE
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIGRE.
*
*  Invocation:
*     CALL IXWGRE( DISPID, NEVAL, RVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     RVALUE = REAL (Returned)
*        Input real value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      REAL RVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGLE( DISPID, NEVAL, LVALUE, STATUS )

*+
*  Name:
*     IXWGLE
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIGLE.
*
*  Invocation:
*     CALL IXWGLE( DISPID, NEVAL, LVALUE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     LVALUE = LOGICAL (Returned)
*        Input logical value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      LOGICAL LVALUE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGSE( DISPID, NEVAL, STRING, SLEN, STATUS )

*+
*  Name:
*     IXWGSE
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIIGSE.
*
*  Invocation:
*     CALL IXWGSE( DISPID, NEVAL, STRING, SLEN, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     STRING = CHARACTER * ( * ) (Returned)
*        Input string
*     SLEN = INTEGER (Returned)
*        Length of returned string
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      CHARACTER * ( * ) STRING
      INTEGER SLEN

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, IMAGE, STATUS )

*+
*  Name:
*     IXWSNP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDSNP.
*
*  Invocation:
*     CALL IXWSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
*    :             PACK, IMAGE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CMODE = INTEGER (Given)
*        Colour mode
*     NPIX = INTEGER (Given)
*        Number of pixels
*     XSTART = INTEGER (Given)
*        Data position in X
*     YSTART = INTEGER (Given)
*        Data position in Y
*     DEPTH = INTEGER (Given)
*        Data depth ( bits / pixel )
*     PACK = INTEGER (Given)
*        Packing factor
*     IMAGE( * ) = INTEGER (Returned)
*        Image data
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CMODE
      INTEGER NPIX
      INTEGER XSTART
      INTEGER YSTART
      INTEGER DEPTH
      INTEGER PACK

*  Arguments Returned:
      INTEGER IMAGE( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )

*+
*  Name:
*     IXWBLM
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIMBLM.
*
*  Invocation:
*     CALL IXWBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     BLINKS( * ) = REAL (Given)
*        Blink periods ( in seconds )
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      REAL BLINKS( * )

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                   YSPLIT, STATUS )

*+
*  Name:
*     IXWSSS
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDSSS.
*
*  Invocation:
*     CALL IXWSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
*    :             YSPLIT, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        Memory identifiers
*     XOFF( * ) = INTEGER (Given)
*        X offsets
*     YOFF( * ) = INTEGER (Given)
*        Y offsets
*     SPLIT = INTEGER (Given)
*        Split flag
*     XSPLIT = INTEGER (Given)
*        Split X location
*     YSPLIT = INTEGER (Given)
*        Split Y location
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER XOFF( * )
      INTEGER YOFF( * )
      INTEGER SPLIT
      INTEGER XSPLIT
      INTEGER YSPLIT

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSBV( DISPID, MEMID, LVIS, STATUS )

*+
*  Name:
*     IXWSBV
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IILSBV.
*
*  Invocation:
*     CALL IXWSBV( DISPID, MEMID, LVIS, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWIAG( DISPID, OUTID, STATUS )

*+
*  Name:
*     IXWIAG
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDIAG.
*
*  Invocation:
*     CALL IXWIAG( DISPID, OUTID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     OUTID = INTEGER (Given)
*        Output identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER OUTID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWENC( DISPID, STATUS )

*+
*  Name:
*     IXWENC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDENC.
*
*  Invocation:
*     CALL IXWENC( DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID,
     :                   STATUS )

*+
*  Name:
*     IXWAMY
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDAMY.
*
*  Invocation:
*     CALL IXWAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XSIZE = INTEGER (Given)
*        Requested X size
*     YSIZE = INTEGER (Given)
*        Requested Y size
*     MEMDEP = INTEGER (Given)
*        Requested memory depth
*     MEMTYP = INTEGER (Given)
*        Requested memory type
*     MEMID = INTEGER (Returned)
*        New memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER MEMDEP
      INTEGER MEMTYP

*  Arguments Returned:
      INTEGER MEMID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWSTC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IXWSTC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDSTC.
*
*  Invocation:
*     CALL IXWSTC( DISPID, NCONF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Returned)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWRLC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IXWRLC
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIDRLC.
*
*  Invocation:
*     CALL IXWRLC( DISPID, NCONF, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWGEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IXWGEP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIEGEP.
*
*  Invocation:
*     CALL IXWGEP( PARAM, SLEN, STRING, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name
*     SLEN = INTEGER (Given)
*        Length of output buffer
*     STRING = CHARACTER * ( * ) (Returned)
*        Buffer for string parameter
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER SLEN

*  Arguments Returned:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWPEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IXWPEP
*
*  Purpose:
*     Perform the X windows specific work for the IDI routine IIEPEP.
*
*  Invocation:
*     CALL IXWPEP( PARAM, SLEN, STRING, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name
*     SLEN = INTEGER (Given)
*        Length of input buffer
*     STRING = CHARACTER * ( * ) (Given)
*        Buffer for string parameter
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER SLEN
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IXWEBM( DISPID, BMDSCR, LDSCR, BMTYPE, XSIZE, YSIZE,
     :                   STATUS )

*+
*  Name:
*     IXWEBM
*
*  Purpose:
*     Perform the X-window specific work for the IDI routine IIMEBM.
*
*  Invocation:
*     CALL IXWEBM( DISPID, BMDSCR, LDSCR, BMTYPE, XSIZE, YSIZE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     BMDSCR = CHARACTER * ( * ) (Given)
*        Bitmap descriptor
*     LDSCR = INTEGER (Given)
*        Length of descriptor string
*     BMTYPE = CHARACTER (Given)
*        Bitmap type
*     XSIZE = INTEGER (Given and Returned)
*        Bitmap X size
*     YSIZE = INTEGER (Given and Returned)
*        Bitmap Y size
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      INTEGER DISPID
      CHARACTER * ( * ) BMDSCR
      INTEGER LDSCR
      CHARACTER BMTYPE

*  Arguments Given and Returned:
      INTEGER XSIZE
      INTEGER YSIZE

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

