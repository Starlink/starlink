************************************************************************

      SUBROUTINE IKNOPN( DEVNAM, DISPID, STATUS )

*+
*  Name:
*     IKNOPN
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDOPN.
*
*  Invocation:
*     CALL IKNOPN( DEVNAM, DISPID, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DEVNAM = CHARACTER * ( * ) (Given)
*        Name of device to open
*     DISPID = INTEGER (Returned)
*        Identifier for display
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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

*  Arguments Returned:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

************************************************************************

      SUBROUTINE IKNCLO( DISPID, STATUS )

*+
*  Name:
*     IKNCLO
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDCLO.
*
*  Invocation:
*     CALL IKNCLO( DISPID, STATUS )
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

      SUBROUTINE IKNRST( DISPID, STATUS )

*+
*  Name:
*     IKNRST
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDRST.
*
*  Invocation:
*     CALL IKNRST( DISPID, STATUS )
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

      SUBROUTINE IKNUPD( DISPID, STATUS )

*+
*  Name:
*     IKNUPD
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDUPD.
*
*  Invocation:
*     CALL IKNUPD( DISPID, STATUS )
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

      SUBROUTINE IKNERR( STATUS, MESSAG, MESLEN )

*+
*  Name:
*     IKNERR
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDERR.
*
*  Invocation:
*     CALL IKNERR( STATUS, MESSAG, MESLEN )
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

      SUBROUTINE IKNQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
     :                   NITT, NCURS, STATUS )

*+
*  Name:
*     IKNQDV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDQDV.
*
*  Invocation:
*     CALL IKNQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
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

      SUBROUTINE IKNQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IKNQCI
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDQCI.
*
*  Invocation:
*     CALL IKNQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
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

      SUBROUTINE IKNQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IKNQCr
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDQCr.
*
*  Invocation:
*     CALL IKNQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
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

      SUBROUTINE IKNQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON,
     :                   MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP,
     :                   NMEM, STATUS )

*+
*  Name:
*     IKNQDC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDQDC.
*
*  Invocation:
*     CALL IKNQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON, MEMID,
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

      SUBROUTINE IKNSEL( DISPID, NCONF, STATUS )

*+
*  Name:
*     IKNSEL
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSEL.
*
*  Invocation:
*     CALL IKNSEL( DISPID, NCONF, STATUS )
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

      SUBROUTINE IKNSMV( DISPID, MEMID, NMEM, LVIS, STATUS )

*+
*  Name:
*     IKNSMV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMSMV.
*
*  Invocation:
*     CALL IKNSMV( DISPID, MEMID, NMEM, LVIS, STATUS )
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

      SUBROUTINE IKNWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )

*+
*  Name:
*     IKNWSC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIZWSC.
*
*  Invocation:
*     CALL IKNWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )
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

      SUBROUTINE IKNWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )

*+
*  Name:
*     IKNWZM
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIZWZM.
*
*  Invocation:
*     CALL IKNWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )
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

      SUBROUTINE IKNRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IKNRSZ
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIZRSZ.
*
*  Invocation:
*     CALL IKNRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )
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

      SUBROUTINE IKNSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )

*+
*  Name:
*     IKNSLT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMSLT.
*
*  Invocation:
*     CALL IKNSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )
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

      SUBROUTINE IKNSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )

*+
*  Name:
*     IKNSDP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSDP.
*
*  Invocation:
*     CALL IKNSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )
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

      SUBROUTINE IKNWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                   XSTART, YSTART, STATUS )

*+
*  Name:
*     IKNWMY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMWMY.
*
*  Invocation:
*     CALL IKNWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
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

      SUBROUTINE IKNCMY( DISPID, MEMID, NMEM, BACK, STATUS )

*+
*  Name:
*     IKNCMY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMCMY.
*
*  Invocation:
*     CALL IKNCMY( DISPID, MEMID, NMEM, BACK, STATUS )
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

      SUBROUTINE IKNRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, ITTON, IMAGE, STATUS )

*+
*  Name:
*     IKNRMY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMRMY.
*
*  Invocation:
*     CALL IKNRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
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

      SUBROUTINE IKNSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
     :                   XOFF, YOFF, STATUS )

*+
*  Name:
*     IKNSTW
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMSTW.
*
*  Invocation:
*     CALL IKNSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
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

      SUBROUTINE IKNPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                   STATUS )

*+
*  Name:
*     IKNPLY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIGPLY.
*
*  Invocation:
*     CALL IKNPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE, STATUS )
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

      SUBROUTINE IKNTXT( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH,
     :                   TANGLE, COLOR, TSIZE, STATUS )

*+
*  Name:
*     IKNTXT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIGTXT.
*
*  Invocation:
*     CALL IKNTXT( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH,
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

      SUBROUTINE IKNWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IKNWIT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILWIT.
*
*  Invocation:
*     CALL IKNWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
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

      SUBROUTINE IKNRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IKNRIT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILRIT.
*
*  Invocation:
*     CALL IKNRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
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

      SUBROUTINE IKNWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IKNWLT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILWLT.
*
*  Invocation:
*     CALL IKNWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
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

      SUBROUTINE IKNRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IKNRLT
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILRLT.
*
*  Invocation:
*     CALL IKNRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
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

      SUBROUTINE IKNWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IKNWZP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIZWZP.
*
*  Invocation:
*     CALL IKNWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
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

      SUBROUTINE IKNRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IKNRZP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIZRZP.
*
*  Invocation:
*     CALL IKNRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
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

      SUBROUTINE IKNINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR,
     :                   XC, YC, STATUS )

*+
*  Name:
*     IKNINC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IICINC.
*
*  Invocation:
*     CALL IKNINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC, YC, STATUS )
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

      SUBROUTINE IKNSCV( DISPID, NUMCUR, LVIS, STATUS )

*+
*  Name:
*     IKNSCV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IICSCV.
*
*  Invocation:
*     CALL IKNSCV( DISPID, NUMCUR, LVIS, STATUS )
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

      SUBROUTINE IKNRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID,
     :                   STATUS )

*+
*  Name:
*     IKNRCP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IICRCP.
*
*  Invocation:
*     CALL IKNRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID, STATUS )
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

      SUBROUTINE IKNWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )

*+
*  Name:
*     IKNWCP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IICWCP.
*
*  Invocation:
*     CALL IKNWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )
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

      SUBROUTINE IKNINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
     :                   ROIID, STATUS )

*+
*  Name:
*     IKNINR
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIRINR.
*
*  Invocation:
*     CALL IKNINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
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

      SUBROUTINE IKNRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                   OUTMID, STATUS )

*+
*  Name:
*     IKNRRI
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIRRRI.
*
*  Invocation:
*     CALL IKNRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
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

      SUBROUTINE IKNWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                  STATUS )

*+
*  Name:
*     IKNWRI
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIRWRI.
*
*  Invocation:
*     CALL IKNWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
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

      SUBROUTINE IKNSRV( DISPID, ROIID, LVIS, STATUS )

*+
*  Name:
*     IKNSRV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIRSRV.
*
*  Invocation:
*     CALL IKNSRV( DISPID, ROIID, LVIS, STATUS )
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

      SUBROUTINE IKNENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
     :                   EXTRN, STATUS )

*+
*  Name:
*     IKNENI
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIENI.
*
*  Invocation:
*     CALL IKNENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
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

      SUBROUTINE IKNEIW( DISPID, TRIGS, STATUS )

*+
*  Name:
*     IKNEIW
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIEIW.
*
*  Invocation:
*     CALL IKNEIW( DISPID, TRIGS, STATUS )
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

      SUBROUTINE IKNSTI( DISPID, STATUS )

*+
*  Name:
*     IKNSTI
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIISTI.
*
*  Invocation:
*     CALL IKNSTI( DISPID, STATUS )
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

      SUBROUTINE IKNQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )

*+
*  Name:
*     IKNQID
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIQID.
*
*  Invocation:
*     CALL IKNQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )
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

      SUBROUTINE IKNGLD( DISPID, LOCNUM, DX, DY, STATUS )

*+
*  Name:
*     IKNGLD
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGLD.
*
*  Invocation:
*     CALL IKNGLD( DISPID, LOCNUM, DX, DY, STATUS )
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

      SUBROUTINE IKNGIE( DISPID, NEVAL, IVALUE, STATUS )

*+
*  Name:
*     IKNGIE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGIE.
*
*  Invocation:
*     CALL IKNGIE( DISPID, NEVAL, IVALUE, STATUS )
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

      SUBROUTINE IKNGRE( DISPID, NEVAL, RVALUE, STATUS )

*+
*  Name:
*     IKNGRE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGRE.
*
*  Invocation:
*     CALL IKNGRE( DISPID, NEVAL, RVALUE, STATUS )
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

      SUBROUTINE IKNGLE( DISPID, NEVAL, LVALUE, STATUS )

*+
*  Name:
*     IKNGLE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGLE.
*
*  Invocation:
*     CALL IKNGLE( DISPID, NEVAL, LVALUE, STATUS )
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

      SUBROUTINE IKNGSE( DISPID, NEVAL, STRING, SLEN, STATUS )

*+
*  Name:
*     IKNGSE
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIIGSE.
*
*  Invocation:
*     CALL IKNGSE( DISPID, NEVAL, STRING, SLEN, STATUS )
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

      SUBROUTINE IKNSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, IMAGE, STATUS )

*+
*  Name:
*     IKNSNP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSNP.
*
*  Invocation:
*     CALL IKNSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
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

      SUBROUTINE IKNBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )

*+
*  Name:
*     IKNBLM
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMBLM.
*
*  Invocation:
*     CALL IKNBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )
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

      SUBROUTINE IKNSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                   YSPLIT, STATUS )

*+
*  Name:
*     IKNSSS
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSSS.
*
*  Invocation:
*     CALL IKNSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
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

      SUBROUTINE IKNSBV( DISPID, MEMID, LVIS, STATUS )

*+
*  Name:
*     IKNSBV
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IILSBV.
*
*  Invocation:
*     CALL IKNSBV( DISPID, MEMID, LVIS, STATUS )
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

      SUBROUTINE IKNIAG( DISPID, OUTID, STATUS )

*+
*  Name:
*     IKNIAG
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDIAG.
*
*  Invocation:
*     CALL IKNIAG( DISPID, OUTID, STATUS )
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

      SUBROUTINE IKNENC( DISPID, STATUS )

*+
*  Name:
*     IKNENC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDENC.
*
*  Invocation:
*     CALL IKNENC( DISPID, STATUS )
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

      SUBROUTINE IKNAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID,
     :                   STATUS )

*+
*  Name:
*     IKNAMY
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDAMY.
*
*  Invocation:
*     CALL IKNAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID, STATUS )
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

      SUBROUTINE IKNSTC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IKNSTC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDSTC.
*
*  Invocation:
*     CALL IKNSTC( DISPID, NCONF, STATUS )
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

      SUBROUTINE IKNRLC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IKNRLC
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIDRLC.
*
*  Invocation:
*     CALL IKNRLC( DISPID, NCONF, STATUS )
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

      SUBROUTINE IKNGEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IKNGEP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIEGEP.
*
*  Invocation:
*     CALL IKNGEP( PARAM, SLEN, STRING, STATUS )
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

      SUBROUTINE IKNPEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IKNPEP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIEPEP.
*
*  Invocation:
*     CALL IKNPEP( PARAM, SLEN, STRING, STATUS )
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

      SUBROUTINE IKNEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )

*+
*  Name:
*     IKNEBM
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIMEBM.
*
*  Invocation:
*     CALL IKNEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     BMDSCR = CHARACTER * ( * ) (Given)
*        Bitmap descriptor
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

