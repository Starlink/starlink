#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <dirent.h>

#include "sae_par.h"
#include "sock_err.h"

#include "sock.h"

/* int getpeername ( int s, struct sockaddr *name, int *namelen );
struct hostent *gethostbyname ( const char *name );
struct hostent *gethostbyaddr ( const char *addr, int len, int type );
*/


void sock_accept
(
int listen_socket,     /* Socket for connections (given) */
int *new_socket,       /* Socket for new connection (returned) */
int *status            /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_ACCEPT

*  Purpose:
*     Accept a network call

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated accepts of a call to allow for signal interrupts.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1994 (REVAD::BDK):
*        Original
*     2012-02-13 (TIMJ):
*        Fix type of namelen
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   for ( ; ; )
   {
      struct sockaddr_in client_address; /* incoming connection details */
      socklen_t namelen = sizeof ( client_address ); /* size of address */
      *new_socket = accept ( listen_socket,
        ( struct sockaddr * ) &client_address, &namelen );

      if ( *new_socket != -1 )
      {
         break;
      }
      else if ( errno == EINTR )
      {
/*   Call was interrupted by a signal, keep going */
      }
      else
      {
         *status = SOCK__REJECT;
         break;
      }
   }
}


void sock_connect
(
int sock,                             /* Socket for connection (given) */
const struct sockaddr *connect_addr,  /* socket info */
int *status                           /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_CONNECT

*  Purpose:
*     Make a connection

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated connection attempts to allow for signal interrupts.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int istat;            /* local status */

   if ( *status != SAI__OK ) return;

   for ( ; ; )
   {
      istat = connect ( sock, connect_addr, sizeof ( *connect_addr ) );

      if ( istat != -1 )
      {
         break;
      }
      else if ( errno == EINTR )
      {
/*   Call was interrupted by a signal, keep going */
      }
      else
      {
         *status = SOCK__REJECT;
         break;
      }
   }
}



void sock_ghba
(
struct sockaddr_in peer,   /* structure for peer details (given) */
struct hostent *peeraddr,  /* structure for peer entry (returned) */
int *status                /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_GHBA

*  Purpose:
*     Get host detail by address

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated gethostbyaddr to allow for signal interrupts.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int addrlen;            /* size of peer data */
   struct hostent *answer; /* pointer to peer entry */

   if ( *status != SAI__OK ) return;

   addrlen = sizeof ( struct in_addr );
   for ( ; ; )
   {
      answer = gethostbyaddr ( (char *)&(peer.sin_addr), addrlen, AF_INET );

      if ( answer != 0 )
      {
         *peeraddr = *answer;
         break;
      }
      else if ( h_errno == HOST_NOT_FOUND)
      {
         printf ( "sock_ghba: host not found\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == TRY_AGAIN )
      {
         printf ( "sock_ghba: try again\n" );
      }
      else if ( h_errno == NO_RECOVERY )
      {
         printf ( "sock_ghba: no recovery\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == NO_DATA )
      {
         printf ( "sock_ghba: no data\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == NO_ADDRESS )
      {
         printf ( "sock_ghba: no address\n" );
         *status = SOCK__REJECT;
         break;
      }
      else
      {
         *status = SOCK__REJECT;
         break;
      }
   }

}



void sock_ghbn
(
char *rmach,                /* name of remote machine (given) */
struct hostent *retentptr,  /* network data structure for other
                               machine (returned) */
int *status                 /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_GHBN

*  Purpose:
*     Get host details by name

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated gethostbyname to allow for signal interrupts.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct hostent *hostentptr;       /* pointer to network data structure
                                        for other machine */

   if ( *status != SAI__OK ) return;

   for ( ; ; )
   {
      hostentptr = gethostbyname ( rmach );

      if ( hostentptr != 0 )
      {
         *retentptr = *hostentptr;
         break;
      }
      else if ( h_errno == HOST_NOT_FOUND)
      {
         printf ( "sock_ghbn: host not found\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == TRY_AGAIN )
      {
         printf ( "sock_ghbn: try again\n" );
      }
      else if ( h_errno == NO_RECOVERY )
      {
         printf ( "sock_ghbn: no recovery\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == NO_DATA )
      {
         printf ( "sock_ghbn: no data\n" );
         *status = SOCK__REJECT;
         break;
      }
      else if ( h_errno == NO_ADDRESS )
      {
         printf ( "sock_ghbn: no address\n" );
         *status = SOCK__REJECT;
         break;
      }
      else
      {
         *status = SOCK__REJECT;
         break;
      }
   }

}



void sock_gpn
(
int channel,           /* i/o channel for communications, bound to a socket
                          (given) */
struct sockaddr_in *peer, /* structure for peer details (returned) */
int *status            /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_GPN

*  Purpose:
*     Get peer details

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated getpeername to allow for signal interrupts.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1994 (REVAD::BDK):
*        Original
*     2012-02-13 (TIMJ):
*        Fix type of namelen
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int istat;            /* local status */
 
   if ( *status != SAI__OK ) return;

   for ( ; ; )
   {
      socklen_t addrlen = sizeof ( struct sockaddr_in ); /* size of structure */
      istat = getpeername ( channel, (struct sockaddr *)peer, &addrlen );

      if ( istat != -1 )
      {
         break;
      }
      else if ( errno == EINTR )
      {
/*   Call was interrupted by a signal, keep going */
      }
      else
      {
         *status = SOCK__REJECT;
         break;
      }
   }

}



void sock_read
(
int read_socket,       /* Socket on which to read the data (given) */
int length_required,   /* Length of the message required (given) */
char *buffer,          /* Buffer to receive the data (returned) */
int *status            /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_READ

*  Purpose:
*     Read a complete message from a socket

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated reads on a socket in order to completely input a
*      block of data of a specified size

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*      Bernard V. McNally (bmc@roe.ac.uk)
*     {enter_new_authors_here}

*  History:
*     1993-03-08 : Original version (bmc@roe.ac.uk)
*     09-JUN-1993 (REVAD::BDK):
*        Adapted from GATEWAY for tranchan
*     16-MAR-1994 (BDK):
*        Adapted for msp
*     12-APR-1994 (BDK):
*        Make the function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int length;            /* Length of a message segment */
   int length_received;   /* Cumulative length of message segments
                             received */


   if ( *status != SAI__OK ) return;

/*   Obtain a message */

   length_received = 0;

   while ( length_received < length_required )

   {
      length = read ( read_socket, (char *) (buffer+length_received),
        length_required-length_received );
      if ( length > 0 )
      {
         length_received += length;
      }
      else if ( length == 0 )
      {
         *status = SOCK__READSOCK;
         break;
      }
      else if ( errno == EINTR )
      {
/*   Some i/o signalled or a timer, keep going */
      }
      else
      {
         *status = SOCK__READSOCK;
         break;
      }
   }

}



void sock_write
(
int write_socket,     /* Socket on which to write the data (given) */
int length_to_send,   /* Length of the message to be sent (given) */
const char *buffer,   /* Buffer containing the data (given) */
int *status           /* global status (given and returned) */
)

/*
*+
*  Name:
*     SOCK_WRITE

*  Purpose:
*     Write a complete message to a socket

*  Language:
*     Starlink C

*  Algorithm:
*      Perform repeated writes on a socket in order to completely output
*      a block of data of a specified size

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*      Bernard V. McNally (bmc@roe.ac.uk)
*     {enter_new_authors_here}

*  History:
*     1993-03-08 : Original version (bmc@roe.ac.uk)
*     09-JUN-1993 (REVAD::BDK):
*        Adapted from GATEWAY for tranchan
*     16-MAR-1994 (BDK):
*        Adapted for msp
*     12-APR-1994 (BDK):
*        Make the function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int length;        /* Length of a message segment */
   int length_sent;   /* Cumulative length of message segments sent */


   if ( *status != SAI__OK ) return;

/*   Send a message */

   length_sent = 0;
   while ( length_sent < length_to_send )
   {
      length = write ( write_socket, (char *)(buffer + length_sent),
        length_to_send-length_sent );
      if ( length > 0 )
      {
         length_sent += length;
      }
      else if ( length == 0 )
      {
         *status = SOCK__WRITESOCK;
         break;
      }
      else if ( errno == EINTR )
      {
/*   Probably a timer, keep going */
      }
      else
      {
         *status = SOCK__WRITESOCK;
         break;
      }
   }

}
