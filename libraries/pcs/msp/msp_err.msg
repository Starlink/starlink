.TITLE       MSP    Message system primitives
!Author:
!  AJC: A.J.Chipperfield (STARLINK)
!
!History:
!  10.10.1994 (AJC):
!     Original version
!  15.05.1995 (AJC):
!     Added MSP__ADMUS
!

.FACILITY     MSP,1000/PREFIX=MSP__

.SEVERITY     ERROR

BADCONNECT    <failed to 'accept' a call>
BADQUEUE      <attempt to use an invalid queue id>
FULL          <MSP tables full>
LOCFAIL       <failed to create a socket pair>
NONE          <no message found>
NOTFOUND      <rendezvous file not found>
QUENOTACT     <queue not active>
RECLEN        <message too long>
SENDLEN       <attempt to send too long a message>
SOCKFAIL      <specified socket is not on the active list>
SOCKINIT      <failed to initialize socket connection>
ADMUS         <failed creating ADAM_USER directory>

.END
