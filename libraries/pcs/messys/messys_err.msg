.TITLE		MESSYS		ADAM message system
.FACILITY	MESSYS,110/PREFIX=MESSYS__
.IDENT		'Version 0.00'
!author		REVA:ADAM 21Jun84 (John Cooke)
!modified       REVA:ADAM 25Feb85 (Charlie Richardson)
!modified       REVA:ADAM 9Apr85 (Charlie Richardson)
!modified	REVAD::BDK 28.08.1985 add NOTASK and TOOLONG
!modified       REVAD::BDK 11.11.1985 add SYNC
!modified       REVAD::BDK 30.05.1986 add NOMSG, IVACKINIT
!modified       REVAD::BDK 07.08.1986 add NONODE
!modified       REVAD::BDK 15.04.1988 add NONET, TOOMACH
!modified       REVAD::BDK 10.05.1988 add NETTIME, IVNETCALL, BADMESS, 
!                NOMESS, TRANSFAIL
!modified       REVAD::BDK 11.05.1988 add IVCOMMSG
!modified	AAOEPP::WFL 1.05.1989 add TRIGGER
!modified	REVAD::BDK 23.05.1991 add KICK
!modified       RLVAD::AJC 31.05.1991 correct spelling
!modified       RLVAD::AJC 25.11.1991 add SYNCREP
!modified       RLVAD::AJC 10.10.1994 add BUFOV

.SEVERITY	INFORMATIONAL
.BASE 100

PATHOPEN	<path already established>
MESNUM		<maximum message number reached>
NOTFOUND	<path not found to that task in table lookup>
NOPRIV		<no privilege for establishing path>
TIMEOUT		<timeout occurred in messys_receive>
RESCHED		<reschedule occurred in messys_receive>
ASTINT		<AST interrupt occurred in messys_receive>
EXTINT		<external interrupt occurred in messys_receive>
PARAMREQ	<request to user interface for parameter>
PARAMREP	<reply from user interface giving parameter>
INFORM		<message sent to user interface for display>
LISTUNDER       <message list depth less than zero, list corrupted>
LISTEMPTY       <message list depth and contents in disagreement:-corruption>
BADPATH         <path outside range of possible path values>
NOTASK		<failed to open path to named task>
TOOLONG		<task name too long>
SYNC		<synchronisation message - this should never be reported>
NOMSG		<message not found in list - should never be reported>
IVACKINIT	<invalid ACK_INIT message received>
NONODE		<failed to translate SYS$NODE>
NONET		<ADAMNET process not available>
TOOMACH		<attempt to open too many network links>
NETTIME		<timeout on attempted network call>
IVNETCALL	<invalid response received to network call>
BADMESS		<messid outside range of possible messid values>
NOMESS		<messid value is not valid>
TRANSFAIL	<other task unable to start transaction>
IVCOMMSG	<invalid type of message received on command_q>
TRIGGER		<message sent to trigger originating task>
KICK		<kick message detected by MESSYS_RECEIVE>
SYNCREP         <reply to SYNC message>

.SEVERITY	ERROR
.BASE 200

COMFULL		<cannot open another path, common block full>
NONEXIST	<non-existent path pointer>
MSGFUNC		<error in message function code>
INTFLG		<error in setting interrupt flag>
FLAGS		<unknown flag set>
BUFOV		<message buffer overflow>

.SEVERITY       FATAL
.BASE 300

NOLISPACE       <no space left on messys message list, message lost>

.END
