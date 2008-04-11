#include <stdio.h>
#include <sys/utsname.h>

main(){
    struct utsname buff;

    uname( &buff );

    printf("\n\nThis is %s (%s) running %s %s\n\n\n", buff.nodename,
						  buff.machine,
						  buff.sysname,
						  buff.release );
}
