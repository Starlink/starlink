.\" @(#)get_aline.l 1.0 95/01/22 SMI
.TH get_aline l "24 Jan 1995"
.SH NAME
get_aline \- add line-editing and history to C or F77 code
.SH SYNOPSIS

.B C main:
.BI "int get_aline_(char *" "line", 
.BI "char *" "prompt", 
.BI "int *" "term", 
.br
.BI "                       int " "llen" , 
.BI "int " "plen" );

.B F77 programme:

      character line*#, prompt*#
.br
      integer   term
.br
      integer   get_aline
.br
      external  get_aline
.br

.B "     "int get_aline( 
.I line, 
.I prompt, 
.I term )
.SH DESCRIPTION
.B "Get_aline"
provides an easy interface to the GNU
.B READLINE
library which enables 
.IR "command-line editing", 
.IR "history", 
.IR "tilde expansion", 
and
.IR "file-completion"
(TAB) for any user-built utility. 
.B "Get_aline"
can be called in place of a 'scanf' (C)  or a 'read' 
(FORTRAN).  
The
.IR prompt
is presented to the user and the reply is put
into the array pointed to by
.IR line.
The input line is scanned until a newline character is read or an
end-of-file condition is encountered.  The newline character is
omitted from 
.IR line.
In case 
.IR term
is 0, the line is padded with blanks (FORTRAN style) else
the line is terminated with a null character (C style).
When called from a C main
.IR llen,
the declaration length of 
.IR line,
and
.IR plen,
the declaration length of 
.IR prompt
need to be provided as well. The F77 compiler adds these automatically.
Note the trailing
.IR underscore
in the C call.
.SH "RETURN VALUES"
.B "Get_aline"
returns the number of characters read into
.IR line.
If the user replies with just a return or end-of-file is encountered
and no characters have been read, 0 is returned. In case the
user input exceeds 
.IR
llen,
the input is truncated and a value of -1 is returned.
.SH COMPILATION AND LINKING
.B "Get_aline"
can be linked with your code by adding
.IR \`get-aline_link\`
to the compile command. For instance:

        cc mycode.c `get_aline_link` -o mycode

If you get an error 'Command not found' for
.IR Get_aline_link
you have to add the absolute path to the file: /usr/local/progs/bin.
Simply type 
.B "Get_aline"_link
if you want to know what is being added upon compilation.

The source and object file for 
.B "Get_aline"
are located in /usr/local/progs/get_aline. The following command
was used to compile the code for Solaris 2.x:

        cc -c -DNEED_RINDEX get_aline.c

For Sunos4.x you may have to recompile it without the 
.IR "-DNEED_RINDEX"
to avoid multiple declarations of 'index' and 'rindex'. For sites
which do not have READLINE installed, the switch 
.IR "-DNO_READLINE"
can be specified, in which case the standard I/O function
.IR "fgets"
will be used to get the line from the terminal. Clearly, in this
case you will not benefit from the special READLINE functionality,
but at least the code will still run.
.SH EXAMPLES
.I "C example:"

#include <stdio.h>
#define  SLEN   81

main()
{
  char line[SLEN], prompt[SLEN];
  int  llen;
  static int  itrm = 1;

  llen = get_aline_(line, "Hello World (over): ", 
         &itrm, SLEN, 20);

  if (llen == 0)
    printf("....Nobody home I guess.\\n");
  else
   {
     printf("World replied:\\n");
     printf("'%s'\\n",line);
     if (llen == -1)
       printf("....*choke* remainder truncated\\n");
   }

  printf("\\nTry history (up-arrow) and some editing 
         next!\\n");
  sprintf(prompt,"Hello Moon too (over): ");

  llen = get_aline_(line, prompt, &itrm, SLEN, 
         strlen(prompt));

  if (llen == 0)
    printf("....Still a dusty and lonely place.\\n");
  else
    {
      printf("Moon replied:\\n");
      printf("'%s'\\n",line);
      if (llen == -1)
         printf("....*choke* remainder truncated\\n");
    }
  printf("\\n");
}

.I "F77 example:"

        program hello

        character  line*132, prompt*80
        integer    llen, itrm

        integer    get_aline
        external   get_aline
  
        itrm = 0

        llen = get_aline(line, 'Hello World (over): ', itrm)

        if (llen .eq. 0) then
          write(6,'(" ....Nobody home I guess.")')
        else
          write(6,'(" World replied:")')
          write(6,'(" -",A,"-")') line(1:llen)
          if (llen .eq. -1)
    &       write(6,'(" ....*choke* remainder truncated.")')
        endif

        write(6,'(/,
    &    "Try history (up-arrow) and some editing next!")')
        write(6,'(" ")')
        write(prompt,'("Hello Moon too (over): ",\$)')

        llen = get_aline(line, prompt, itrm)

        if (llen .eq. 0) then
          write(6,'(
     &      " ....Still a dusty and lonely place.")')
        else
          write(6,'(" Moon replied:")')
        write(6,'(" -",A,"-")') line(1:llen)
          if (llen .eq. -1)
            write(6,'(
     &        " ....*choke* remainder truncated.")')
        endif
        write(6,'(" ")')

        stop
        end

.SH SEE ALSO
.BR "/usr/local/progs/get_aline (i.e. csample, fsample, readline)"
.BR gets (3S)
.SH AUTHOR
Remo Tilanus -- UK/Netherlands/Canada Joint Astronomy Centre
.br
                Hilo, Hawaii
.\"	@(#)get_aline.l		1.0 of 01/22/1995
.\".Ee
