#include <stdio.h>
#include <ctype.h>

/*
** This function loads lines of text from a file into a text widget.
**
** The name of the widget is zWidget.  "in" is a FILE pointer for the
** file.  *pLine is the integer number of the next line to be read from
** the file.  target is the number of lines we want to be loaded.
** zTag is the name of a tag to be associated with the text loaded into
** the text widget.
*/
static void 
load_to_line(char *zWidget, FILE *in, int *pLine, int target, char *zTag){
  char zLine[1000];
  while( *pLine<target && fgets(zLine,sizeof(zLine),in)!=0 ){
    ET( %s(zWidget) insert end "%q(zLine)" {%s(zTag)} );
    ++*pLine;
  }
}

/* The function loads a number of blank lines into the given text
** widget.
*/
static void blank_lines(char *zWidget, int nLine){
  int i;
  char zText[1000];

  while( nLine ){
    for(i=0; i<nLine && i<sizeof(zText)-1; i++) zText[i] = '\n';
    zText[i] = 0;
    nLine -= i;
    ET( %s(zWidget) insert end "%q(zText)" {blnk} );
  }
}

/* Two files have been opened by "in1" and "in2".  "pipe" is the output
** of a "diff" command on these two files.  The job of this function is
** to load the text of the two files into the two text widges of the
** tkdiff display.
*/
static void process_files(FILE *in1, FILE *in2, FILE *pipe){
  int line1 = 1;
  int line2 = 1;
  int a,b,c,d;
  char zLine[1000];

  while( fgets(zLine,sizeof(zLine),pipe) ){
    if( !isdigit(zLine[0]) ) continue;

    /* Case 1.  Text inserted into the right file. 
    ** 
    ** "XaY" means that a single line is insert on the right.  The
    ** line number of the inserted line is Y.  It appears right after
    ** line X on the left.
    **
    ** "XaY,Z" means that multiple lines (numbered Y thru Z) are inserted.
    ** They occur right after line X on the left.
    */
    if( 
      (c=0, sscanf(zLine,"%da%d,%d",&a,&b,&c))>=2 
    ){
      if( c==0 ) c = b;
      load_to_line(".left",in1,&line1,a+1,"");
      blank_lines(".left",c-b+1);
      load_to_line(".right",in2,&line2,b,"");
      load_to_line(".right",in2,&line2,c+1,"ins");
    }else 

    /* Case 2.   Text deleted from the right file.
    ** 
    ** "XdY" means a single line is delete after line Y on the right.
    ** Line X on the left is the line that was deleted.
    **
    ** "X,YdZ" means that multiple lines were deleted after line Z on the
    ** right.  Lines X thru Y on the left are the lines that were deleted.
    */
    if( 
      (c=0, sscanf(zLine,"%dd%d",&b,&a)==2) || 
      sscanf(zLine,"%d,%dd%d",&b,&c,&a)==3 
    ){
      if( c==0 ) c = b;
      load_to_line(".right",in2,&line2,a+1,"");
      blank_lines(".right",c-b+1);
      load_to_line(".left",in1,&line1,b,"");
      load_to_line(".left",in1,&line1,c+1,"del");
    }else 

    /* Case 3.  A difference between left and right.
    **
    ** "U,VcX,Y" means that lines U thru V on the left are changed into
    ** lines X thru Y on the right.
    */
    if(
      (d=0, sscanf(zLine,"%d,%dc%d,%d",&a,&b,&c,&d))>=3 ||
      (b=d=0, sscanf(zLine,"%dc%d,%d",&a,&c,&d)==3)
    ){
      int nLeft, nRight;

      if( b==0 ) b = a;
      if( d==0 ) d = c;
      nLeft = b - a + 1;
      nRight = d - c + 1;
      if( nLeft == nRight ){
        load_to_line(".left",in1,&line1,a,"");
        load_to_line(".left",in1,&line1,b+1,"diff");
        load_to_line(".right",in2,&line2,c,"");
        load_to_line(".right",in2,&line2,d+1,"diff");
      }else if( nLeft > nRight ){
        load_to_line(".left",in1,&line1,a,"");
        load_to_line(".left",in1,&line1,a+nRight,"diff");
        load_to_line(".left",in1,&line1,b+1,"del");
        load_to_line(".right",in2,&line2,c,"");
        load_to_line(".right",in2,&line2,d+1,"diff");
        blank_lines(".right",nLeft - nRight);
      }else /* if( nRight > nLeft ) */ {
        load_to_line(".left",in1,&line1,a,"");
        load_to_line(".left",in1,&line1,b+1,"diff");
        blank_lines(".left",nRight - nLeft);
        load_to_line(".right",in2,&line2,c,"");
        load_to_line(".right",in2,&line2,c+nLeft,"diff");
        load_to_line(".right",in2,&line2,d+1,"ins");
      }
    }
  }
  load_to_line(".left",in1,&line1,10000000,"");
  load_to_line(".right",in2,&line2,10000000,"");
}

/* The main procedure */
void main(int argc, char **argv){
  char *zFile1 = 0;         /* For reading the left file */
  char *zFile2 = 0;         /* For reading the right file */
  char *flagB = "";         /* The "-b" command line option */
  char *flagI = "";         /* The "-i" command line option */
  int end_of_options = 0;   /* True if the "--" flag has been seen */
  int i;                    /* Loop index */
  FILE *in1, *in2;          /* The left and right files, respectively */
  FILE *pipe;               /* Output of the diff command */

  /* Start ET */
  Et_Init(&argc,argv);

  /* Process the command-line arguments */
  for(i=1; i<argc; i++){
    if( strcmp(argv[i],"-b")==0 && !end_of_options ){
      flagB = "-b";
    }else if( strcmp(argv[i],"-i")==0 && !end_of_options ){
      flagI = "-i";
    }else if( strcmp(argv[i],"--")==0 ){
      end_of_options = 1;
    }else if( zFile1==0 ){
      zFile1 = argv[i];
    }else if( zFile2==0 ){
      zFile2 = argv[i];
    }else{
      fprintf(stderr,
        "%s: Too many file arguments.  Can only diff two files at a time.\n",
        *argv);
      exit(1);
    }
  }
  if( zFile1==0 || zFile2==0 ){
    fprintf(stderr,
      "%s: Requires names of two files to be compared.\n",argv[0]);
    exit(1);
  }

  /* Open the left and right files 
  */
  ET( set File1 "%q(zFile1)"; set File2 "%q(zFile2)" );
  in1 = fopen(zFile1,"r");
  if( in1==0 ){
    perror( ET_STR( return "Can't open $File1" ) );
    exit(1);
  }
  in2 = fopen(zFile2,"r");
  if( in2==0 ){
    perror( ET_STR( return "Can't open $File2" ) );
    exit(1);
  }

  /* Start the diff command 
  */
  pipe = popen(ET_STR( return "diff %s(flagB) %s(flagI) $File1 $File2"), "r");
  if( pipe==0 ){
    perror( ET_STR( return "Can't exec \"diff $File1 $File2\"" ) );
    exit(1);
  }

  /* Build the display */
  ET_INCLUDE( tkdiff.tcl );

  /* Process the output of the diff command */
  process_files(in1, in2, pipe);
  fclose(in1);
  fclose(in2);
  pclose(pipe);

  /* Disable the text widgets so that they will accept no input, then
  ** start the event loop */
  ET( 
    .left config -state disabled; 
    .right config -state disabled 
  );
  Et_MainLoop();
}
