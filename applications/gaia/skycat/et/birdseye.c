/*
** A program to give a bird's-eye view of a source code file.
*/
#include <stdio.h>
#include <ctype.h>

void ProcessFile(FILE *in, const char *zWidget){
  int lineNo = 0;
  int maxWidth = 80;
  int nCanvas = 1;
  char zLine[1000];

  ET( set c [canvas %s(zWidget).c -bg black] );
  while( fgets(zLine,sizeof(zLine),in) ){
    int start;
    int column;
    int i;
    lineNo++;
    if( lineNo>500 ){
      ET( $c config -width %d(maxWidth/2) -height %d(lineNo) );
      ET( pack $c -side left -anchor n -padx 1 );
      lineNo = 1;
      maxWidth = 80;
      nCanvas++;
      ET( set c [canvas %s(zWidget).c%d(nCanvas) -bg black] );
    }
    start = 0;
    i = 0;
    column = 0;
    while( zLine[i] ){
      while( isspace(zLine[i]) ){
        if( zLine[i]=='\t' ) column = (column+8)&~7;
        else column++;
        i++;
      }
      start = column;
      while( 
        (zLine[i] && !isspace(zLine[i])) || 
        (zLine[i+1] && !isspace(zLine[i+1]))  ||
        (zLine[i+2] && !isspace(zLine[i+2]))
      ){
        if( zLine[i]=='\t' ) column = (column+8)&~7;
        if( zLine[i]==0 ) break;
        else column++;
        i++;
      }
      ET( $c create line %d(start/2) %d(lineNo) %d(column/2) %d(lineNo) \
            -fill red );
      /* if( column > maxWidth ) maxWidth = column; */
    }
  }
  if( nCanvas==1 || lineNo>1 ){
    ET( $c config -width %d(maxWidth/2) -height %d(lineNo) );
    ET( pack $c -side left -anchor n -padx 1 );
  }
}

void main(int argc, char **argv){
  int errcnt = 0;
  int widgetCnt = 0;
  int i;

  Et_Init(&argc,argv);
  if( argc<2 ){
    fprintf(stderr,"Usage: %s filename...\n",argv[0]);
    exit(1);
  }
  ET( option add *highlightThickness 0 );
  for(i=1; i<argc; i++){
    FILE *in = fopen(argv[i],"r");
    if( in==0 ){
      char zMsg[1000];
      sprintf(zMsg,"Can't open %.*s",(int)sizeof(zMsg)-100,argv[i]);
      perror(zMsg);
      errcnt++;
    }else{
      char zWidgetName[20];
      if( widgetCnt ){
        sprintf(zWidgetName,".w%d",widgetCnt);
        ET( toplevel %s(zWidgetName) );
        ET( wm title %s(zWidgetName) [file tail "%q(argv[i])"] );
      }else{
        zWidgetName[0] = 0;
        ET( wm title . [file tail "%q(argv[i])"] );
      }
      widgetCnt++;
      ProcessFile(in,zWidgetName);
    }
    ET( update );
  }
  if( errcnt ) exit(errcnt);
  Et_MainLoop();
}
