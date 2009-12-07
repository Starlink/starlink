# !!begin
# !!author T.R. Marsh
# !!created 08 July 2000
# !!revised 12 June 2001
# !!title  Perl auto-documentation script
# !!root   document
# !!index  document.pl
# !!descr  generates documentation for programs etc.
# !!head1  document - perl routine for generating documentation.
#
# The problem this routine tries to handle is that of documenting
# code, scripts etc. In particular the need for documentation in
# more than one format. The idea is to define a set of flags which
# are recognised and then translated into html, Latex or whatever.
# 
# For example the line: 
#
# %!!head1 Program definition
#
# will be picked up and the text following the %!!head1 will be
# converted into an appropriate heading. In this manner only one
# bit of documentation is needed to generate the different types.
# 
#If you want to extend the argument over more than one line you need
# to delimit it with braces as in 
#
# %!!head1{This is a very long definition of an argument that
# looks better if split}.
#
# Any control flags requiring more than 1 argument must have them
# delimited by braces e.g.
#
# %!!arg{ argument } { argument description}
#
# which is used to define arguments in a table.
#
# !!head2 Subroutine call: document($file, $dir, $ext, $style, $cflag)
#
# !!table
# !!arg{ $file }{ file name to extract documentation from.}
# !!arg{ $dir  }{ directory to place files}
# !!arg{ $ext  }{ extension to give files}
# !!arg{ $style}{ style 'html' or 'latex'}
# !!arg{ $cflag}{ comment flag e.g. // for C++, or # for perl}
# !!table
#
# Each time "document" is run it will add entries to a hash called
# %index which will have keys corresponding to items to
# be entered into a list (such as a command list or a list
# of scripts). For each item the following is available:
#
# !!table
# !!arg{ $index{$item}->{file}}{ corresponding file within the directory $dir}
# !!arg{ $index{$item}->{description}}{the description to be attached to the 
# item.} 
# !!arg{$index{$item}->{source}}{the source file of the docmentation}
# !!table
#
# The index hash can be used to generate an index file at the end if wanted.
#
# !!head2 Syntax to use within documentation.
#
# All flags are introduced by %!!. In some cases arguments are
# expected after a flag. As explained above braces can be used to delimit
# arguments or just the end of the line for single arguments. The exception
# to the rule is %!!emph which always requires braces since it will most often
# be used wihin lines.
# Here follows a list of recognised flags:
#
# !!table
# !!arg{%!!arg{arg1}{arg2}}{argument definition in a table, 
# arg1 is the argument and arg2 is the description, as in this table.}
# !!arg{%!!author arg}{name of author of document}
# !!arg{%!!begin}{beginning of documentation}
# !!arg{%!!break}{force a line break}
# !!arg{%!!class arg}{A class that the program belongs to}
# !!arg{%!!created arg}{Creation date}
# !!arg{%!!css arg}{Name of a cascading style script file}
# !!arg{%!!date arg}{Deprectaed version of %!!created}
# !!arg{%!!descr arg}{description to be included in an index}
# !!arg{%!!emph{arg}}{emphasize text of arg}
# !!arg{%!!head1 arg}{First level heading set to arg}
# !!arg{%!!head2 arg}{Second level heading set to arg}
# !!arg{%!!head3 arg}{Third level heading set to arg}
# !!arg{%!!index arg}{Index entry name}
# !!arg{ %!!ref{arg1}{arg2}}{Generates a reference to a file arg1 
# called arg2. Mainly designed for hyper-links so that arg1 can be an 
# html file and arg2 what you want it to be called.}
# !!arg{%!!revised arg}{Revision date}
# !!arg{%!!root arg}{root name for file. An extension such a .html is added
# to this according to whatever is specified as an argument to document.pl}
# !!arg{%!!start}{restarts document input (see %!!stop)}
# !!arg{%!!stop}{suspends document input (see %!!start)}
# !!arg{%!!table}{Start and end of a table.}
# !!arg{%!!title arg}{gives a title to the document}
# !!arg{%!!trow{arg1}{arg2} etc}{more generalised tables than arg 1 to 
# 5 arguments}
# !!table
#
# A (commented) blank line generates a new paragraph.
#
# !!end

sub document{
    my($file,$dir,$ext,$style,$cflag) = @_;
    my($author,$time,$print,$table,$fname);
    
    ($style =~ /html/ || $style =~ /ascii/) or 
	die "$style is not recognised by document\n";
    
# Read in all text between begin/end, start/stop. 
# Comment flags stripped. Text loaded into a single
# string $string
    
    open(FILE, "$file") or die "Failed to open $file\n";
    $store  = 0;
    $string = "";
    while(<FILE>){
	s/^\s*$cflag//;
	if(/^\s*!!begin\s*$/ || /^\s*!!start\s*$/){
	    $store = 1;
	}
	if($store) {
	    if(/^$/){
		$string .= "\n";
	    }else{
		$string .= $_;
	    }
	}
	if(/^\s*!!end\s*$/   || /^\s*!!stop\s*$/) {
	    $store = 0;
	}
    }

    close(FILE);
    
    (!$store) or die "A !!begin without a matching !!end was encountered in $file\n";
    
    ($string ne "") or die "$file contains no documentation!!\n";

    $time  = localtime;
    $print = 0;
    $table = 0;
    $verb  = 0;

    $string =~ s/%!!/%%&&%%/g;
    $string =~ s/\\\{/%%aa%%/g;
    $string =~ s/\\\}/%%bb%%/g;

    $temp   = $string;
    while($temp =~ /!!begin(.*?)!!end(.*)$/s){
	$doc  = $1;
	$temp = $2;
	undef $author;
	undef $created;
	undef $date;
	undef $descr;
	undef $index;
	undef $revised;
	undef $root;
	undef $title;
	@class  = ();
	$nclass = 0;
	if($style =~ /html/){

# references to other files, line breaks, emphasis, tables

	    $doc =~ s/!!ref\s*\{\s*(.*?)\s*\}\s*\{\s*(.*?)\s*\}/<a href=\"$1\">$2<\/a>/gs;
	    $doc =~ s/!!break/<br>/g;
	    $doc =~ s/!!emph\{\s*(.*?\s*)\}/<strong>$1<\/strong>/gs;
	    $doc =~ s/!!table(.*?)!!table/\n<table>$1<\/table>/gs;	

# deal with headings
	
	    $doc =~ s/!!head(\d)\s*\{\s*(.*?)\s*\}/\n<h$1>$2<\/h$1>\n\n/gs;
	    $doc =~ s/!!head(\d)\s*(.*?)\s*\n/\n<h$1>$2<\/h$1>\n\n/g;

# we now want to put <p> every time there is a blank line except in tables
# so we first substitute out such stuff from tables, then look for blank lines
# then substitute the table stuff back.

	    while($doc =~ s/(<table>.*?)\n\s*\n(.*?<\/table>)/$1ZzXzZ$2/gs){};
	    $doc =~ s/\n\s*\n/\n<p>\n/g;
	    $doc =~ s/ZzXzZ/\n\n/gs;	

# pick up author, creation and revision dates, description, 
# index reference, root file name, title
# allow for one-liners and delimited arguments
	    
	    if($doc =~ s/!!author\s*\{(.*?)\}\s*\n?//s){$author = $1;}
	    if($doc =~ s/!!author\s*(.*?)\s*\n//){$author = $1;}
	    if($doc =~ s/!!created\s*\{(.*?)\}\s*\n?//s){$created = $1;}
	    if($doc =~ s/!!created\s*(.*?)\s*\n//){$created = $1;}
	    if($doc =~ s/!!date\s*\{(.*?)\}\s*\n?//s){$date = $1;}
	    if($doc =~ s/!!date\s*(.*?)\s*\n//){$date = $1;}
	    if($doc =~ s/!!descr\s*\{(.*?)\}\s*\n?//s){$descr = $1;}
	    if($doc =~ s/!!descr\s*(.*?)\s*\n//){$descr = $1;}
	    if($doc =~ s/!!index\s*\{(.*?)\}\s*\n?//s){$index = $1;}
	    if($doc =~ s/!!index\s*(.*?)\s*\n//){$index = $1;}
	    if($doc =~ s/!!revised\s*\{(.*?)\}\s*\n?//s){$revised = $1;}
	    if($doc =~ s/!!revised\s*(.*?)\s*\n//){$revised = $1;}
	    if($doc =~ s/!!root\s*\{\s*(\S+?)\s*\}\s*\n?//s){$root = $1;}
	    if($doc =~ s/!!root\s*(\S+?)\s*\n//){$root = $1;}
	    if($doc =~ s/!!title\s*\{\s*(.*?)\s*\}\s*\n?//s){$title = $1;}
	    if($doc =~ s/!!title\s*(.*?)\s*\n//){$title = $1;}
	    if($doc =~ s/!!css\s*\{\s*(.*?)\s*\}\s*\n?//s){$css = $1;}
	    if($doc =~ s/!!css\s*(.*?)\s*\n//){$css = $1;}
	    while($doc =~ s/!!class\s*\{(.*?)\}\s*\n?//s){
		$class[$nclass++] = $1;
	    }
	    while($doc =~ s/!!class\s*(.*?)\s*\n//){
		$class[$nclass++] = $1;
	    }

# argument lists
	    
	    $doc =~ s/
		!!arg\s*\{\s*(.*?)\s*\}\s*\{\s*(.*?)\s*\}\s*\n?
	    /<tr valign=\"top\"><td><i>$1<\/i><\/td><td>---<\/td><td>$2<\/td><\/tr>\n/xgs;


# more general table entries 1--5 entries

	    $doc =~ s/!!trow
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
	        [^{]\s*\n?
		/<tr valign=\"top\"><td>$1<\/td><td>$2<\/td><td>$3<\/td><td>$4<\/td><td>$5<\/td><\/tr>\n/xgs;

	    $doc =~ s/!!trow
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
	        [^{]\s*\n?
		/<tr valign=\"top\"><td>$1<\/td><td>$2<\/td><td>$3<\/td><td>$4<\/td><\/tr>\n/xgs;

	    $doc =~ s/!!trow
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
	        [^{]\s*\n?
		/<tr valign=\"top\"><td>$1<\/td><td>$2<\/td><td>$3<\/td><\/tr>\n/xgs;

	    $doc =~ s/!!trow
		\s*\{\s*([^{}]*?)\s*\}
		\s*\{\s*([^{}]*?)\s*\}
                [^{]\s*\n?
		/<tr valign=\"top\"><td>$1<\/td><td>$2<\/td><\/tr>\n/xgs;

	    $doc =~ s/!!trow
		\s*\{\s*([^{}]*?)\s*\}
                [^{]\s*\n?
		/<tr valign=\"top\"><td>$1<\/td><\/tr>\n/xgs;
	    
	 }elsif($style =~ /ascii/){

	     $wrap  = 80;
	     $newl  = "%NEWLINE%";
	     $dnewl = "%DNEWLINE%";
	     $doc =~ s/!!ref\s*\{\s*(.*?)\s*\}\s*\{\s*(.*?)\s*\}/$2/gs; # remove html refs
	     $doc =~ s/!!emph\{\s*(.*?\s*)\}/$1/gs;                     # remove emphasis flags

# make headings by adding = signs underneath them

	     while($doc =~ m/!!head(\d)\s*\{\s*(.*?)\s*\}/s){
		 $emph = repeat('=', length($2));
		 $doc =~ s/!!head(\d)\s*\{\s*(.*?)\s*\}/$dnewl$2$newl$emph$dnewl/s;
	     }

	     while($doc =~ m/!!head(\d)\s*(.*?)\s*\n\n/s){
		 $emph = repeat('=', length($2));
		 $doc =~ s/!!head(\d)\s*(.*?)\s*\n\n/$dnewl$2$newl$emph$dnewl/s;
	     }

	     if($doc =~ s/!!author\s*\{(.*?)\}\s*\n?//s){$author = $1;}
	     if($doc =~ s/!!author\s*(.*?)\s*\n//){$author = $1;}
	     if($doc =~ s/!!created\s*\{(.*?)\}\s*\n?//s){$created = $1;}
	     if($doc =~ s/!!created\s*(.*?)\s*\n//){$created = $1;}
	     if($doc =~ s/!!date\s*\{(.*?)\}\s*\n?//s){$date = $1;}
	     if($doc =~ s/!!date\s*(.*?)\s*\n//){$date = $1;}
	     if($doc =~ s/!!descr\s*\{(.*?)\}\s*\n?//s){$descr = $1;}
	     if($doc =~ s/!!descr\s*(.*?)\s*\n//){$descr = $1;}
	     if($doc =~ s/!!index\s*\{(.*?)\}\s*\n?//s){$index = $1;}
	     if($doc =~ s/!!index\s*(.*?)\s*\n//){$index = $1;}
	     if($doc =~ s/!!revised\s*\{(.*?)\}\s*\n?//s){$revised = $1;}
	     if($doc =~ s/!!revised\s*(.*?)\s*\n//){$revised = $1;}
	     if($doc =~ s/!!root\s*\{\s*(\S+?)\s*\}\s*\n?//s){$root = $1;}
	     if($doc =~ s/!!root\s*(\S+?)\s*\n//){$root = $1;}
	     if($doc =~ s/!!title\s*\{\s*(.*?)\s*\}\s*\n?//s){$title = $1;}
	     if($doc =~ s/!!title\s*(.*?)\s*\n//){$title = $1;}
	     if($doc =~ s/!!css\s*\{\s*(.*?)\s*\}\s*\n?//s){$css = $1;}
	     if($doc =~ s/!!css\s*(.*?)\s*\n//){$css = $1;}
	     while($doc =~ s/!!class\s*\{(.*?)\}\s*\n?//s){
		 $class[$nclass++] = $1;
	     }
	     while($doc =~ s/!!class\s*(.*?)\s*\n//){
		 $class[$nclass++] = $1;
	     }
	     
	     $doc =~ s/([^\n])\n([^\n])/$1 $2/gs;     # remove \n

# Now deal with tables

	     $spec1 = "%ZZZZ%";
	     $spec2 = "%XXXX%";
	     while($doc =~ s/!!table(.*?)!!table/$spec1$1$spec1/s){
		 $larg = 0;
		 while($doc =~ s/$spec1(.*?)!!arg\s*\{\s*(.*?)\s*}\s*\{\s*(.*?)\s*\}(.*?)$spec1/$spec1$1$spec2$2$spec2$3$spec2$4$spec1/s){
		     $larg = $larg > length($2) ? $larg : length($2);
		 }
		 while($doc =~ m/$spec2(.*?)$spec2(.*?)$spec2/s){
		     $just =  wrap($2,$larg+6,$wrap);
		     $pad  =  repeat(' ', $larg - length($1)) . " --- ";
		     $doc  =~ s/$spec2(.*?)$spec2(.*?)$spec2/$1$pad$just\n/s;
		 }
	         $doc =~ s/$spec1(.*?)$spec1/\n$1\n/s;
	     }

	     $doc =~ s/$newl/\n/gs;                                     # put back newlines
	     $doc =~ s/$dnewl/\n\n/gs;                                  # put back double newlines
	     $doc =~ s/!!break/\n/g;                                    # put \n for line breaks
	    
	}	
	if(defined $root){

	    open(OUT, ">$dir/$root.$ext") or die "Could not open $dir/$root.$ext\n";

	    if($style =~ /html/){

		print OUT "<html>\n<head>\n";
		(defined $title) && print OUT "<title>$title</title>\n";
		if(defined $css){
		    print OUT "<link rel=stylesheet href=\"$css\" type=\"text/css\">\n";
		}
		print OUT "</head>\n";
		print OUT "<body>\n\n";

	    }else{

		if(defined $title){
		    print OUT "\n$title\n";
		    $emph = repeat('=', length($title));
		    print OUT "$emph\n\n";
		}

	    }

	    $string =~ s/%!!/%%&&%%/g;
	    $string =~ s/%%aa%%/\{/g;
	    $string =~ s/%%bb%%/\}/g;

	    print OUT $doc;

	    if($style =~ /html/){

		if(scalar(@class)){
		    print OUT "\n\n<h2>Classes</h2>\n\n<p>\n";
		    if(scalar(@class) == 1){
			print OUT "This command is a member of the class: ";
		    }else{
			print OUT "This command is a member of the classes: ";
		    }
		    $first = 1;
		    foreach $class (sort @class){
			if($first){
			    print OUT "<a href=\"$class.html\">$class</a>";
			    $first = 0;
			}else{
			    print OUT ", <a href=\"$class.html\">$class</a>";
			}
		    }
		    print OUT ".\n";
		}
		print OUT "\n<p>\n";

		(defined $author)  && print OUT "Author: $author<br>\n";
		(defined $date)    && print OUT "Author: $author<br>\n";
		(defined $created) && print OUT "Created: $created<br>\n";
		(defined $revised) && print OUT "Revised: $revised<br>\n";

		print OUT "\n<p>\n<hr>\n";
		(defined $author) && print OUT "<address>Page generated $time<\/address>\n";
		print OUT "</body>\n</html>\n";

	    }elsif($style =~ /ascii/){

		if(scalar(@class)){
		    print OUT "\n";
		    print OUT "-------------------------------------------------------\n\n";
		    if(scalar(@class) == 1){
			print OUT "This command is a member of the class: ";
		    }else{
			print OUT "This command is a member of the classes: ";
		    }
		    $first = 1;
		    foreach $class (sort @class){
			if($first){
			    print OUT "$class";
			    $first = 0;
			}else{
			    print OUT ", $class";
			}
		    }
		    print OUT ".\n";
		}
		print OUT "\n";

		(defined $author)  && print OUT "Author:  $author\n";
		(defined $date)    && print OUT "Author:  $author\n";
		(defined $created) && print OUT "Created: $created\n";
		(defined $revised) && print OUT "Revised: $revised\n";

		print OUT "\n";
		(defined $author) && print OUT "Page generated $time\n";

	    }
	    close(OUT);
	    
# set the index hash entries, first changing any leading
# uppercase letter to lower case
	    
	    if($descr =~ /^\s*([A-Z])/){
		($first = $1) =~ tr/A-Z/a-z/;
		$descr =~ s/^\s*([A-Z])/$first/;
	    }
	    $index{$index}->{file}        = "$root.$ext";
	    $index{$index}->{description} = $descr;
	    $index{$index}->{source}      = $file;
	    if(scalar(@class)){
		$index{$index}->{class} = [@class];
	    }
	}else{
	    die "No root defined for $file\n";
	}
    }
}

sub repeat {
    my ($c,$n) = @_;
    my $repstr = "";
    my $i;
    for($i=0; $i<$n; $i++){
	$repstr .= $c;
    }
    return $repstr;
}

sub wrap {
    my ($text, $ngap, $wrap) = @_;
    my $wcount  =  0;
    my $plast   =  0;
    my $blast   = -$ngap;
    my $pad     =  repeat(' ',$ngap);
    pos($text)  =  0;
    while($text =~ m/\S+/g){
	$wcount++;
	if(pos($text) > $blast + $wrap){
	    if($wcount == 1){
		$plast = pos($text);
	    }
	    $length = $plast+1;
	    $text   =~ s/^(.{$length})/$1\n$pad/s;
	    $blast  = $plast+2;
	    $wcount = 0;
	}
	$plast = pos($text);
    }
    return $text;
}

1;
    












