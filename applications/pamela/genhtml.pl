#
# Perl script to generate html help pages for pamela.
# Generates
#
# 1) Help on each command
# 2) Class command lists
# 3) List of classes.
# 4) List of commands
#
# Load hash of pointers to where help is contained on each command

(@ARGV == 2) or die "usage: genhtml.pl directory alias\n";

$html = shift;

open(HELP,"help_pointers")
    or die "Could not open help_pointers!\n";
while(<HELP>){
    ($command,$file) = split(' ');
    $command = lc $command;
    $help{$command} = $file;
}
close(HELP);

open(CDEFS,"command_defs")
    or die "Could not open command_defs!\n";
$i = 0;
while(<CDEFS>){
    if(/^class (.*?) *# *(.*)$/){
       $class = lc $1;
       $classes{$class} = $2;
    }
    if(/^([a-zA-Z]*) *-- *(.*) *# *(.*)/){
       $command = lc $1;
       $prompt  = $2;
       $classes = [split(' ', $3)];
       $comms{$command} = {
	   "PROMPT"  => $prompt,
           "CLASSES" => $classes,
       };
    }
}
close(CDEFS);

# Now generate html for every command

print "Generating command help files ...\n";

foreach $command (keys %comms){
    if($help{$command}){
	open(IFILE, $help{$command}) or
	    die "Failed to open $help{$command} for input\n";

	# Open output and write out standard preamble. Gives command
	# name, standard links to command and class lists and intro.
	# Sets up rest for preformatted input. Use a css file to define the
	# style.

	open(OFILE, ">$html/$command.html") or
	    die "Failed to open $html/$command.html for output\n";

	print OFILE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"\n";
	print OFILE "  \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
	print OFILE "<html>\n<head>\n<title>Help text on $command</title>\n";
	print OFILE "<link rel=stylesheet href=\"pamela.css\" type=\"text/css\">\n";
	print OFILE "</head>\n<body>\n<h1>$command</h1>\n\n";
	print OFILE "<p>\n<pre>\n";

	# Now get stuff from input file

	$output = 0;
	$found = 0;
	while(<IFILE>){
	    if(/^\*$command/i){
		$output = !$output;
	    }elsif($output){
                if(/.( *Related *commands?): *(.*) *$/){
		    $found = 1;
		    $save = $1;
		    @related = split / *, */, $2;
		}else{
		    $_ =~ s/^.//;
		    print OFILE;
		}
	    }
	}
	close(IFILE);
	print OFILE "</pre>\n";
	if($found){
	    print OFILE "$save:\n";
	    $n = 0;
	    foreach $com (@related){
		$com = lc $com;
		if($n){
		    print OFILE ", <a href=\"$com.html\">$com</a>\n";
		}else{
		    print OFILE "<a href=\"$com.html\">$com</a>\n";
		}
		$n++;
	    }
	}

	if(@{$comms{$command}->{"CLASSES"}} > 1){
	    print OFILE "\n<p>This command belongs to the classes:\n";
	}else{
	    print OFILE "\n<p>This command belongs to the class:\n";
	}
	$n = 0;
	foreach $class (@{$comms{$command}->{"CLASSES"}}){
	    $class = lc $class;
	    if($n){
		print OFILE ", <a href=\"$class.class.html\">$class</a>\n";
	    }else{
		print OFILE "<a href=\"$class.class.html\">$class</a>\n";
	    }
	    $n++;
	}

	print OFILE "\n<p>\n<hr>\n<address>\n";
	print OFILE "Tom Marsh, Warwick\n</address>\n</body>\n</html>\n";
	close(OFILE);
    }else{
	print "No help found on \"$command\"\n";
    }
}

# Now generate html for every class

print "Generating class help files ...\n";

foreach $class (keys %classes){

    open(OFILE, ">$html/$class.class.html") or
	die "Failed to open $html/$class.class.html for output\n";

    print OFILE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"\n";
    print OFILE "  \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
    print OFILE "<html>\n<head>\n<title>Help text on $class pamela class</title>\n";
    print OFILE "<link rel=stylesheet href=\"pamela.css\" type=\"text/css\">\n";
    print OFILE "</head>\n<body>\n<h1>pamela $class commands</h1>\n";
    print OFILE "\n<p>\n<table>\n<ul>\n";

    foreach $command (sort keys %comms){
	$found = 0;
	foreach $clss (@{$comms{$command}->{"CLASSES"}}){
	    if($class eq $clss){
		$found = 1;
	    }
	}
	if($found){
	    print OFILE "<tr><td><a href=\"$command.html\">$command</a></td>";
	    print OFILE "<td>--</td>";
	    print OFILE "<td>$comms{$command}->{PROMPT}</td></tr>\n";
	}
    }
    print OFILE "\n</table>\n</ul>\n\n<p>\n<hr>\n<address>\n";
    print OFILE "Tom Marsh, Warwick\n</address>\n</body>\n</html>\n";
    close(OFILE);
}

# Now generate class list

print "Generating class list ...\n";

open(OFILE, ">$html/CLASSES.html") or
    die "Failed to open $html/CLASSES.html for output\n";

print OFILE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"\n";
print OFILE "  \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
print OFILE "<html>\n<head>\n<title>pamela command classes</title>\n";
print OFILE "<link rel=stylesheet href=\"pamela.css\" type=\"text/css\">\n";
print OFILE "</head>\n<body>\n<h1>pamela command classes</h1>\n";

print OFILE <<END1;
<p>
The following headings classify pamela commands lossely according to their
function. The same command may appear in more than one class. The help
files on individual commands sometimes give links to other closely related
commands as well.
END1

print OFILE "\n<p>\n<ul>\n<table>\n";

foreach $class (sort keys %classes){
    print OFILE "<tr><td><a href=\"$class.class.html\">$class</a></td>\n";
    print OFILE "<td>--</td>";
    print OFILE "<td>$classes{$class}</td></tr>\n";
}
print OFILE "\n</table>\n</ul>\n\n<p>\n<hr>\n<address>\n";
print OFILE "Tom Marsh, Warwick\n</address>\n</body>\n</html>\n";
close(OFILE);

# Finally the full command list

print "Generating command list ...\n";

open(OFILE, ">$html/COMMANDS.html") or
    die "Failed to open $html/COMMANDS.html for output\n";

print OFILE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"\n";
print OFILE "  \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
print OFILE "<html>\n<head>\n<title>pamela commands</title>\n";
print OFILE "<link rel=stylesheet href=\"pamela.css\" type=\"text/css\">\n";
print OFILE "</head>\n<body>\n<h1>pamela commands</h1>\n";

print OFILE <<END2;
<p>
This is the full list of commands available inside pamela. If you
are new to the program you may find it easier to search using
the <a href="CLASSES.html">command classes</a> first of all.
END2

print OFILE "\n<p>\n<ul>\n<table>\n";

foreach $command (sort keys %comms){
    print OFILE "<tr><td><a href=\"$command.html\">$command</a></td>";
    print OFILE "<td>--</td>";
    print OFILE "<td>$comms{$command}->{PROMPT}</td></tr>\n";
}
print OFILE "\n</table>\n</ul>\n\n<p>\n<hr>\n<address>\n";
print OFILE "Tom Marsh, Warwick\n</address>\n</body>\n</html>\n";
close(OFILE);

# Now generate class list

print "Generating class list ...\n";

open(OFILE, ">$html/CLASSES.html") or
    die "Failed to open $html/CLASSES.html for output\n";

print OFILE "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"\n";
print OFILE "  \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
print OFILE "<html>\n<head>\n<title>pamela command classes</title>\n";
print OFILE "<link rel=stylesheet href=\"pamela.css\" type=\"text/css\">\n";
print OFILE "</head>\n<body>\n<h1>pamela command classes</h1>\n";

print OFILE <<END1;
<p>
The following headings classify pamela commands lossely according to their
function. The same command may appear in more than one class. The help
files on individual commands sometimes give links to other closely related
commands as well.
END1

print OFILE "\n<p>\n<ul>\n<table>\n";

foreach $class (sort keys %classes){
    print OFILE "<tr><td><a href=\"$class.class.html\">$class</a></td>\n";
    print OFILE "<td>--</td>";
    print OFILE "<td>$classes{$class}</td></tr>\n";
}
print OFILE "\n</table>\n</ul>\n\n<p>\n<hr>\n<address>\n";
print OFILE "Tom Marsh, Warwick\n</address>\n</body>\n</html>\n";
close(OFILE);

# Finally the index file

print "Generating index file\n";

open(INDEX, ">$html/INDEX.html") or
    die "Failed to open $html/INDEX.html for output\n";

print INDEX <<END3;

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN"
  "http://www.w3.org/TR/REC-html40/strict.dtd">
<html>
<HEAD>
<TITLE>pamela</TITLE>
<link rel=stylesheet href="pamela.css" type="text/css">
</HEAD>
<BODY>
<H1>pamela</H1>

<p>
Should you publish any papers using pamela I would appreciate an
acknowledgement.

<p>
pamela is a package of routines for the reduction of 2D spectrum data
to 1D. It does not deal with any subsequent steps such as arc
calibration etc. pamela includes full propagation of uncertainties
through the reduction. It implements 2 forms of optimal extraction
which are Keith Horne's original method for reasonably straight
spectra and my revised version for spectra with significant tilts or
curvature.

<P>
pamela now runs over NDF and uses the ADAM parameter system.

<p>
To get pamela running, source the script $alias. You may want to
define an alias for this if you use pamela frequently.

<H2> Information on pamela </H2>

<MENU>

<LI> <A HREF = "SETUP.html"> Setting up pamela </A>

<LI> <A HREF = "RECIPE.html"> Getting started with pamela </A>

<LI> <A HREF = "CLASSES.html"> Classified list of pamela commands </A>

<LI> <A HREF = "COMMANDS.html"> Alphabetically ordered command list </A>

<LI> <A HREF = "SCRIPTS.html"> Useful general scripts for pamela-based reduction. </A>

<LI> <A HREF = "EXAMPLE_SCRIPTS.html">Example one-off scripts at starters to develop new ones</A>

<LI> <A HREF = "CHANGES.html"> List of bug fixes and upgrades.</A> Replaced as of 20/01/2009
by the <a href="git_change_log">change list</a> generated by git.

<LI> <A HREF = "HISTORY.html"> Brief history of pamela </A>

<LI> <A HREF = "CUSTOM.html"> How to adjust the parameter prompting </A>

</MENU>

<HR>
<ADDRESS>
Tom Marsh, Warwick.
</ADDRESS>
</BODY>
END3

print "Finished.\n\n";

exit;


