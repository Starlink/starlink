DEFECTS=/home/norman/sgml/w/sgml/defect

defects.html: defects.xml
	java com.jclark.xsl.sax.Driver \
		defects.xml $(DEFECTS)/defects.xslt defects.html \
		loc-appendices=appendix/

