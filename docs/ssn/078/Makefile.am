## Process this file with automake to produce Makefile.in

stardocs_DATA = ssn78.htx_tar

# Location of the Jade binary.  OpenJade appears to be installed by default
# on RedHat systems since 7.3.
JADE=@JADE@
SGMLNORM=@SGMLNORM@
STARLINK_SGML_DIR=@STARLINK_SGML_DIR@

# Requires that STARLINK_SGML_DIR hold the path to the top of the Starlink
# SGML Kit, so that $STARLINK_SGML_DIR/bin/sgml2docs is the path to the 
# sgml2docs application.  These will work with any version of Jade,
# in particular with openjade.
ssn78.htx_tar: ssn78.xml
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		STARLINK_SGML_DIR=$(STARLINK_SGML_DIR) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(STARLINK_SGML_DIR)/bin/sgml2docs --html --tarball=$@ \
		ssn78.xml
ssn78-single.htx_tar: ssn78.xml
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		STARLINK_SGML_DIR=$(STARLINK_SGML_DIR) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(STARLINK_SGML_DIR)/bin/sgml2docs --htmlsingle --tarball=$@ \
		ssn78.xml

# The following target will work ONLY if the JADE which is configured is one
# which has Norman's LaTeX patch applied.
ssn78.latex_tar: ssn78.xml
	JADE=$(JADE) SGMLNORM=$(SGMLNORM) \
		STARLINK_SGML_DIR=$(STARLINK_SGML_DIR) \
		SGML_CATALOG_FILES=$(STARLINK_SGML_DIR)/dtd/CATALOG-override \
		$(STARLINK_SGML_DIR)/bin/sgml2docs --latex --tarball=$@ \
		ssn78.xml
