NAME=openapi
VERSION=0.8.0

DIST_DIR=openapi-ada-$(VERSION)
DIST_FILE=openapi-ada-$(VERSION).tar.gz

MAKE_ARGS += -XOPENAPI_BUILD=$(BUILD)

-include Makefile.conf

HAVE_SERVER?=yes
HAVE_CURL?=yes
HAVE_AWS?=yes
HAVE_EWS?=yes

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XOPENAPI_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XOPENAPI_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSERVLETADA_CORE_BUILD=relocatable
SHARED_MAKE_ARGS += -XSERVLET_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSERVLETADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XSERVLET_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XELADA_BUILD=relocatable -XEL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSECURITYADA_BUILD=relocatable -XSECURITY_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable -XAWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTIL_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

DEFAULT_ADA_PROJECT_PATH:=$(SRC_ROOT)

ifeq ($(HAVE_SERVER),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/server
endif

DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(ADA_PROJECT_PATH)

CLEAN_FILES=server/src/openapi-servers-config.ads

build-test::  lib-setup
ifeq ($(HAVE_ALIRE),yes)
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) 
else
ifeq ($(HAVE_AWS),yes)
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Ptestapi_server_aws.gpr
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Popenapi_tests_aws.gpr
endif
ifeq ($(HAVE_CURL),yes)
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) -Popenapi_tests_curl.gpr
endif
endif

OPENAPI=./scripts/openapi-generator
OPENAPI_OPTIONS=--enable-post-process-file

generate:
	$(OPENAPI) generate --enable-post-process-file --generator-name ada -i regtests/swagger.yaml \
            --additional-properties projectName=TestAPI $(OPENAPI_OPTIONS) \
            --model-package TestAPI -o regtests/client
	$(OPENAPI) generate --enable-post-process-file --generator-name ada -i regtests/responses-binary.yaml \
            --additional-properties projectName=TestBinary $(OPENAPI_OPTIONS) \
            --model-package TestBinary -o regtests/client
	$(OPENAPI) generate --generator-name ada-server -i regtests/swagger.yaml \
            --additional-properties projectName=TestAPI $(OPENAPI_OPTIONS) \
            --model-package TestAPI -o regtests/server
	$(OPENAPI) generate --generator-name ada-server -i regtests/responses-binary.yaml \
            --additional-properties projectName=TestBinary $(OPENAPI_OPTIONS) \
            --model-package TestBinary -o regtests/server

# Build and run the unit tests
test:	build-test
ifeq ($(HAVE_EWS),yes)
	bin/testapi_ews > testapi-server-ews.log & \
        SERVER_PID=$$!; \
        sleep 1; \
	(test ! -f bin/openapi_harness_aws || \
          bin/openapi_harness_aws -l $(NAME):AWS:EWS -p AWS_EWS -config tests.properties -xml openapi-aws-ews-aunit.xml) ;\
	(test ! -f bin/openapi_harness_curl || \
          bin/openapi_harness_curl -l $(NAME):CURL:EWS -p CURL_EWS -config tests.properties -xml openapi-curl-ews-aunit.xml) ;\
        kill $$SERVER_PID
endif
ifeq ($(HAVE_AWS),yes)
	bin/testapi_aws > testapi-server-aws.log & \
        SERVER_PID=$$!; \
        sleep 1; \
	(test ! -f bin/openapi_harness_aws || \
          bin/openapi_harness_aws -l $(NAME):AWS:AWS -p AWS_AWS -config tests.properties -xml openapi-aws-aws-aunit.xml) ;\
	(test ! -f bin/openapi_harness_curl || \
          bin/openapi_harness_curl -l $(NAME):CURL:AWS -p CURL_AWS -config tests.properties -xml openapi-curl-aws-aunit.xml) ;\
        kill $$SERVER_PID
	test ! -f bin/openapi_harness_aws || \
          bin/openapi_harness_aws -p AWS -config tests-client.properties -xml openapi-aws-aunit.xml
	test ! -f bin/openapi_harness_curl || \
          bin/openapi_harness_curl -p CURL -config tests-client.properties -xml openapi-curl-aunit.xml
endif

install:: install-data

install-data::
	rm -rf $(DESTDIR)${prefix}/share/openapi-ada
	${MKDIR} -p $(DESTDIR)${prefix}/share/openapi-ada
	${CP} -rp server/web $(DESTDIR)${prefix}/share/openapi-ada/web
	${MKDIR} -p $(DESTDIR)${prefix}/bin
	$(INSTALL) scripts/openapi-generator $(DESTDIR)$(prefix)/bin/openapi-generator
	$(INSTALL) scripts/openapi-generate-client $(DESTDIR)$(prefix)/bin/openapi-generate-client
	$(INSTALL) scripts/openapi-generate-server $(DESTDIR)$(prefix)/bin/openapi-generate-server
	$(CP) share/openapi-ada/openapi-generator-cli.jar $(DESTDIR)$(prefix)/share/openapi-ada

$(eval $(call ada_library,$(NAME),.))

ifneq (, ${PANDOC})
doc::  docs/openapi-book.pdf docs/openapi-book.html
ifneq (${DYNAMO},)
	$(DYNAMO) build-doc -markdown wiki
endif

OPENAPI_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Tutorial.md

DOC_OPTIONS=-f markdown -o openapi-book.pdf --listings --number-sections --toc
HTML_OPTIONS=-f markdown -o openapi-book.html --listings --number-sections --toc --css pandoc.css

docs/openapi-book.pdf: $(OPENAPI_DOC_DEP) force
ifneq (${DYNAMO},)
	$(DYNAMO) build-doc -pandoc docs
endif
	#cat docs/Model.md docs/ADO_Objects.md > docs/ADO_Model.md
	cd docs && pandoc $(DOC_OPTIONS) --template=./eisvogel.tex $(OPENAPI_DOC)

docs/openapi-book.html: docs/openapi-book.pdf force
	cd docs && pandoc $(HTML_OPTIONS) $(OPENAPI_DOC)

endif

install::
	$(ALR) exec -- $(GPRINSTALL) -p -f --prefix=$(DESTDIR)${prefix} \
          $(STATIC_MAKE_ARGS) swagger.gpr

uninstall::
	-$(ALR) exec -- $(GPRINSTALL) --uninstall -q -f --prefix=$(DESTDIR)${prefix} $(MAKE_ARGS) swagger.gpr

ifeq ($(HAVE_SERVER),yes)
ifneq ($(HAVE_ALIRE),yes)
lib-setup:: server/src/openapi-servers-config.ads

server/src/openapi-servers-config.ads: server/src/openapi-servers-config.gpb
	$(GNATPREP) -DWEB_DIR=\"$(PREFIX)\" \
             server/src/openapi-servers-config.gpb \
             server/src/openapi-servers-config.ads
endif

$(eval $(call ada_library,openapi_server,server))

install::
	$(ALR) exec -- $(GPRINSTALL) -p -f --prefix=$(DESTDIR)${prefix} \
          $(STATIC_MAKE_ARGS) server/swagger_server.gpr

uninstall::
	-$(ALR) exec -- $(GPRINSTALL) --uninstall -q -f --prefix=$(DESTDIR)${prefix} $(MAKE_ARGS) swagger_server.gpr
endif

$(eval $(call alire_publish,.,op/openapi,openapi-$(VERSION).toml))
$(eval $(call alire_publish,server,op/openapi_server,openapi_server-$(VERSION).toml))

setup:: 
	echo "HAVE_SERVER=$(HAVE_SERVER)" >> Makefile.conf
	echo "HAVE_CURL=$(HAVE_CURL)" >> Makefile.conf
	echo "HAVE_AWS=$(HAVE_AWS)" >> Makefile.conf
	echo "HAVE_EWS=$(HAVE_EWS)" >> Makefile.conf
