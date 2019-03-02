
NAME=swagger

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=relocatable
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

build-test:  setup
	$(GNATMAKE) -p -Pswagger_tests $(MAKE_ARGS)

ifeq (${HAVE_SERVER},yes)
setup:: src/server/swagger-servers-config.ads

src/server/swagger-servers-config.ads: Makefile src/server/swagger-servers-config.gpb
	gnatprep -DWEB_DIR=\"${sharedir}/swagger-ada/web\" \
		src/server/swagger-servers-config.gpb $@
else
setup::
endif

# Clean the files produced by the unit tests
clean_test:
	rm -rf regtests/result/*

generate:
	java -jar openapi-generator-cli.jar generate \
           --generator-name ada -i regtests/swagger.yaml -o regtests/client \
           -DprojectName=TestAPI --model-package TestAPI
	java -jar openapi-generator-cli.jar generate \
           --generator-name ada-server -i regtests/swagger.yaml -o regtests/server \
           -DprojectName=TestAPI --model-package TestAPI

# Build and run the unit tests
test:	build-test
ifeq (${HAVE_SERVER},yes)
	bin/testapi-server & \
        SERVER_PID=$$!; \
        sleep 1; \
	bin/swagger_harness -config tests.properties -xml swagger-aunit.xml ;\
        kill $$SERVER_PID
else
	bin/swagger_harness -config tests-client.properties -xml swagger-aunit.xml
endif

install_lib:
	$(GPRINSTALL) -p -f --prefix=$(prefix) $(MAKE_ARGS) \
		--build-name=$(SWAGGER_LIBRARY_TYPE) $(GPRPATH)
ifeq (${HAVE_SERVER},yes)
	$(GPRINSTALL) -p -f --prefix=$(prefix) $(MAKE_ARGS) \
		--build-name=$(SWAGGER_LIBRARY_TYPE) $(SERVER_GPRPATH)
endif

install_web:
	rm -rf ${sharedir}/swagger-ada
	${MKDIR} -p ${sharedir}/swagger-ada
	${CP} -rp web ${sharedir}/swagger-ada/web

$(eval $(call ada_library,$(NAME)))

ifeq ($(HAVE_SERVER),yes)
$(eval $(call ada_library,swagger_server))
endif

