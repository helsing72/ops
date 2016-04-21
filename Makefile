BUILD_DEBUG=build.debug
BUILD_OPT=build.opt

CC=$(shell which gcc)
CXX=$(shell which g++)

CCV=$(shell $(CC) -dumpversion)
CXXV=$(shell $(CXX) -dumpversion)

.PHONY : all
all: debug opt
	$(MAKE) deploy

.PHONY : clean
clean: clean_debug clean_opt clean_deploy

.PHONY : clean_debug
clean_debug:
	@echo "Cleaning debug"
	rm -rf $(BUILD_DEBUG)

.PHONY : clean_opt
clean_opt:
	@echo "Cleaning opt"
	rm -rf $(BUILD_OPT)

.PHONY : opt
opt: $(BUILD_OPT)/Makefile
	$(MAKE) -C $(BUILD_OPT) --no-print-directory

$(BUILD_OPT)/Makefile : %/Makefile :
	export CC=$(CC) && \
	export CXX=$(CXX) && \
	mkdir -p $* && \
	cd $* && \
	cmake -DCMAKE_BUILD_TYPE=Release ..

.PHONY : debug
debug: $(BUILD_DEBUG)/Makefile
	$(MAKE) -C $(BUILD_DEBUG) --no-print-directory

$(BUILD_DEBUG)/Makefile : %/Makefile :
	export CC=$(CC) && \
	export CXX=$(CXX) && \
	mkdir -p $* && \
	cd $* && \
	cmake -DCMAKE_BUILD_TYPE=Debug ..


.PHONY : unittest
unittest : debug
	@echo "Running unit tests!!!"
	cd UnitTests/OPStest && \
	./pizzatest.sh

.PHONY : deploy
deploy : deploy_cpp

.PHONY : deploy_cpp
deploy_cpp :
	@echo "Target: $@"
	mkdir -p deploy/cpp/src
	mkdir -p deploy/cpp/src/xml
	cp -r Cpp/include deploy/cpp
	cp -r Cpp/source/* deploy/cpp/src
	-cp -r build.debug/Cpp/source/*.a deploy/cpp/lib
	-cp -r build.debug/Cpp/source/*.so* deploy/cpp/lib
	-cp -r build.opt/Cpp/source/*.a deploy/cpp/lib
	-cp -r build.opt/Cpp/source/*.so* deploy/cpp/lib
	echo "gcc version: $(CCV)" > deploy/cpp/README
	echo "g++ version: $(CXXV)" >> deploy/cpp/README
	@echo "Deployment made to $(CURDIR)/deploy"

.PHONY : clean_deploy
clean_deploy :
	rm -rf deploy

# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... opt (optimized/release)"
	@echo "... debug"
	@echo "... deploy"
	@echo "... unittest"
	@echo "... clean"
	@echo "... clean_debug"
	@echo "... clean_opt"
.PHONY : help
