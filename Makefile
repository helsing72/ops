BUILD_DEBUG=build.debug
BUILD_OPT=build.opt

CC=$(shell which gcc)
CXX=$(shell which g++)
INSTALL_PREFIX=$(CURDIR)/deploy

CCV=$(shell $(CC) -dumpversion)
CXXV=$(shell $(CXX) -dumpversion)

.PHONY : all
all: debug opt
	$(MAKE) install

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
	cmake -DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) -DCMAKE_BUILD_TYPE=Release ..

.PHONY : debug
debug: $(BUILD_DEBUG)/Makefile
	$(MAKE) -C $(BUILD_DEBUG) --no-print-directory

$(BUILD_DEBUG)/Makefile : %/Makefile :
	export CC=$(CC) && \
	export CXX=$(CXX) && \
	mkdir -p $* && \
	cd $* && \
	cmake -DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) -DCMAKE_BUILD_TYPE=Debug ..

.PHONY : unittest-c++
unittest-c++ : debug
	@echo "Running C++ unit tests!!!"
	cd UnitTests/OPStest-C++ && \
	./pizzatest.sh
	
	
.PHONY : unittest-python
unittest-python :
	@echo "Running C++ unit tests!!!"
	cd UnitTests/OPStest-Python && \
	./pizzatest.sh

.PHONY : install
install :
	$(MAKE) install_debug
	$(MAKE) install_opt
	$(MAKE) $(INSTALL_PREFIX)/lib/README
	@echo "Installed ops4 in $(INSTALL_PREFIX)"

.PHONY : install_debug
install_debug : debug
	$(MAKE) -C $(BUILD_DEBUG) install

.PHONY : install_opt
install_opt : opt
	$(MAKE) -C $(BUILD_OPT) install

$(INSTALL_PREFIX)/lib/README :
	mkdir -p $(shell dirname $@)
	echo "gcc version: $(CCV)" > $@
	echo "g++ version: $(CXXV)" >> $@

.PHONY : clean_deploy

clean_deploy :
	rm -rf deploy

# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... opt (optimized/release)"
	@echo "... debug"
	@echo "... unittest-c++"
	@echo "... unittest-python"
	@echo "... install"
	@echo "... unittest"
	@echo "... clean"
	@echo "... clean_debug"
	@echo "... clean_opt"
.PHONY : help
