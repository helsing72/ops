BUILD_DEBUG=build.debug
BUILD_OPT=build.opt

.PHONY : all
all: debug opt

.PHONY : clean
clean: clean_debug clean_opt

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
	export CC=$(shell which gcc) && \
	export CXX=$(shell which g++) && \
	mkdir -p $* && \
	cd $* && \
	cmake -DCMAKE_BUILD_TYPE=Release ..

.PHONY : debug
debug: $(BUILD_DEBUG)/Makefile
	$(MAKE) -C $(BUILD_DEBUG) --no-print-directory

$(BUILD_DEBUG)/Makefile : %/Makefile :
	export CC=$(shell which gcc) && \
	export CXX=$(shell which g++) && \
	mkdir -p $* && \
	cd $* && \
	cmake -DCMAKE_BUILD_TYPE=Debug ..


.PHONY : UnitTests
UnitTests : debug
	@echo "Running Unit tests!!!"
	cd UnitTests/OPStest && \
	pizzatest.sh
	


# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... opt (optimized/release)"
	@echo "... debug"
	@echo "... UnitTests"
	@echo "... clean"
	@echo "... clean_debug"
	@echo "... clean_opt"
.PHONY : help
