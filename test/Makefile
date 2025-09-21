# [make test] runs several tests in parallel.
# These tests do not terminate; they run forever.

SUBDIRS := \
  WordBitSet \
  DWordBitSet \
  QWordBitSet \
  SparseBitSet \

.PHONY: test
test:
	@ make clean
	@ for d in $(SUBDIRS) ; do make -C $$d all ; done
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: $(SUBDIRS)

.PHONY: clean
clean:
	@ git clean -fdX
