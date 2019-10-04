SHELL=/bin/sh

# sources, headers, objects, library and etc.
SRCS_F  = ncpus.F lapack_gen.F splat.F fftpack.F
SRCS_i  = $(patsubst %.F, %.i, $(SRCS_F))
SRCS_f  = $(wildcard *.f)
SRCS    = $(SRCS_F) $(SRCS_f)
OBJS    = $(patsubst %.F, %.o, $(SRCS_F)) \
          $(patsubst %.f, %.o, $(SRCS_f))

all: build

build: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

debug: dbg_build

dbg_build: override FFLAGS += $(DEBUG)
dbg_build: $(LIB)

%.o: %.f
	$(OMPFC) -c $(FFLAGS) $<

%.o: %.F
	$(OMPFC) -c $(FPPCPP) $(FFLAGS) $(FFLAGSDEFS) $<

message: $(MSGSRC)
	$(CC) -c $(MSGSRC)
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.c, %.o, $(MSGSRC))
	$(RM) $(MSGSRC) $(patsubst %.c, %.o, $(MSGSRC))

install:
	[[ -z "$(LIB)" || -z "$(LIB_DIR)" ]] || mv $(LIB) $(LIB_DIR)
	[[ -z "$(SRC_DIR)" ]] || cp ./* $(SRC_DIR)

test:
	@echo "*** unit test unavailable currently ***"

clean:
	$(RM) $(SRCS_i) $(OBJS) $(LIB)

.SUFFIXES: .F .f .i .o

# Specify targets that do not generate filesystem objects
.PHONY: all build install debug dbg_build message test clean

