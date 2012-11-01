# Makefile for Peter's Particle Tracker (PPT)

# Directories
VPATH = src
BASEDIR = $(shell pwd)
OBJDIR = $(BASEDIR)/obj
SRCDIR = $(BASEDIR)/src

# Compiler
FC = gfortran
FFLAGS = #-ffpe-trap=invalid -g -O0

# Files
OBJECTS = globals.f03 particle.f03 pusher.f03 fields.f03 ppt.f03
OBJLIST = $(addprefix $(OBJDIR)/,$(OBJECTS:.f03=.o))

# Executable linking
all: ppt

ppt: $(OBJLIST)
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -f $(OBJDIR)/*.o
	rm -f $(OBJDIR)/*.mod
	rm -f $(SRCDIR)/*.mod

# Compiling rules
$(OBJDIR)/%.o: %.f03 
	$(FC) $(FFLAGS) -c -o $@ $<
