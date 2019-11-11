HC=ghc
HFLAGS= -Wall

ODIR = obj
IDIR = include

main:
	$(HC) --make -odir $(ODIR) -hidir $(IDIR) main

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(IDIR)/*.hi *~ main *~