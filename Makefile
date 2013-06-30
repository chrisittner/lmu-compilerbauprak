SRCS = $(shell ls *.hs) $(subst .x,.hs,$(shell ls *.x)) $(subst .y,.hs,$(shell ls *.y))
OBJS = $(subst .hs,.o,$(SRCS))
TARGET = mjc
.PRECIOUS : *.hs 

$(TARGET): $(SRCS)
	ghc --make -prof -auto-all -rtsopts -o $@ MJMain.hs
#	rm *.o MJLex.hs MJParse.hs

run-all:
	for i in $(ls ../MiniJava-Beispiele/*/*/*java) $(ls ../MiniJava-Beispiele/*/*java) ; do echo "$i"; ./$(TARGET) < $i; done

%.hs:	%.y
	happy $<

%.hs:	%.x
	alex $<

%.o:	%.hs
	ghc -c -o $@ $<

clean:
	@rm *.o *.hi MJLex.hs MJParse.hs Backend/*.o Backend/*.hi $(TARGET)
