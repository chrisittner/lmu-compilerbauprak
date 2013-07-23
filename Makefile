SRCS = $(shell ls *.hs) $(subst .x,.hs,$(shell ls *.x)) $(subst .y,.hs,$(shell ls *.y))
OBJS = $(subst .hs,.o,$(SRCS))
TARGET = mjc
.PRECIOUS : *.hs 

$(TARGET): $(SRCS)
	ghc --make -prof -auto-all -rtsopts -o $@ MJMain.hs
#	rm *.o MJLex.hs MJParse.hs

run-all:
	for i in $(ls ../MiniJava-Beispiele/*/*/*java) $(ls ../MiniJava-Beispiele/*/*java) ; do echo "$i"; ./$(TARGET) $i; done

%.hs:	%.y
	happy $<

%.hs:	%.x
	alex $<

%.o:	%.hs
	ghc -c -o $@ $<

clean:
	@rm *.o *.hi MJLex.hs MJParse.hs Backend/*.o Backend/*.hi $(TARGET)




# for i in $(ls bsp/Small/*.java); do echo "$i";javac $i; java -cp bsp/Small:bsp/Large `echo "$i"|awk -F"." '{ print $1 }'|awk -F"/" '{ print $3 }'` > tmp.out.java; ./mjc $i > tmp.s; gcc -m32 -o tmp.out tmp.s bsp/runtime.c; ./tmp.out > tmp.out.asm; if $(diff tmp.out.java tmp.out.asm &>/dev/null); then echo "yay";else echo "nope"; fi; done; rm tmp.s tmp.out tmp.out.java tmp.out.asm;
# for i in $(ls bsp/Small/*.java); do echo "$i";javac $i; java -cp bsp/Small:bsp/Large `echo "$i"|awk -F"." '{ print $1 }'|awk -F"/" '{ print $3 }'`;done
# for i in $(ls bsp/Small/*.java); do echo "$i"; ./mjc $i > tmp.s; gcc -m32 -o tmp.out tmp.s bsp/runtime.c; ./tmp.out; done
