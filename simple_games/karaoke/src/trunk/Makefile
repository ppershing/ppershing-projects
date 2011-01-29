.PHONY: all
SRC=src
OBJ=obj
all:
	cd $(SRC); make

.PHONY: clean
clean:
	rm -f $(OBJ)/*.o *.exe

.PHONY: log_clean
log_clean:
	rm -f Karaoke.log Karaoke.leaks Karaoke.exe.stackdump \
	profiler.out gmon.out

.PHONY: doc
doc:
	../../cppdoc2/cppdoc \
	-title="Sing F(or)Ever" \
	-classdir=projects \
	-module=cppdoc-standard \
	-extensions=h \
	-show-timestamp \
	-include-private \
	-enable-author \
	-enable-version \
	-languages="c=cpp,cc=cpp,cpp=cpp,cs=csharp,cxx=cpp,h=cpp,hpp=cpp,hxx=cp" \
	-overwrite \
	-v \
	include/ \
	#"include/.svn" \
	projects/docs.html
	echo "Output was sent to /tmp/karaoke/docs.html"  

.PHONY: run
run: Karaoke.exe
	 ./Karaoke.exe; gprof Karaoke.exe \
	> profiler.out; ./errors.sh; ./leaks.sh; cat Karaoke.log
