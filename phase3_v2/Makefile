all:
	bison -v -d --file-prefix=y mini_l2.y
	flex mini_l2.lex
	g++ -std=c++11 -g -o compiler y.tab.c lex.yy.c -lfl -Wno-write-strings
clean:
	rm compiler lex.yy.c y.output y.tab.* *.min.txt

# had to add the -Wno-write-strings to get rid of error
# warning: ISO C++ forbids converting a string constant to ‘char*’ [-Wwrite-strings]