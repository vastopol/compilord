all:
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	g++ -o compiler y.tab.c lex.yy.c -lfl
clean:
	rm compiler lex.yy.c y.output y.tab.* *.min.txt
