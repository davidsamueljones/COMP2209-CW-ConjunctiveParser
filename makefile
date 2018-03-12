
all: Main

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Main : Main.hs Tokens.hs Grammar.hs 
	ghc -o myinterpreter Main.hs
	./myinterpreter

clean:
	rm -f *.hi *.o
	rm -f Tokens.hs
	rm -f Grammar.hs