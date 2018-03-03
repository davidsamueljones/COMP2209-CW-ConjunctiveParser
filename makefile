
all: Main

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Main : Main.hs Tokens.hs Grammar.hs 
	ghci Main.hs -e main

clean:
	rm -f Tokens.hs
	rm -f Grammar.hs