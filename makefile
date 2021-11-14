Dyxlexio: 	
	alex Lexer.x && happy Parser.y && ghc -Wall -Wcompat -Widentities -Wincomplete-uni-patterns \
		-Wincomplete-record-updates -Wredundant-constraints -Wmissing-export-lists \
		-Wpartial-fields Main.hs

clean:
	@echo "Cleaning.."
	rm Lexer.hs Parser.hs *.hi *.o Main
