Dyxlexio: 	
	alex Lexer.x && ghc -Wall -Wcompat -Widentities -Wincomplete-uni-patterns \
		-Wincomplete-record-updates -Wredundant-constraints -Wmissing-export-lists \
		-Wpartial-fields Main.hs

clean:
	@echo "Cleaning.."
	rm Lexer.hs *.hi *.o Main
