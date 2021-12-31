default:
	stack build --system-ghc

run:
	export PORT=8080; ./chesskell || stack run
