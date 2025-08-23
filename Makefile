NETWORK_FLAGS=-package-db ../network-3.2.8.0/dist-newstyle/packagedb/ghc-9.4.7 -package network

Main:
	ghc $(NETWORK_FLAGS) $^