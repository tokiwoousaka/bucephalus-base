all: check_haste check_ghc

check_haste: bucephalus-base_haste.cabal.tmp app/Haste_Check.hs 
	@echo "##################################"
	@echo "##### make check_haste start #####"
	@echo "##################################"
	cp bucephalus-base_haste.cabal.tmp bucephalus-base.cabal
	haste-cabal build
	haste-cabal install

check_ghc: bucephalus-base_ghc.cabal.tmp app/GHC_Check.hs 
	@echo "################################"
	@echo "##### make check_ghc start #####"
	@echo "################################"
	cp bucephalus-base_ghc.cabal.tmp bucephalus-base.cabal
	stack build

clean:
	cd app; rm -f *.hi; rm -f *.jsmod; rm -f *.o
	cd app/Control/Monad; rm -f *.hi; rm -f *.jsmod; rm -f *.o
