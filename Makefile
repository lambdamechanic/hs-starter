.PHONY: lint lint-hlint lint-ormolu

lint: lint-ormolu lint-hlint

lint-ormolu:
	@echo "Checking formatting with ormolu..."
	@files="$$(git ls-files '*.hs')"; \
	if [ -n "$$files" ]; then \
		if command -v ormolu >/dev/null 2>&1; then \
			ormolu --mode check $$files; \
		elif command -v cabal >/dev/null 2>&1; then \
			if ! cabal run ormolu -- --mode check $$files; then \
				echo "Unable to run ormolu. Install it with 'cabal install ormolu' or add it to your project." >&2; \
				exit 1; \
			fi; \
		else \
			echo "Neither ormolu nor cabal were found in PATH; cannot check formatting." >&2; \
			exit 1; \
		fi; \
	else \
		echo "No Haskell sources found; skipping ormolu."; \
	fi

lint-hlint:
	@echo "Running hlint..."
	@files="$$(git ls-files '*.hs')"; \
	if [ -n "$$files" ]; then \
		hlint --config .hlint.yaml $$files; \
	else \
		echo "No Haskell sources found; skipping hlint."; \
	fi
