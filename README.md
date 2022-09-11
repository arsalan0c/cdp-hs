# cdp-hs


### Quick start

1. Clone the repo with submodules `git clone --recurse-submodules https://github.com/arsalan0c/cdp-hs.git`
2. Run Chromium with debugging port enabled: `chromium --headless --remote-debugging-port=9222`
3. Switch directories to the CDP library: `cd cdp`
4. Drop into nix environment: `nix-shell --pure`
5. Generate cabal file: `hpack`
6. Run the example program: `cabal run cdp-exe`

### Generating the CDP library

1. Build the generator library: `nix-build --attr exe`
2. Run the generator: `result/bin/gen-exe`
3. 

### References

- https://jaspervdj.be/posts/2013-09-01-controlling-chromium-in-haskell.html