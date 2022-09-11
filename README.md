# cdp-hs


### Running the example program

1. Run Chromium with debugging port enabled: `chromium --headless --remote-debugging-port=9222`
2. Drop into nix environment: `nix-shell`
3. Run the program in the nix shell: `cabal run cdp-hs-exe`

### References

- https://jaspervdj.be/posts/2013-09-01-controlling-chromium-in-haskell.html