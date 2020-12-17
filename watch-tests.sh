#!/bin/bash
rerun --dir "src" --dir "test" --pattern "**/*.{hs}" --exit --clear --name "learn4haskell Tests" -- make test-chapter4