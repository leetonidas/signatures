# Signatures

This repository contains supporting material for the paper 

# Material

 - The paper itself
 - The slide deck as presented at ROOTS 2017
 - The source code

# Build nucleus
apply nucleus.patch to [nucleus](https://bitbucket.org/vusec/nucleus) from commit: 9ef73e8.
then build nucleus

# Build signatures
stack build

# Usage

run nucleus to generate signatures from some binary
./nucleus -e binary -s sigs -d linear
./nucleus -e binary2 -s sigs2 -d linear

- match signatures:
signatures-exe -l loose -m 5 -s sigs -t sigs2
