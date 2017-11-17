# Signatures

This is the reference implementation for CFG Based Function Identification

# build nucleus
apply nucleus.patch to [nucleus](https://bitbucket.org/vusec/nucleus) from commit: 9ef73e8.
then build nucleus

# build signatures
stack build

# usage

run nucleus to generate signatures from some binary
./nucleus -e binary -s sigs -d linear
./nucleus -e binary2 -s sigs2 -d linear

- match signatures:
signatures-exe -l loose -m 5 -s sigs -t sigs2
