# signatures

This repository contains supporting material for the paper `Enhancing Control Flow Graph Based Binary Function Identification`.

## Material

 - The paper itself
 - The slide deck as presented at ROOTS 2017
 - The source code

## Summary

The aim of this project is to provide a Function identification tool with a reduced number of false positives.
It's main novelity is the use of transformations on the CFG before attempting the matching.

It can print out simple statistics about the binary executable and match functions from one to another.
It has to be used in conjuction with a modified version of [Nucleus](https://bitbucket.org/vusec/nucleus) that generates all the neccesariy static information about one executable. This modified version of Nucleus may be realeased seperatily at a later point in time.