[![Scrutinizer Coverage](https://img.shields.io/scrutinizer/coverage/g/filp/whoops.svg?style=plastic)]()

External Sorting
================

A __non-idiomatic__ Scala implementation of External Sorting algorithm.
The idea of this library is to provide an API for external sorting, not the end-to-end solution as it will have to make some assumptions on how you will process the data you have (sequential or parallel, where to save it, how much memory you have and can use) which is a separate problem.


Please take a look at [tests](./src/test/scala) (last one) for a usage example.