# lda

An experimental implementaion of Latent Dirichlet Allocation (LDA).

## Usage

"lein run" with leiningen.

The structure of the input csv file: document-id, word, number-of-words-in-the-document

The output: vector of [[document-id, word, number-of-words-in-the-document], collection-of-probabilities-belonging-to-the-topics]

The codes on lda.memoized are almost identical to those of lda.core. lda.memoized uses clojure.core.memoized and runs a bit faster.

## License

Copyright © 2014 Takahiro SAWADA

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
