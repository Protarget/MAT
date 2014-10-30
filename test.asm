[macro doubler [x] [lambda [] [+ x x]]]

lda [apply [doubler 10]]