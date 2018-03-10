module Interpreter where
import Grammar

-- TODO: Just ya know, write this thing?
runInterpreter :: Show a => Prog -> [a]
runInterpreter ast = []


-- Our Rules:
-- * A variable that appears under the scope of an existential quantifier is 
--   said to be bound, otherwise it is free
-- * LHS of turnstile must have all free variables 
-- * Free variables must all be in the scope of at least one relation

-- Musings:
-- * Probably want either CEK or CESK interpreter, probably don't need store 
--   in CESK but who knows...
-- * Looks like breaking down into normal form a good idea?
-- * Use HashMap package for the environment?


-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
