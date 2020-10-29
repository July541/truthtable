# Truth table of logical connectives

`truthtable` is a tool for printing [truth table](https://en.wikipedia.org/wiki/Truth_table) of combining logical connectives including (And, Or, Implication, Not).

## Usage
```haskell
λ> import TruthTable
λ> truthTable (TAnd (TVar A) (TVar B))
```
![and](./images/and.png)

```haskell
λ> import TruthTable
λ> truthTable (TNot (TVar A))
```
![not](./images/not.png)

```haskell
λ> import TruthTable
λ> truthTable (TAnd (TOr (TVar A) (TVar B)) (TOr (TVar A) (TVar C)))
```
![combination](./images/combination.png)

## TODO
- [ ] Add tests
- [ ] Add multi color support
- [ ] Add parser