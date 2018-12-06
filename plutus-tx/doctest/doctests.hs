import           Test.DocTest
main = doctest [
    "--fast",
    "-XTemplateHaskell",
    "-XScopedTypeVariables",
    "src/Language/PlutusTx/Prelude.hs"]
