import           Test.DocTest
main = doctest ["-pgmL markdown-unlit", "-isrc", "-itutorial", "--fast", "tutorial/Tutorial.lhs"]
